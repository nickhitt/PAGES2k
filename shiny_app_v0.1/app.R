library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ML Analysis on PAGES2k Temperature Reconstructions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            checkboxGroupInput("recons_to_include",
                               "Reconstructions to Include",
                               choices = c("Antarctic Temperature",           
                                           "Arctic Temperature",
                                           "Asia Temperature",                
                                           "Australasia Temperature",
                                           "Europe Temperature" ,
                                           "North America Pollen Temperature", 
                                           "North America Trees Temperature",
                                           "South America Temperature"),
                               selected = c("Antarctic Temperature",           
                                            "Arctic Temperature",
                                            "Asia Temperature",                
                                            "Australasia Temperature",
                                            "Europe Temperature" ,
                                            "North America Pollen Temperature", 
                                            "North America Trees Temperature",
                                            "South America Temperature")),
            selectInput("plot_type",
                        "Figure to Show:",
                        choices = c("Regression", 
                                    "Calibration Timeseries", 
                                    "2000yr Hindcast"), 
                        selected = c("Regression")),
            sliderInput("calib_interval",
                        "Calibration Interval (CE):",
                        min = 1880,
                        max =2021,
                        value = c(1900, 2020)),
            selectInput("model", 
                        "Machine Learning Method:", 
                        choices = c("Linear Regression", 
                        "Random Forest", 
                        "Gradient Boosted Trees",
                        "Support Vector Machine"), 
                        selected = c("Linear Regression")),
            sliderInput("train_perc",
                        "Training Data %",
                        min = 0,
                        max = 1,
                        value = 0.7),   
            sliderInput("num_folds",
                        "Number of Folds",
                        min = 3,
                        max = 8,
                        value = 5), 
            checkboxGroupInput("pre_processing", 
                               "Pre-Processing Steps:", 
                               choices = c("center", "scale", "pca"), 
                               selected = c("center", "scale", "pca")),
            selectInput("impute", 
                        "Imputation Technique", 
                        choices = c("knnImpute", 
                                    "bagImpute", 
                                    "medianImpute"), 
                        selected = "knnImpute",
                        multiple = FALSE),
        ),

        # Show a plot of the generated distribution
        mainPanel(plotlyOutput("figure"),
                  tableOutput("summaryTable")
        ),
    )
)

server <- function(input, output) {
    
    records_to_include <- reactive({input$records_to_include})
    calib_interval <- reactive({input$calib_interval})
    model <- reactive({input$model})
    train_perc <- reactive({input$train_perc})
    num_folds <- reactive({input$num_folds})
    pre_processing <- reactive({input$pre_processing})
    impute <- reactive({input$impute})
    plot_type <- reactive({input$plot_type})
    
    # Loading Data and Wrangling
    setwd("~/Dropbox/R Codes/PAGES2k")
    source("~/Dropbox/R Codes/PAGES2k/0 Load Packages.R")
    all_temps <- readRDS("~/Dropbox/R Codes/PAGES2k/all_temps.rds")
    
    dataframe <- all_temps
    
    # Selecting Records to be included
    record_select <- function(dataframe, records_to_include){
        names <- c("Antarctic Temperature",           
                   "Arctic Temperature",
                   "Asia Temperature",                
                   "Australasia Temperature",
                   "Europe Temperature" ,
                   "North America Pollen Temperature", 
                   "North America.Trees Temperature",
                   "South America Temperature")
        for (i in length(names)) {
            if (names[i] %notin% records_to_include ) {
                dataframe <- select(dataframe, -names[i])
            } 
        }
        return(dataframe)
    }

    # Interval Selection Function
    data_select <- function(dataframe, calib_interval){
        # First slice data where we have global temps
        dataframe <-dataframe %>%
            dplyr::filter(!is.na(global_temp)) %>%
        # Then select time interval
            dplyr::filter(between(Time, calib_interval[1],calib_interval[2]))
        return(dataframe)
    }
    
    # Cross Validation Function
    cv <- function(dataframe, train_perc, num_folds){
        #Set Random Seed
        set.seed(42)
        
        #Split data into testing and training set based on user input
        to_train_training <- sample(nrow(dataframe), 
                                    round(train_perc*nrow(dataframe)),
                                    replace = FALSE)
        to_train_test <- dataframe[-to_train_training,]
        to_train_training <- dataframe[to_train_training,]
    
        #Create folds based on user input
        folds <- createFolds(to_train_training$global_temp, 
                             k = as.numeric(num_folds))
    
        myControl <- trainControl(verboseIter = TRUE, 
                                  savePredictions = TRUE, 
                                  index = folds)
    
        cv_list <- list("train" = to_train_training, 
                        "test" = to_train_test,
                        "cv_obj" = myControl)
        
        # Return the object
        return(cv_list)
    }
    
    # Fitting Models
    model_fit <- function(data_select, pre_processing, impute, model){
        
        ### Fitting Model Based on User Input
        
        # Selecting data
        x <- data_select[["train"]] %>%
            select(-global_temp, 
                    -"North America Pollen Temperature", 
                    -"North America Trees Temperature")
        
        y <- data_select[["train"]]$global_temp
        
        #Set pre-processing conditions based on user input
        preProcess <- c(impute, pre_processing)
        
        # Set Model based on user input
        if (model == "Linear Regression"){
            method <- "lm"
        } else if (model == "Random Forest") {
            method <- "ranger"
        } else if (model == "Gradient Boosted Trees") {
            method <- "xgbLinear"
        } else if (model == "Support Vector Machine") {
            method <- "svmLinear"
        }
        
        #Train Model
        finalModel <- train(
            y = y,
            x = select(x,-Time),
            method = method,
            #tuneGrid = grid,
            trControl = data_select[["cv_obj"]],
            preProcess = preProcess)
        
        # Return output
        fitted <- list("finalModel" = finalModel,
                       "x" = x,
                       "y" = y)
        
        return(fitted)
    }
    
    # Model Diagnostics Summary Statistics Table
    
    model_stats <- function(data_select){
        
        # Extract RMSE, R squared, and MAE Values
        stats <- data_select[["resample"]] %>%
            tidyr::pivot_longer(c(RMSE, Rsquared, MAE), 
                         names_to = c("Metric"),
                         values_to = c("Value")) %>%
            dplyr::group_by(Metric) %>%
            dplyr::summarise(Mean = mean(Value, na.rm = TRUE),
                             Median = median(Value, na.rm = TRUE),
                             Sigma = sd(Value, na.rm = TRUE))
        return(stats)
    }
    
    # Model Plot on Training Data
    
    model_training_plot <- function(model, x, y, model_type){
        
        # Predicted outcomes 
        predicted <- data.frame("time" = x[["Time"]],
                                "GISTemp" = y,
                                model_type = predict(model, select(x,-Time)))
    
        #Reorganise data for plotting
        predicted_longer <- predicted %>%
            tidyr::pivot_longer(c(model_type, "GISTemp"),
                         names_to = c("Model"), 
                         values_to = c("Temperature")) %>%
            dplyr::group_by(Model)
        
        # Plot timeseries
        timeseries_plot <- predicted_longer %>%
            ggplot(aes(time,Temperature, color = Model)) +
            geom_point() +
            geom_line() +
            theme(panel.background = element_rect(#fill = "white", 
                colour = "black", size = 3),
                legend.box.background = element_rect(fill = NA), 
                legend.key = element_rect(colour = "transparent", 
                                          fill = "white"),
                legend.box.margin = margin(1, 1, 1, 1), 
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.title = element_text(size = 8), 
                legend.text = element_text(size = 8)) +
            xlab("Year (CE)") +
            ylab(paste0("Temperature", " (", expression("\u00B0C"), ")"))
        
        # Plot regression
        regression_plot <- predicted %>%
            ggplot(aes(model_type,GISTemp)) +
            geom_point() +
            geom_smooth(method = "lm", col = "red") +
            theme(panel.background = element_rect(#fill = "white", 
                                                  colour = "black", size = 3),
                  legend.box.background = element_rect(fill = NA), 
                  legend.key = element_rect(colour = "transparent", 
                                            fill = "white"),
                  legend.box.margin = margin(1, 1, 1, 1), 
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  legend.title = element_text(size = 8), 
                  legend.text = element_text(size = 8)) +
            xlab(paste0("Model Temperature", " (", expression("\u00B0C"), ")")) +
            ylab(paste0("GIS Temperature", " (", expression("\u00B0C"), ")"))
        
        
        timeseries_plot <- ggplotly(timeseries_plot)
        regression_plot <- ggplotly(regression_plot)
        
        plots <- list("timeseries" = timeseries_plot,
                      "regression" = regression_plot)
        
        return(plots)
        
    }
    
    ## Hindcast last 2000 years using model and compare
    
    hindcast_plot <- function(model, dataframe, y){
        
        x <- select(dataframe,
                    -Time,
                    -global_temp, 
                    -"North America Pollen Temperature", 
                    -"North America Trees Temperature")
        
        predicted <- dataframe %>%
                     mutate(model_type = predict(model, x)) %>%
            rename(GISTemp = global_temp) %>%
            select(Time, GISTemp, model_type)
        
        #Reorganise data for plotting
        predicted_longer <- predicted %>%
            tidyr::pivot_longer(c(model_type, GISTemp),
                                names_to = c("Model"), 
                                values_to = c("Temperature")) %>%
            dplyr::group_by(Model)
        
        # Plotting
        timeseries_plot <- predicted_longer %>%
            ggplot(aes(Time,Temperature, color = Model)) +
            geom_point() +
            geom_line() +
            theme(panel.background = element_rect(fill = "white", 
                colour = "black", size = 1),
                legend.box.background = element_rect(fill = NA), 
                legend.key = element_rect(colour = "transparent", 
                                          fill = "white"),
                legend.box.margin = margin(1, 1, 1, 1), 
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                legend.title = element_text(size = 8), 
                legend.text = element_text(size = 8)) +
            xlab("Year (CE)") +
            ylab(paste0("Temperature", " (", expression("\u00B0C"), ")"))
        
        return(timeseries_plot)
        
    }
    
    # Put the function calls for interactive regression plot here
    output$figure <- renderPlotly({
        dataframe <- record_select(dataframe, records_to_include())
        selected_data <- data_select(dataframe, calib_interval())
        cv_data <- cv(selected_data, train_perc(), num_folds())
        fitted_data <- model_fit(cv_data, pre_processing(), impute(), model())
        plots <- model_training_plot(fitted_data[["finalModel"]],
                                     fitted_data[["x"]],
                                     fitted_data[["y"]],
                                     model())
        timeseries <- hindcast_plot(fitted_data[["finalModel"]],
                                    dataframe,
                                    fitted_data[["y"]])
        
        if (plot_type() == "Regression") {
            return(plots$regression)  
        } else if (plot_type() == "Calibration Timeseries") {
            return(plots$timeseries)
        } else if (plot_type() == "2000yr Hindcast") {
            return(timeseries)
        }
        
    })
    
    # Put the function calls for summary table here
    output$summaryTable <- renderTable({
        dataframe <- record_select(dataframe, records_to_include())
        selected_data <- data_select(dataframe, calib_interval())
        cv_data <- cv(selected_data, train_perc(), num_folds())
        fitted_data <- model_fit(cv_data, pre_processing(), impute(), model())
        sum_table <- model_stats(fitted_data[["finalModel"]])
        return(sum_table)
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
