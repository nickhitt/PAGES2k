#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("EDA on Pages2k Temperature Construction Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # checkboxGroupInput("recons_to_include", 
            #                    "Reconstructions to Include", 
            #                    choices = c(), 
            #                    selected = ),
            sliderInput("calib_interval",
                        "Calibration Interval (CE):",
                        min = 1,
                        max = 50,
                        value = c(1970, 2020)),
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
                        max = 5,
                        value = 0.8),   
            sliderInput("num_folds",
                        "Number of Folds",
                        min = 0,
                        max = 5,
                        value = 1), 
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
        mainPanel(
            tabsetPanel(
                tabPanel("Regression Plot", plotOutput("regressPlot")),
                tabPanel("Diagostics Table", tableOutput("table")),
                #tabPanel("Timeseries Comparison", verbatimTextOutput("summary")),
                ),
        )
    )
)

server <- function(input, output) {
    
    source("~/Dropbox/R Codes/PAGES2k/0 Load Packages.R")
    
    # Data Selection Function
    data_select <- function(dataframe){
        # First slice data where we have global temps
        dataframe <-dataframe %>%
            filter(!is.na(global_temp)) %>%
        # Then select time interval
            filter(time > calib_interval[1]() & time < calib_interval[2]())
        return(dataframe)
    }
    
    # Cross Validation Function
    cv <- function(dataframe){
        to_train_training <- sample(nrow(dataframe), 
                                    round(train_perc()*nrow(dataframe)),
                                    replace = FALSE)
        to_train_test <- to_train[-to_train_training,]
        to_train_training <- to_train[to_train_training,]
        
        set.seed(42)
    
        folds <- createFolds(to_train_training$global_temp, 
                             k =num_folds())
    
        myControl <- trainControl(verboseIter = TRUE, 
                                  savePredictions = TRUE, 
                                  index = folds)
    
        cv_list <- list("train" = to_train_training, 
                        "test" = to_train_test,
                        "cv_obj" = myControl)
    
        return(cv_list)
    }
    
    # Fitting Models
    model_fit <- function(data_select){
        
        ### Fitting Models
        
        x <- data_select[["train"]] %>%
            select(-global_temp, 
                   -North.America.Pollen.Temperature, 
                   -North.America.Trees.Temperature)
        
        y <- data_select[["train"]]$global_temp
        
        preProcess <- c(impute(), pre_processing())
        
        method <- if (model() == "Linear Regression"){
            return("lm")
        } else if (model() == "Random Forest") {
            return("ranger")
        } else if (model() == "Gradient Boosted Trees") {
            return("xgbLinear")
        } else if (model() == "Support Vector Machine") {
            return("svmLinear")
        }
        
        model <- train(
            y = y,
            x = x,
            method = method,
            #tuneGrid = grid,
            trControl = data_select[["cv_obj"]],
            preProcess = preProcess)
        
        return(model, x, y)
    }
    
    # Model Diagnostics Summary Statistics Table
    
    model_stats <- function(data_select){
        stats <- data_select[["resample"]] %>%
            dplyr::pivot_longer(c(RMSE, Rsquared, MAE), 
                         names_to = c("Metric"),
                         values_to = c("Value")) %>%
            dplyr::group_by(Resample, Metric) %>%
            dplyr::summarise(mean = mean(Value),
                             median = median(Value),
                             sd = sd(Value))
    }
    
    # Model Plot on Training Data
    
    model_training_plot <- function(model, x, y){
        
        predicted <- predict(model, x)
        
        time <- seq(calib_interval[2](),
                    calib_interval[2]()-(nrow(predicted)-1), 
                    -1)
        
        predicted$time <- time
        
        predicted <- c(predicted, y)
        
        colnames(predicted) <- c("time", 
                                 model(),
                                 "GISTemp")
        
        predicted_longer <- predicted %>%
            pivot_longer(c(model(), "GISTemp"),
                         names_to = c("Model"), 
                         values_to = c("Temperature"))
        
        timeseries_plot <- predicted_longer %>%
            ggplot(aes(time,Temperature, color = Model)) +
            geom_point() +
            geom_line() 
        
        regression_plot <- predicted %>%
            ggplot(aes(model(),GISTemp)) +
            geom_point() +
            geom_smooth(method = "lm", col = "red")
        
        timeseries_plot <- ggplotly(timeseries_plot)
        regression_plot <- ggplotly(regression_plot)
        
        plots <- list("timeseries" = timeseries_plot,
                      "regression" = regression_plot)
        
        return(plots)
        
    }
    
    # Put the function calls for interactive regression plot here
    output$regressPlot <- renderPlotly({
        selected_data <- data_select()
        cv_data <- cv(selected_data)
        fitted_data <- model_fit(cv_data)
        plots <- model_training_plot(fitted_data$model,
                                     fitted_data$x,
                                     fitted_data$y)
        
        return(plots$regression)
        
    })
    
    # Put the function calls for summary table here
    output$distPlot <- renderPlot({
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
