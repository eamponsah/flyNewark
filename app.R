# Shiny App by Group 1
library(markdown)
library(shiny)
library(ggplot2)
library(dplyr)
library(corrgram)
library(ellipse)
library(caret)
library(glmnet)
make_type = c('factor', 'factor', 'factor', 'factor', 'factor', 'factor', 'factor', 
              'numeric', 'factor', 'numeric', 'numeric')
flightData_p = read.csv('Newarkflights2021.csv', colClasses = make_type)
flightData = read.csv('Newarkflights2021.csv')

# Define UI
ui = navbarPage('EWR Dataset EDA',
           tabPanel('Data Glimpse',
                    sidebarLayout(
                      sidebarPanel(
                        h3('Purpose of the Shiny App'),
                        p('This Shiny App is created for users to analyze the datasets of Newark Airport,
        which contains flights departing from Newark Airport in New Jersey from June to 
        September in 2021. There were 30,226 flights across 12 airlines that operated 
        during this period. The Shiny App displays plots of variables in the dataset
        and provides the function to predict the probability of flight being delayed or 
        on time in the future.'),
                        br(),
                        br(),
                        br(),
                        img(src = 'EWR_logo.png', height = 80, width = 240),
                        br(),
                        h4('by Group 1'),
                        width = 4
                      ),
                      mainPanel(
                        h2('Glimpse of the Dataset'),
                        fluidRow(
                          column(4,selectInput('month_1', 'Month:', 
                                               c('All', unique(as.character(flightData_p$month))))),
                          column(4,selectInput('day_1', 'Day of Week:',
                                               c('All', unique(as.character(flightData_p$day))))),
                          column(4, selectInput('carrier_1', 'Carrier:',
                                                c('All', unique(as.character(flightData_p$carrier)))))
                        ),
                        helpText(strong('Carrier:'), 'AA: American, DL: Delta, F9: Frontier, NK: Spirit,  
                           OO: skyWest, UA: United, YX: Republic, B6: JetBlue, 9E: Endeavor, 
                           AS: Alaska, G4: Allegiant, MQ: Envoy'),
                        DT::dataTableOutput('table')))),
           tabPanel('Data Description',
                    fluidRow(
                      column(8, selectInput('sum_str', 'Choose Summary or Structure of the Dataset:', 
                                            c('Summary' = 'summary', 'Structure' = 'str')))),
                    mainPanel(
                      h4(textOutput('caption')),
                      verbatimTextOutput("summaryStr"),
                      h3('Description of the Dataset'),
                      p('There are 11 variables in the dataset. 8 of them are categorical, 3 of them are numerical.'),
                      h4('Categorical Variables:'),
                      p('1) delay: flight delayed, 1= delayed, 0=not delayed'),
                      p('2) month: month of flight (June, July, August, September)'),
                      p('3) day: day of flight, 1=Monday, 7=Sunday'),
                      p('4) carrier: 12 airlines: 9E, AA, AS, B6, DL, F9, G4, MQ, NK, OO, UA, YX'),
                      p('AA: American, DL: Delta, F9: Frontier, NK: Spirit,  
                           OO: skyWest, UA: United, YX: Republic, B6: JetBlue, 9E: Endeavor, 
                           AS: Alaska, G4: Allegiant, MQ: Envoy'),
                      p('5) origin: airport origin (EWR)'),
                      p('6) orstate: state of flight origin (NJ)'),
                      p('7) dest: airport destination (73)'),
                      p('8) deststate: state of flight destination (36)'),
                      h4('Numerical Variables:'),
                      p('9) duration: scheduled flight duration in minutes (29 ~ 631)'),
                      p('10) distance: scheduled flight distance in miles (160 ~4962)'),
                      p('11) depart: time of scheduled flight departure in the form of number of minutes starting at 12:01
            a.m. For example: 300 = 5:00 a.m., since 300/60 = 5 hours starting at 12:01 a.m.')
                      
                    )),
           tabPanel('Response Var and Cor',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('variable_3', 'Make a choice to represent correlation',
                                    choices = c('ovals', 'numbers'))),
                      mainPanel(
                        fluidRow(
                          splitLayout(cellWidths = c('50%', '50%'), plotOutput('barPlot_3'), plotOutput('corPlot'))
                        )
                      )
                    )),
           tabPanel('Categorical Var',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput('variable_4', 'Choose a variable',
                                    choices = c('month', 'day', 'carrier', 'dest', 'deststate')),
                        br(),
                        p('Note: The left plot is the barpolt for selected variable. The right plot is the 
      boxplot for delay vs. selected variable.'),
                        width = 3),
                      mainPanel(
                        fluidRow(
                          splitLayout(cellWidths = c('50%', '50%'), plotOutput('barPlot_4'), plotOutput('mosaicPlot'))
                        )
                      )
                    )),
           tabPanel('Numerical Var',
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(inputId = 'variable_5', 
                                    label = 'Choose a variable',
                                    choices = c('depart', 'duration', 'distance')),
                        checkboxInput('outliers', 'Show outliers', TRUE),
                        sliderInput(inputId = 'bins',
                                    label = 'Number of bins:',
                                    min = 1,
                                    max = 100,
                                    value = 15),
                        width = 3
                      ),
                      mainPanel(
                        fluidRow(
                          splitLayout(cellWidths = c('50%', '50%'), plotOutput('distPlot'), plotOutput('flightPlot'))
                        )
                      )
                    )),
           tabPanel('Prediction',
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons('interest', 'Choose the Flight Status of Interest:',
                                     c('Delay', 'On Time')),
                        selectInput('day', 'Choose the Day of Week:', list(1, 2, 3, 4, 5, 6, 7)),
                        selectInput('month', 'Choose the Month:', list(6, 7, 8, 9)),
                        selectInput('carrier', 'Choose the Carrier:', c('9E', 'AA', 'AS', 'B6', 'DL', 'F9', 'G4', 'MQ', 'NK', 'OO', 'UA', 'YX')),
                        numericInput('duration', 'Enter the Flight Duration (in minutes):', 100),
                        numericInput('depart', 'Enter the Departure Time:', 100),
                        helpText('Note: time of scheduled flight departure in the form of number of minutes starting at 12:01
a.m. For example: 300 = 5:00 a.m..'),
                        ),
                      mainPanel(
                        img(src = 'welcome.png', height = 300, width = 600),
                        br(),
                        h3('The Probability of the Flight Status of Interest is:'),
                        br(),
                        textOutput('pred')
                      )
                    )))

# Define server function
server = function(input, output, session){
  output$table = DT::renderDataTable(DT::datatable({
    data = flightData_p
    if(input$month_1 != 'All'){
      data = data[data$month == input$month_1,]
    }
    if(input$day_1 != 'All'){
      data = data[data$day == input$day_1,]
    }
    if(input$carrier_1 != 'All'){
      data = data[data$carrier == input$carrier_1,]
    }
    data
  })
  )
  
  formulaText = reactive({
    input$sum_str
  })
  output$caption = renderText({
    formulaText()
  })
  output$summaryStr = renderPrint({
    x = input$sum_str
    if(x == 'summary'){
      dataset = flightData_p
      summary(dataset)
    }else{
      dataset = flightData_p
      str(dataset)}
  })
  output$barPlot_3 = renderPlot({
    plot_1 = barplot(table(flightData$delay),
                     names.arg = c("On Time", "Delayed"),
                     main= 'Barplot of Flight Status', col=c('pink', 'lightblue'),
                     xlab = 'Flight On Time Status', ylab="Number of Flights", 
                     ylim = c(0, 25000))
    text(plot_1, table(flightData$delay) + 1000, labels = table(flightData$delay))})
  output$corPlot = renderPlot({
    x = c('delay', 'depart', 'duration', 'distance')
    corData = flightData[, x]
    corMat = cor(corData, use = 'pairwise.complete.obs')
    if(input$variable_3 == 'ovals'){
      colVar = colorRamp(c("#CC0000","white","#3366CC"),space="Lab")
      plotcorr(corMat, col = rgb(colVar((corMat + 1)/2), maxColorValue = 255), main = 'Using Ovals to Show Correlation')}
    else{
      corrgram(corMat, type = 'cor', upper.panel = panel.conf, lower.panel = panel.conf, main = 'Using Numbers to Show Correlation')}
  })
 
   output$barPlot_4 = renderPlot({
    x = input$variable_4
    if(x == 'dest'){
      tmp = flightData %>% count(dest, sort = TRUE)
      tmp1 = tmp[1:10, ]
      values = tmp1$n
      names = tmp1$dest
      barplot(values, names.arg = names, col = 'salmon',
              main = 'Barplot of Top 10 Destination Airports',
              xlab = x,
              ylab = 'Number of Flights')}else if(x == 'deststate'){
                tmp = flightData %>% count(deststate, sort = TRUE)
                tmp1 = tmp[1:10, ]
                values = tmp1$n
                names = tmp1$deststate
                barplot(values, names.arg = names, col = 'olivedrab1',
                        main = 'Barplot of Top 10 Destination States',
                        xlab = x,
                        ylab = 'Number of Flights')
              }else{
                barplot(table(flightData[,x]), las = 1,  
                        main = paste('Barplot of ', x, sep = ''),
                        xlab = x, col = 'lightgreen',
                        ylab = 'Number of Flights')}})
  output$mosaicPlot = renderPlot({
    x = input$variable_4
    if(x == 'carrier'){
      mosaicplot(flightData[, x] ~ flightData$delay, las = 2, cex.axis = 1,
                 main = paste('Delay by ', x, sep = ''), color = c('blue', 'red'),
                 xlab = x, ylab = 'Delay: 1 = Yes(red), 0 = No(blue)')
    }else if(x == 'dest'){
      tmp = flightData %>% count(dest, sort = TRUE)
      tmp1 = tmp[1:10, ]
      names = tmp1$dest
      tmp2 = filter(flightData, dest %in% names)
      mosaicplot(tmp2[, x] ~ tmp2$delay, las = 1, cex.axis = 1,
                 main = paste('Delay by ', x, sep = ''), color = c('blue', 'red'),
                 xlab = x, ylab = 'Delay: 1 = Yes(red), 0 = No(blue)')
    }else if(x == 'deststate'){
      tmp = flightData %>% count(deststate, sort = TRUE)
      tmp1 = tmp[1:10, ]
      names = tmp1$deststate
      tmp2 = filter(flightData, deststate %in% names)
      mosaicplot(tmp2[, x] ~ tmp2$delay, las = 1, cex.axis = 1,
                 main = paste('Delay by ', x, sep = ''), color = c('blue', 'red'),
                 xlab = x, ylab = 'Delay: 1 = Yes(red), 0 = No(blue)')
    }else{
      mosaicplot(flightData[, x] ~ flightData$delay, las = 1, cex.axis = 1,
                 main = paste('Delay by ', x, sep = ''), color = c('blue', 'red'),
                 xlab = x, ylab = 'Delay: 1 = Yes(red), 0 = No(blue)')}
  })
  
  output$distPlot = renderPlot({
    x = input$variable_5
    bins = seq(min(flightData[,x]), max(flightData[,x]), length.out = input$bins + 1)
    hist(flightData[,x], breaks = bins, col = 'violet', border = 'white',
         xlab = x,
         main = paste("Histogram of", x, sep=" "))
  })
  formulaText_5 = reactive({
    paste(input$variable_5, '~ delay')
  })
  output$caption_5 = renderText({
    formulaText_5()
  })
  output$flightPlot = renderPlot({
    boxplot(as.formula(formulaText_5()),
            data = flightData, outline = input$outliers, col = "#75AADB", pch = 19,
            main = paste(input$variable_5, '~ delay'))
  })
  
  delay_model = readRDS('delay_model.RDS')
  onTime_model = readRDS('onTime_model.RDS')
  input_df = reactive({
    interest_out = input$interest
    day = as.factor(c(input$day, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5))
    month = as.factor(c(input$month, 6, 7, 8, 9, 6, 7, 8, 9, 6, 7, 8, 9))
    carrier = as.factor(c(input$carrier, '9E', 'AA', 'AS', 'B6', 'DL', 'F9', 'G4', 'MQ', 'NK', 'OO', 'UA', 'YX'))
    depart = c(input$depart, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200)
    duration = c(input$duration, 100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200)
    df = data.frame(day, month, carrier, depart, duration)
    if(interest_out == 'Delay'){
      model.matrix( ~ (day + depart + duration + month)^2 + carrier + 
                      poly(depart, degree=3,raw=TRUE)[,2:3] + 
                      poly(duration,degree=3,raw=TRUE)[,2:3], data = df)[,-1]
    }else{
      model.matrix( ~ (day + depart + duration + month)^3 + carrier + 
                      poly(depart, degree=5,raw=TRUE)[,2:5] + 
                      poly(duration,degree=5,raw=TRUE)[,2:5], data = df)[,-1]
    }})
  pred = reactive({
    interest_out = input$interest
    if(interest_out == 'Delay'){
      model = delay_model
      best_lambda = 1.538643e-05 # It is the best lambda for model 2 found in assignment 5
      predict(model, s=best_lambda, input_df(), type = 'response')
    }else{model = onTime_model
    best_lambda = 1.563291e-05 # best lambda for model 3 found in assignment 5
    1 - predict(model, s=best_lambda, input_df(), type = 'response')} 
  })
  output$pred = renderText({pred()[1]})
}
shinyApp(ui, server)
