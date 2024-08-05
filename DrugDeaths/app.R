#DrugDeaths App
#A web application for examining drug-related deaths
#Mark Reuter
#August 2024
#contact mark.reuter@googlemail.com
#git
#original data set from https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsrelatedtodrugpoisoningbyselectedsubstances

#Number of drug-related poisonings by selected substances, England and Wales, deaths registered between 1993 and 2022",,,,,,

#### library ####
library(shiny)
library(shinythemes)
library(wesanderson)

#### functions ####
plotByRegion <- function(region,data,year,substance){
  xdata <- data[data$Usual.residence.name == region,]
  male.death <- xdata[xdata$Sex == 'Males',]
  female.death <- xdata[xdata$Sex == "Females",]
  malex <- male.death[male.death$Substance == substance,]
  femalex <- female.death[female.death$Substance == substance,]
  
  matrix <- as.matrix(data.frame(Males=malex$Deaths,
                                 Females=femalex$Deaths),
  )
  
  row.names(matrix) <- c('On the death certificate',
                         'Without other drugs',
                         'With alcohol',
                         'Without alcohol')
  
  plot <- barplot(t(matrix),
                  col=colors()[c(11,23)],
                  beside = TRUE,
                  ylab = "Deaths",
                  main = paste0('Number of deaths due to ',substance,', \n ',region,', deaths registered in ',year))
  zlegend <- legend("topright",
                    legend = c('male','female'),
                    fill = colours()[c(11,23)])
  return(plot)
}

#### Data ####

allData <- read.csv("data/2022substances.xlsx - All data.csv",
                    header = TRUE)
print(names(allData))

allYears <- unique(allData$Year.of.death.registration)
sexes <- unique(allData$Sex)
residence <- unique(allData$Usual.residence.name)
substance <- unique(allData$Substance)
mentioned <- unique(allData$Mentioned)

print(allYears)
print(sexes)
print(residence)
print(substance)
print(mentioned)

#### UI ####
ui <- fluidPage(

    titlePanel("Drug Deaths"),

    sidebarLayout(
        sidebarPanel(
          selectInput(
            "year",
            "Year",
            allYears,
            selected = "2022",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          selectInput(
            "sex",
            "Sex",
            sexes,
            selected = "Persons",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          selectInput(
            "residence",
            "Residence",
            residence,
            selected = "England and Wales",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          selectInput(
            "substance",
            "Substance",
            substance,
            selected = "01 All drug poisonings",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          actionButton("update",
                       "Show/Update",
                       width = 150),
          
          hr(),
          HTML("<h3>Comparison with...</h3>"),
          
          selectInput(
            "year2",
            "Year",
            allYears,
            selected = "2022",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          selectInput(
            "sex2",
            "Sex",
            sexes,
            selected = "Persons",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          selectInput(
            "residence2",
            "Residence",
            residence,
            selected = "England and Wales",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          selectInput(
            "substance2",
            "Substance",
            substance,
            selected = "01 All drug poisonings",
            multiple = FALSE,
            selectize = TRUE,
            width = NULL,
            size = NULL
          ),
          
          actionButton("update2",
                       "Show/Update",
                       width = 150)
          
        ), #sidbar end
#### main panel ####
        mainPanel(
        tabsetPanel(
          tabPanel("Comparison plots",
                     HTML("<h4>Number of drug-related poisonings by selected substances, England and Wales, deaths registered between 1993 and 2022</h4>"),
                     dataTableOutput("filtered"),
                     plotOutput("plot"),
                     hr(),
                     plotOutput("plot2"),
                     plotOutput("plot3")
          ),
          tabPanel("Totals Deaths by Region",
                   plotOutput('englandAndWales'),
                   plotOutput("england"),
                   plotOutput("wales")),
          tabPanel("Time Series",
                   HTML("<h3>to do</3h3>"))
        )
        ) #main panel end
    ) #sidebar layout end
) #app end

#### server ####
server <- function(input, output) {

    
    observeEvent(input$update,{
      filteredData <- allData
      filteredData <- filteredData[filteredData$Year.of.death.registration==input$year,]
      filteredData <- filteredData[filteredData$Sex==input$sex,]
      filteredData <- filteredData[filteredData$Usual.residence.name==input$residence,]
      filteredData <- filteredData[filteredData$Substance==input$substance,]
      
      output$filtered <- renderDT(
        filteredData
      )
      
      output$plot <- renderPlot({
        barplot(filteredData$Deaths,
                names.arg = filteredData$Mentioned,
                ylab = "Deaths",
                col=wes_palette(4,
                                type = "continuous"),
                main = paste0("Number of drug-related poisonings by selected substances, \n",input$residence,", deaths registered in ",
                              input$year)
                )
      })
      
    })
  
  ##comparison
  observeEvent(input$update2,{
    
    filteredData <- allData
    filteredData <- filteredData[filteredData$Year.of.death.registration==input$year,]
    filteredData <- filteredData[filteredData$Sex==input$sex,]
    filteredData <- filteredData[filteredData$Usual.residence.name==input$residence,]
    filteredData <- filteredData[filteredData$Substance==input$substance,]
    
    filteredData2 <- allData
    filteredData2 <- filteredData2[filteredData2$Year.of.death.registration==input$year2,]
    filteredData2 <- filteredData2[filteredData2$Sex==input$sex2,]
    filteredData2 <- filteredData2[filteredData2$Usual.residence.name==input$residence2,]
    filteredData2 <- filteredData2[filteredData2$Substance==input$substance2,]
    
    output$plot2 <- renderPlot({
      barplot(filteredData2$Deaths,
              names.arg = filteredData2$Mentioned,
              ylab = "Deaths",
              col=wes_palette(4,
                              type = "continuous"),
              main = paste0("Number of drug-related poisonings by selected substances, \n",input$residence2,", deaths registered in ",
                            input$year2)
      )
    })
    
    comp.matrix <- t(as.matrix(data.frame(A=filteredData$Deaths,
                                        B=filteredData2$Deaths)))
    
    #rownames(comp.matrix) <- c("On the death certificate", "Without other drugs","With alcohol","Without alcohol")
    #rownames(comp.matrix) <- c("On the death certificate", "Without other drugs","With alcohol","Without alcohol")
    colnames(comp.matrix) <- c("On the death certificate", "Without other drugs","With alcohol","Without alcohol")
    print(comp.matrix)
    output$plot3 <- renderPlot({
      barplot(comp.matrix,
              #names.arg = row.names(comp.matrix),
              col=colors()[c(23,77)], 
              #col = c("red", "blue"),
              beside = TRUE,
              ylab = "Deaths")
      legend("topright",                                    
             legend = c(
               paste0(
                input$year," | ",
                input$sex," | ",
                input$residence," | ",
                input$substance),
               paste0(
                 input$year2," | ",
                 input$sex2," | ",
                 input$residence2," | ",
                 input$substance2
               )),
             fill = colours()[c(23,77)])
    })
    
  })
    
  ##individual plots
  #england and wales
  listen1 <- reactive({
    list(input$year,input$substance)
  })
  
  observeEvent(listen1(),{
    print(listen1())
    
      output$englandAndWales <- renderPlot({
        xdata <- allData[allData$Year.of.death.registration == input$year,]
        plot <- plotByRegion('England and Wales',xdata,input$year,input$substance)
        plot
      })
      #England
      output$england <- renderPlot({
        xdata <- allData[allData$Year.of.death.registration == input$year,]
        plot <- plotByRegion('England',xdata,input$year,input$substance)
        plot
      })
      #wales
      output$wales <- renderPlot({
        xdata <- allData[allData$Year.of.death.registration == input$year,]
        plot <- plotByRegion('Wales',xdata,input$year,input$substance)
        plot
      })
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
