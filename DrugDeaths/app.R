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
library(ggplot2)

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
  
  row.names(matrix) <- c('On the death \n certificate',
                         'Without \n other drugs',
                         'With \n alcohol',
                         'Without \n alcohol')
  
  plot <- barplot(t(matrix),
                  col=colors()[c(11,23)],
                  beside = TRUE,
                  ylab = "Deaths",
                  cex.lab = 1,
                  cex.axis = 1,
                  cex = 0.9,
                  las = 1,
                  main = paste0('Number of deaths due to ',substance,', \n ',region,', deaths registered in ',year))
  zlegend <- legend("topright",
                    legend = c('male','female'),
                    fill = colours()[c(11,23)])
  return(plot)
}

#main comparison plots using ggplot
comparePlots <- function(year,sex,residence,substance,data){
  
  p <- ggplot(
    data,
    aes(x=Mentioned,
        y=Deaths)) +
    ggtitle(paste0("Number of drug-related poisonings by ",substance, "\n",residence,", deaths registered in ",year)) +
    geom_bar(stat="identity", fill=colors()[c(57,38,27,11)]) +
    theme(axis.title = element_text(size=14)) +
    theme(axis.text = element_text(size=14))
  return(p)
}

plotTimeseries <- function(sex,region,substance,data){
  xdata <- data[data$Usual.residence.name == region,]
  xdata <- xdata[xdata$Sex == sex,]
  xdata <- xdata[xdata$Substance == substance,]
  
  plot <- ggplot(
    xdata[,c(1,7)],
    aes(x=xdata$Year.of.death.registration,
        y=xdata$Deaths,
        group=xdata$Mentioned,
        color=xdata$Mentioned)) +
    geom_line() +
    xlab("Year") + 
    ylab("Deaths") +
    labs(color = "Mentioned") +
    ggtitle(paste0('Number of deaths due to ',substance,', \n ',region,' - ',sex)) +
    theme(plot.title = element_text(size=18)) +
    theme(legend.title = element_text(size=16)) +
    theme(legend.text = element_text(size=14)) +
    theme(legend.position="top")
  
  
  # plot <- plot(
  #   deathCert$Year.of.death.registration,
  #   deathCert$Deaths,
  #   type = 'p',
  #   xlab = 'year',
  #   ylab = 'deaths',
  #   col=colors()[11],
  #   main = paste0('Number of deaths due to ',substance,', \n ',region,' - ',sex)
  # )
  # lines(withAlcohol$Year.of.death.registration,
  #       withAlcohol$Deaths,
  #       pch = 18,
  #       col = colors()[29],
  #       type = "p",
  #       lty = 2)
  # legend("topleft",
  #      legend = c('On the death certificate'),#,'With alcohol','Without alcohol','With other drugs'),
  #      #fill = colours()[c(11,29,31,33)])
  #      fill = colours()[11])
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
          
          # actionButton("update",
          #              "Show/Update",
          #              width = 150),
          
          hr()
          
        ), #sidbar end
#### main panel ####
        mainPanel(
        tabsetPanel(
          tabPanel("Comparison plots",
                   HTML("<h4>Number of drug-related poisonings by selected substances, England and Wales, deaths registered between 1993 and 2022</h4>"),
                   dataTableOutput("filtered"),
                   plotOutput("plot"),
                   hr(),
                   HTML("<h3>Comparison with...</h3>"),
                   fluidRow(
                     column(6,
                            uiOutput("year2"),
                            uiOutput("sex2")
                            ),
                     column(6,
                            uiOutput("residence2"),
                            uiOutput("substance2")
                            )
                   ),
                   actionButton("update2",
                                "Show/Update",
                                width = 150),
                   plotOutput("plot2"),
                   plotOutput("plot3")
          ),
          tabPanel("Totals Deaths by Region",
                   fluidRow(
                     column(6,
                            plotOutput('englandAndWales')
                            ),
                     column(6,
                            plotOutput('england')
                            )
                   ),
                   fluidRow(
                     column(6,
                            plotOutput('wales')
                            ),
                     column(6,
                            plotOutput('london'))
                   ),
                   fluidRow(
                     column(6,
                            plotOutput('northeast')
                     ),
                     column(6,
                            plotOutput('northwest'))
                   ),
                   fluidRow(
                     column(6,
                            plotOutput('yorkshire')
                     ),
                     column(6,
                            plotOutput('eastmidlands'))
                   )
          ),
          tabPanel("Time Series",
                   plotOutput('timePlot')),
          tabPanel("All Data",
                   dataTableOutput("allData"))
                   
        ) #tabset panel end
        ) #main panel end
    ) #sidebar layout end
) #app end

#### server ####
server <- function(input, output) {

  listen0 <- reactive({
    list(input$year,
         input$sex,
         input$residence,
         input$substance)
  })
  
  #### comparison outputs ####
  output$year2 <- renderUI({
    selectInput(
      "year2",
      "Year",
      allYears,
      selected = "2022",
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
  output$sex2 <- renderUI({
    selectInput(
      "sex2",
      "Sex",
      sexes,
      selected = "Persons",
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
  output$residence2 <- renderUI({
    selectInput(
      "residence2",
      "Residence",
      residence,
      selected = "England and Wales",
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
  output$substance2 <- renderUI({
    selectInput(
      "substance2",
      "Substance",
      substance,
      selected = "01 All drug poisonings",
      multiple = FALSE,
      selectize = TRUE,
      width = NULL,
      size = NULL
    )
  })
  
    
  #### comparison plots ####
    #observeEvent(input$update,{
    observeEvent(listen0(),{
      filteredData <- allData
      filteredData <- filteredData[filteredData$Year.of.death.registration==input$year,]
      filteredData <- filteredData[filteredData$Sex==input$sex,]
      filteredData <- filteredData[filteredData$Usual.residence.name==input$residence,]
      filteredData <- filteredData[filteredData$Substance==input$substance,]
      
      output$filtered <- renderDT(
        filteredData
      )
      
      output$plot <- renderPlot({
        year <- listen0()[[1]]
        sex <- listen0()[[2]]
        residence <- listen0()[[3]]
        substance <- listen0()[[4]]
        
        barplot <- comparePlots(year,sex,residence,substance,filteredData)
        barplot
        # barplot(filteredData$Deaths,
        #         names.arg = filteredData$Mentioned,
        #         ylab = "Deaths",
        #         col=wes_palette(4,
        #                         type = "continuous"),
        #         main = paste0("Number of drug-related poisonings by selected substances, \n",input$residence,", deaths registered in ",
        #                       input$year)
        #         )
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
      year <- input$year2
      sex <- input$sex2
      residence <- input$resicence2
      substance <- input$substance2
      
      barplot <- comparePlots(year,sex,residence,substance,filteredData2)
      barplot
      # barplot(filteredData2$Deaths,
      #         names.arg = filteredData2$Mentioned,
      #         ylab = "Deaths",
      #         col=wes_palette(4,
      #                         type = "continuous"),
      #         main = paste0("Number of drug-related poisonings by selected substances, \n",input$residence2,", deaths registered in ",
      #                       input$year2)
      # )
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
      #London
      output$london <- renderPlot({
        xdata <- allData[allData$Year.of.death.registration == input$year,]
        plot <- plotByRegion('London',xdata,input$year,input$substance)
        plot
      })
      #North East
      output$northeast <- renderPlot({
        xdata <- allData[allData$Year.of.death.registration == input$year,]
        plot <- plotByRegion('North East',xdata,input$year,input$substance)
        plot
      })
      #North West
      output$northwest <- renderPlot({
        xdata <- allData[allData$Year.of.death.registration == input$year,]
        plot <- plotByRegion('North West',xdata,input$year,input$substance)
        plot
      })
      #Yorkshire
      output$yorkshire <- renderPlot({
        xdata <- allData[allData$Year.of.death.registration == input$year,]
        plot <- plotByRegion('Yorkshire and The Humber',xdata,input$year,input$substance)
        plot
      })
      #East Midlands
      output$eastmidlands <- renderPlot({
        xdata <- allData[allData$Year.of.death.registration == input$year,]
        plot <- plotByRegion('East Midlands',xdata,input$year,input$substance)
        plot
      })
  })
  
  #### time series ####
  listen2 <- reactive({
    list(input$sex,input$residence,input$substance) 
  })
  
  observeEvent(listen2(),{
    print(listen2())
    
    output$timePlot <- renderPlot({
      timeplot <- plotTimeseries(input$sex,input$residence,input$substance,allData)
      timeplot
    })
    
  })
  
  output$allData <- renderDT({
    allData
  })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
