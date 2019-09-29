library(shiny)
library(shinyBS)
library(leaflet)
library(timevis)
library(shinythemes)
library(knitr)

## Shiny Interface and Layout 

shinyUI(navbarPage("Updated Systematic Review on CAFO Data",
                   tabPanel("Getting Started",
                            fluidRow(column(12, includeMarkdown("gettingstarted.Rmd")))
                            ),
                   tabPanel("Descriptive Plots",
                            fluidRow(column(12, uiOutput("descriptive"))),
                            splitLayout(cellWidths = c("40%", "40%"), leafletOutput("map", width = "85%"), plotOutput("bar", width = "95%", height = "430px")),
                            fluidRow(column(9, includeMarkdown("descriptive2.Rmd"))),
                            fluidRow(column(7, timevisOutput("time", height = "300px",width = "800px"))),
                            fluidRow(column(7, includeMarkdown("descriptive3.Rmd"))),
                            fluidRow(plotOutput("multiple", width = "60%"))
                  ),
                   tabPanel("Metafor Forest Plot",
                            fluidRow(column(4,
                                            selectInput("Body",
                                                        "Body",
                                                        choices = c("Nervous", "Upper Respiratory", "Lower Respiratory" ,"Other"),width = "30%")),
                                     column(4,
                                            selectInput("Outcome",
                                                        "Outcome",
                                                        choices = " ", selected = " ")),
                                     column(5, img(src="https://raw.githubusercontent.com/jesslk/Stat-585-Lab-1/master/label.jpg",  width="160%",
                                                   height="120%" ))),
                            plotOutput("metafor", width = "70%", height = "800px")
                            

                            ),
                   tabPanel("Original Forest Plot",
                            fluidPage(theme = shinytheme("cerulean"),
                                      tags$head(
                                        tags$style(HTML("
                                                        pre, table.table{
                                                        font-size: smaller;
                                                        }        
                                                        body{
                                                        min-height: 2000px;
                                                        min-width: 1800px;
                                                        }
                                                        .option-group{
                                                        border: 3px solid #ccc;
                                                        padding: 2px 2px;
                                                        margin: -250px -210px;
                                                        margin-bottom: -200px;
                                                        }
                                                        #    .well { 
                                                        #       padding-left: 15px;
                                                        #       padding-right: 15px; 
                                                        #       margin-bottom: 0px; 
                                                        #       width: auto; 
                                                        #     }
                                                        "))
                                        ),
                                      
                                      titlePanel("Concentrated Animal Feeding Operations(CAFOs) Data"),
                                      
                                      
                                      fluidRow(
                                        column(width = 2,
                                               wellPanel(
                                                 uiOutput("class")
                                               ) 
                                        ),
                                        
                                        column(width = 2,
                                               wellPanel(
                                                 uiOutput("expo_var_board")
                                               )  
                                        ),
                                        
                                        column(width = 3,
                                               wellPanel(
                                                 uiOutput("expo_var_naro")
                                               )
                                        ),
                                        
                                        column(width = 2,
                                               wellPanel(
                                                 uiOutput("effect_measure_method")
                                               )           
                                        )       
                                      ),
                                 
                                      fluidRow(
                                        column(6,
                                               wellPanel(     
                                                 helpText(p(style="text-align:justify","Note:"),
                                                          p(style = "text-align: justify", 
                                                            em("1. In the 'ROB class' column,
                                                               OE: Objective Exposure; SE: Subjective Exposure; 
                                                               OO: Objective Outcome; SO: Subjective Outcome.")),
                                                          p(style = "text-align: justify",
                                                            em("2. In the ROB plot, each half circle represents opinion for that 
                                                               particular type of ROB from one of two independent reviewers,
                                                               i.e. for each circle, left hand side half circle represents opinion 
                                                               from first reviewer and  right hand side half circle from second reviewer."))
                                                            )
                                                            )     
                                                            ),
                                        
                                        ## add side bar to control figure plot sise on the screen 
                                        fluidRow(
                                          column(4,
                                                 sliderInput(input = "height",
                                                             label = "Plot Height (px):",
                                                             min = 0, max = 2000, step = 10, value = 400)),
                                          column(4,
                                                 sliderInput(input = "width", 
                                                             label = "Plot Width (%):",
                                                             min = 0, max = 100, step = 1, value = 80))
                                        )
                                                            ),
                                      
                                      
                                      #   div(plotOutput("forestROBPlot"))
                                      uiOutput("forestROBPlot2")
                                      ## download plot 
                                      #    downloadButton('downloadForestROBPlot','Download the plot as a .pdf')
                                      
                                               )
                            
                            
                            
                            ),
                   tabPanel("Summary",
                            fluidRow(includeMarkdown("summary.Rmd")),
                            fluidRow(plotOutput("bias",width="800px",height="500px"))
),
                   tabPanel("References",
                            fluidPage(uiOutput("references")))))
