library(shiny)
library(shinyjs)
library(rhandsontable)


shinyUI(fluidPage(
  
  titlePanel(""),
  fluidRow(
    useShinyjs()
  ),
  fluidRow(id="dataPage",
           column(6,
                  div(id = "dataControlsSection",
                      #verbatimTextOutput("debug"),
                      uiOutput("dataInputSection"),
                      uiOutput("dataInputControls"),
                      verbatimTextOutput("dataInputSectionDebug")
                  )
           ),
           column(6,
                  div(id="dataSection",
                      rHandsontableOutput("dataInputPreview"),
                      uiOutput("dataControls")
                  )
                  
           )),
  fluidRow(id="vizPage",
           column(3,
                  div(id = "vizControlsSection",
                      uiOutput("vizControls"),
                      uiOutput("vizControls2")
                  ),
                  verbatimTextOutput("debugViz")
           ),
           column(9,
                  uiOutput("viz")           )
  )
))
