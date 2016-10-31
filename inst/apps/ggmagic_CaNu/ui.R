library(shiny)
library(shinyjs)
library(rhandsontable)


shinyUI(fluidPage(
  
  titlePanel(""),
  fluidRow(
    useShinyjs()
  ),
  fluidRow(id="dataPage",
           column(3,
                  div(id = "dataControlsSection",
                      #verbatimTextOutput("debug"),
                      uiOutput("dataInputSection"),
                      uiOutput("dataInputControls"),
                      verbatimTextOutput("dataInputSectionDebug")
                  )
           ),
           column(9,
                  div(id="dataSection",
                      rHandsontableOutput("dataInputPreview"),
                      uiOutput("dataControls")
                  )
                  
           )),
  fluidRow(id="vizPage",
           column(3,
                  div(id = "vizControlsSection",
                      uiOutput("vizControls")
                  ),
                  verbatimTextOutput("debugViz")
           ),
           column(9,
                  uiOutput("viz")           )
  )
))
