#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

if (!require(shiny)) {
  install.packages("shiny")
  install.packages("shinydashboard")
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}
require(shiny)
require(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
              menuItem("Set Parameters", tabName = "params"),
              menuItem("Target Trial Results", tabName = "results"),
              menuItem("Sensitivity Plot Outputs", tabName = "plots"),
              menuItem("Sensitivity Plot Data", tabName = "data")
  ),
  h5("Please cite results from this tool by citing:"),
  h5("Kennedy-Shaffer and Hughes, 2019 (under review)"),
  h5("For questions, contact Lee Kennedy-Shaffer at:"),
  h6("lee_kennedyshaffer@g.harvard.edu"),
  h5("For underlying code, visit:"),
  h6("https://github.com/leekshaffer")
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName="params",
            fluidRow(uiOutput("UIerrs")),
            fluidRow(
              column(width=3,
              box(
                title="Power and Level", status="primary", solidHeader=TRUE,
                width=12,
                sliderInput("power",
                            "Power (%)",
                            min = 0,
                            max = 100,
                            value = 90),
                sliderInput("sig",
                            "Significance Level (%)",
                            min = 0,
                            max = 100,
                            value = 5)
              ),
              uiOutput("UIFXa"),
              uiOutput("UIFXb")),
              
              column(width=6,
              box(title="Number of Strata", status="warning", solidHeader=TRUE,
                  width=12,
                  #numericInput("S","Number of Strata",value=2),
                  radioButtons("S",NULL,
                               choices=c(1,2,3), selected=c(1),
                               inline=TRUE),
                  helpText("For more strata, see the underlying R code. Caution should be exercised when some strata have few clusters, as the sample size formulae implemented here rely on asymptotics within each stratum. See the paper cited at left for more detail.")),
              uiOutput("UIstrat"),
              uiOutput("UIstrat2a"),
              uiOutput("UIstrat2b"),
              uiOutput("UIstrat3a"),
              uiOutput("UIstrat3b"),
              uiOutput("UIstrat3c")),
              
              column(width=3,
              box(title="IRT or CRT", status="info", solidHeader=TRUE,
                  width=12,
                  radioButtons("ICRT",NULL,
                               choices=c("IRT","CRT"), selected=c("IRT"),
                               inline=TRUE)),
              uiOutput("UIclust")
              )
              )
            ),
    
    tabItem(tabName="plots",
            uiOutput("UIplotsa"),
            uiOutput("UIplotsb")
            ),
    
    tabItem(tabName="results",
            fluidRow(uiOutput("uiTargErr")),
            
                    fluidRow(
                      uiOutput("NCRTs"),
                      uiOutput("NCRT"),
                      uiOutput("RCRT")
                    ),
                    
                    fluidRow(
                      uiOutput("BetaStar"),
                      uiOutput("Beta")
                    ),
 
                    uiOutput("UIRHOrow"),
                    
                    uiOutput("UIFrow")
            ),
    
    tabItem(tabName="data",
            uiOutput("UIdata")
            )
  )
)


shinyUI(dashboardPage(
  dashboardHeader(title="Sample Size Calculator for Stratified IRTs and CRTs with Binary Outcomes",
                  titleWidth=700),
  sidebar,
  body))
  