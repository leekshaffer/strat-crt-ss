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
    menuItem("Instructions for Use", tabName = "insts"),
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
    tabItem(tabName="insts",
            h3("Citation and Support:"),
            h5("Please cite results from this tool by citing Kennedy-Shaffer and Hughes, 2019 (under review)"),
            h5("For questions, contact Lee Kennedy-Shaffer at: lee_kennedyshaffer@g.harvard.edu"),
            h5("For underlying code, visit: https://github.com/leekshaffer"),
            h5("For more than 3 strata, see the underlying R code. Caution should be exercised when some strata have few clusters, as the sample size formulae implemented here rely on asymptotics within each stratum. See the cited article for more detail."),
            h3("Instructions for Use:"),
            h5("First, select the power and significance level desired for the test, the number of strata, and whether it is an IRT or CRT."),
            h5("Then, select whether the hypothesized effect size is on the overall or within-stratum scale and enter the effect size."),
            h5("Select whether to specify each stratum's probability of event or the overall probability and all but one strata's. Enter these probabilities, the target proportions in each stratum and the range of proportions in stratum 1 to test for sensitivity analyses."),
            h5("Finally, for CRTs, select whether to assume a common within-stratum ICC and cluster size distribution (and enter those parameters), or whether to specify the overall ICC and the ICCs and cluster size distributions for each stratum."),
            h5("An error message will appear on the Set Parameters page if impossible parameters are chosen. If the parameters chosen yield ICCs in other strata that are inadmissible, NAs will appear in the sample size boxes in the Target Trial Results page.")
    ),
    
    tabItem(tabName="params",
            fluidRow(uiOutput("UIerrs")),
            fluidRow(
              column(width=4,
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
              
              column(width=4,
              box(title="Number of Strata", status="warning", solidHeader=TRUE,
                  width=12,
                  radioButtons("S",NULL,
                               choices=c("1","2","3"), selected=c("1"),
                               inline=TRUE)
                  ),
              uiOutput("UIstrat"),
              uiOutput("UIstrat2Oa"),
              uiOutput("UIstrat2Sa"),
              uiOutput("UIstrat2Ob"),
              uiOutput("UIstrat2Sb"),
              uiOutput("UIstrat3Oa"),
              uiOutput("UIstrat3Sa"),
              uiOutput("UIstrat3Ob"),
              uiOutput("UIstrat3Sb"),
              uiOutput("UIstrat3Oc"),
              uiOutput("UIstrat3Sc")),
              
              column(width=4,
              box(title="IRT or CRT", status="info", solidHeader=TRUE,
                  width=12,
                  radioButtons("ICRT",NULL,
                               choices=c("IRT","CRT"), selected=c("IRT"),
                               inline=TRUE)),
              uiOutput("UIclusta"),
              uiOutput("UIclustb"),
              uiOutput("UIclustc")
              )
              )
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
            ),
    
    
    tabItem(tabName="plots",
            uiOutput("UIplotsa"),
            uiOutput("UIplotsb")
    )
  )
)


shinyUI(dashboardPage(
  dashboardHeader(title="Sample Size Calculator for Stratified IRTs and CRTs with Binary Outcomes",
                  titleWidth=700),
  sidebar,
  body))
  