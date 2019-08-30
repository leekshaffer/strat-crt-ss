#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

source("StratCRTfns.R")

if (!require(shiny)) {
  install.packages("shiny")
  install.packages("shinydashboard")
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}
require(shiny)
require(shinydashboard)

shinyServer(function(input, output) {
  
  
  ##### HELPER FUNCTIONS #####
  
  ResultsFn <- function(f1) {
    if (input$S == 1) {
      if (input$ICRT == "IRT") {
        return(RCRT(pi0s=input$pi0,fs=1,
                    alpha=input$sig/100, gamma=1-input$power/100,
                    Fover=1,F.strats=1,
                    logORst=input$betast))
      } else {
        return(RCRT(pi0s=input$pi0,fs=1,
                    alpha=input$sig/100, gamma=1-input$power/100,
                    logORst=input$betast,
                    mbars=input$m,
                    rho0s=input$rho0,
                    CVs=input$cv))
      }
    } else if (input$S == 2) {
      f2 <- 1 - f1
      if (input$fix2=="O") {
        pi02 <- (input$pi0 - f1*input$pi01)/f2
      } else {
        pi02 <- input$pi02
      }
      if (input$ICRT == "IRT") {
        if (input$txFX == "S") {
          return(RCRT(pi0s=c(input$pi01,pi02),fs=c(f1,f2),
                      alpha=input$sig/100, gamma=1-input$power/100,
                      Fover=1,F.strats=1,
                      logORst=input$betast))
        } else {
          return(RCRT(pi0s=c(input$pi01,pi02),fs=c(f1,f2),
                      alpha=input$sig/100, gamma=1-input$power/100,
                      Fover=1,F.strats=1,
                      logOR=input$beta))
        }
      } else {
        if (input$txFX == "S") {
          return(RCRT(pi0s=c(input$pi01,pi02),fs=c(f1,f2),
                      alpha=input$sig/100, gamma=1-input$power/100,
                      logORst=input$betast,mbars=input$m,
                      rho0s=ICC.common(pi0s=c(input$pi01,pi02),fs=c(f1,f2),
                                       rho0=input$rho0)$rho0st,
                      CVs=input$cv))
        } else {
          return(RCRT(pi0s=c(input$pi01,pi02),fs=c(f1,f2),
                      alpha=input$sig/100, gamma=1-input$power/100,
                      logOR=input$beta,mbars=input$m,
                      rho0s=ICC.common(pi0s=c(input$pi01,pi02),fs=c(f1,f2),
                                       rho0=input$rho0)$rho0st,
                      CVs=input$cv))
        }
      }
    } else {
      f3 <- 1 - f1 - input$f2target
      if (input$fix3=="O") {
        pi03 <- (input$pi0 - f1*input$pi01 - input$f2target*input$pi02)/f3
      } else {
        pi03 <- input$pi03
      }
      if (input$ICRT == "IRT") {
        if (input$txFX == "S") {
          return(RCRT(pi0s=c(input$pi01,input$pi02,pi03),
                      fs=c(f1,input$f2target,f3),
                      alpha=input$sig/100, gamma=1-input$power/100,
                      Fover=1,F.strats=1,
                      logORst=input$betast))
        } else {
          return(RCRT(pi0s=c(input$pi01,input$pi02,pi03),
                      fs=c(f1,input$f2target,f3),
                      alpha=input$sig/100, gamma=1-input$power/100,
                      Fover=1,F.strats=1,
                      logOR=input$beta))
        }
      } else {
        if (input$txFX == "S") {
          return(RCRT(pi0s=c(input$pi01,input$pi02,pi03),
                      fs=c(f1,input$f2target,f3),
                      alpha=input$sig/100, gamma=1-input$power/100,
                      logORst=input$betast,
                      mbars=input$m,
                      rho0s=ICC.common(pi0s=c(input$pi01,input$pi02,pi03),
                                       fs=c(f1,input$f2target,f3),
                                       rho0=input$rho0)$rho0st,
                      CVs=input$cv))
        } else {
          return(RCRT(pi0s=c(input$pi01,input$pi02,pi03),
                      fs=c(f1,input$f2target,f3),
                      alpha=input$sig/100, gamma=1-input$power/100,
                      logOR=input$beta,
                      mbars=input$m,
                      rho0s=ICC.common(pi0s=c(input$pi01,input$pi02,pi03),
                                       fs=c(f1,input$f2target,f3),
                                       rho0=input$rho0)$rho0st,
                      CVs=input$cv))
        }
      }
    }
  }
  
  
  ResultsFnCapt <- function(f1) {
    capture.output(ResCapt <- tryCatch({ResultsFn(f1)},
                                       error=function(err) {"A"}), file="/dev/null")
    namelist <- c("NCRT","NCRTs","RCRT","RIRT","pi0s","pi1s","rho0s","F.strats","OR","logOR","ORst","logORst")
    Default <- as.list(rep(NA,length(namelist)))
    names(Default) <- namelist
    if (sum(ResCapt=="A")==length(ResCapt)) {
      return(Default)
    } else {
      return(ResCapt)
    }
  }
  
  RangeResultsFn <- function(range) {
    if (input$S > 1) {
    RangeRes <- t(sapply(X=range,
                       FUN=function(z) as.data.frame(ResultsFnCapt(z))[1,]))
    RangeRes2 <- subset(RangeRes, select=-c(RIRT,OR,ORst,pi0s,pi1s))
    if (input$ICRT == "IRT") {
      RangeRes2 <- subset(RangeRes2, select=-c(rho0s,F.strats))
      namelist <- colnames(RangeRes2)
      namelist[1:3] <- c("NIRT","NIRTs","RIRT")
      colnames(RangeRes2) <- namelist
    }
    return(RangeRes2)
    } else {
      return(NA)
    }
  }
  
  

  ######## DYNAMIC SENSITIVITY DATA TABLE UI ###########
  
    
  output$UIdata <- renderUI({
    if (input$S==1) {
      box(title="No Sensitivity Plots Available for 1-Stratum Calculations",
          width=12)
    } else {
      range2 <- round(input$range, digits=2)
      range2[1] <- ifelse(range2[1]==0, .01, range2[1])
      range2[2] <- ifelse(range2[2]==1, .99, range2[2])
      
      f1vals <- seq(range2[1],range2[2],by=.01) #set of f1 values to test
      num <- length(f1vals)
      ResDF <- RangeResultsFn(f1vals)
      
      if (input$S==2) {
        fsDF <- data.frame(f1=f1vals, f2=rep(1,num)-f1vals)
      } else {
        fsDF <- data.frame(f1=f1vals, f2=rep(input$f2target, num),
                           f3=rep(1,num)-f1vals-rep(input$f2target, num))
      }
      
      if (input$S==2) {
        if (input$fix2=="S") {
          pi02s <- rep(input$pi02,num)
        } else {
          pi02s <- (input$pi0 - f1vals*input$pi01)/fsDF$f2
        }
        pisDF <- data.frame(pi01=rep(input$pi01,num),pi02=pi02s)
      } else {
        if (input$fix3=="S") {
          pi03s <- rep(input$pi03,num)
        } else {
          pi03s <- (input$pi0 - f1vals*input$pi01 - input$f2target*input$pi02)/fsDF$f3
        }
        pisDF <- data.frame(pi01=rep(input$pi01,num),pi02=rep(input$pi02,num),pi03=pi03s)
      }
      
      ResDF2 <- cbind(fsDF,pisDF,ResDF)
      namesI2 <- c("f\U2081","f\U2082","\U03C0\U2080\U2081","\U03C0\U2080\U2082",
                   "N_IRT","N_IRT(S)","R_IRT","\U03B2","\U03B2\U002A")
      namesC2 <- c("f\U2081","f\U2082","\U03C0\U2080\U2081","\U03C0\U2080\U2082",
                   "N_CRT","N_CRT(S)","R_CRT","\U03C1\U2080\U002A","F\U002A",
                   "\U03B2","\U03B2\U002A")
      namesI3 <- c("f\U2081","f\U2082","f\U2083",
                   "\U03C0\U2080\U2081","\U03C0\U2080\U2082","\U03C0\U2080\U2083",
                   "N_IRT","N_IRT(S)","R_IRT","\U03B2","\U03B2\U002A")
      namesC3 <- c("f\U2081","f\U2082","f\U2083",
                   "\U03C0\U2080\U2081","\U03C0\U2080\U2082","\U03C0\U2080\U2083",
                   "N_CRT","N_CRT(S)","R_CRT","\U03C1\U2080\U002A","F\U002A",
                   "\U03B2","\U03B2\U002A")
      if (input$ICRT=="IRT") {
        if (input$S == 2) {
          names(ResDF2) <- namesI2
        } else if (input$S == 3) {
          names(ResDF2) <- namesI3
        }
      } else if (input$ICRT=="CRT") {
        if (input$S == 2) {
          names(ResDF2) <- namesC2
        } else if (input$S == 3) {
          names(ResDF2) <- namesC3
        }
      }
      box(title="Data to Generate Sensitivity Plots",
          width=12,
          renderTable(ResDF2))
    }
  })
  
  
  ######## DYNAMIC SENSITIVITY PLOTS UI ###########
  
  
  output$UIplotsa <- renderUI({
    if (input$S==1) {
      box(title="No Sensitivity Plots Available for 1-Stratum Calculations",
          width=12)
    } else {
      range2 <- round(input$range, digits=2)
      range2[1] <- ifelse(range2[1]==0, .01, range2[1])
      range2[2] <- ifelse(range2[2]==1, .99, range2[2])
      
      f1vals <- seq(range2[1],range2[2],by=.01) #set of f1 values to test
      
      RangeResDF <- RangeResultsFn(f1vals)
      
      if (input$ICRT=="IRT") {
        NIRTsvals <- unlist(RangeResDF[,"NIRTs"])
        
        output$distplot <- renderPlot({plot(x=f1vals, y=NIRTsvals, type="l", col=4, lwd=3,
                         xlim=c(range2[1],range2[2]),
                         ylim=c(min(NIRTsvals, na.rm=TRUE),max(NIRTsvals, na.rm=TRUE)),
                         xlab=expression("f"["1"]),
                         ylab=expression("N"["IRT(S)"]))})
        
        fluidRow(
          box(
            title="Plot of N_IRT(S) (Sample Size Required for Stratified IRT) vs. f\U2081 (Proportion of Participants in Stratum 1)",
            width=12,
            plotOutput("distplot")
          )
        )
      } else {
        NCRTsvals <- unlist(RangeResDF[,"NCRTs"])
        
        output$distplot <- renderPlot({plot(x=f1vals, y=NCRTsvals, type="l", col=4, lwd=3,
                         xlim=c(range2[1],range2[2]),
                         ylim=c(min(NCRTsvals, na.rm=TRUE),max(NCRTsvals, na.rm=TRUE)),
                         xlab=expression("f"["1"]),
                         ylab=expression("N"["CRT(S)"]))})
        
        fluidRow(
          box(
            title="Plot of N_CRT(S) (Sample Size Required for Stratified CRT) vs. f\U2081 (Proportion of Participants in Stratum 1)",
            width=12,
            plotOutput("distplot")
          )
        )
      }
    }
  })
  
  output$UIplotsb <- renderUI({
    if (input$S==1) {
      NULL
    } else {
      range2 <- round(input$range, digits=2)
      range2[1] <- ifelse(range2[1]==0, .01, range2[1])
      range2[2] <- ifelse(range2[2]==1, .99, range2[2])
      
      f1vals <- seq(range2[1],range2[2],by=.01) #set of f1 values to test
      
      RangeResDF <- RangeResultsFn(f1vals)
      
      if (input$ICRT=="IRT") {
        RIRTvals <- unlist(RangeResDF[,"RIRT"])
        
        output$distplot2 <- renderPlot({plot(x=f1vals, y=RIRTvals, type="l", col=3, lwd=3,
                         xlim=c(range2[1],range2[2]),
                         ylim=c(min(RIRTvals, na.rm=TRUE),1),
                         xlab=expression("f"["1"]),
                         ylab=expression("R"["IRT"]))})
        
        fluidRow(
          box(
            title="Plot of R_IRT (Ratio of Sample Sizes Required for Stratified IRT vs. Unstratified IRT) vs. f\U2081 (Proportion of Participants in Stratum 1)",
            width=12,
            plotOutput("distplot2")
          )
        )
      } else {
        RCRTvals <- unlist(RangeResDF[,"RCRT"])
        
        output$distplot2 <- renderPlot({plot(x=f1vals, y=RCRTvals, type="l", col=3, lwd=3,
                         xlim=c(range2[1],range2[2]),
                         ylim=c(min(RCRTvals, na.rm=TRUE),1),
                         xlab=expression("f"["1"]),
                         ylab=expression("R"["CRT"]))})
        
        fluidRow(
          box(
            title="Plot of R_CRT (Ratio of Sample Sizes Required for Stratified CRT vs. Unstratified CRT) vs. f\U2081 (Proportion of Participants in Stratum 1)",
            width=12,
            plotOutput("distplot2")
          )
        )
      }
    }
  })
  
  
  
  
  ###### DYNAMIC BOX RESULTS UI #########

  
  output$NCRTs <- renderUI({
    if (input$S==1) {
      ResDF <- ResultsFnCapt(1)
    } else {
      ResDF <- ResultsFnCapt(input$f1target)
    }
    
    # infoBox(
    #   ifelse(input$ICRT=="IRT","N_IRT(S)","N_CRT(S)"), 
    #   paste0(format(ceiling(ResDF$NCRTs), big.mark=",")),
    #   color="purple"
    # )
    box(title=ifelse(input$ICRT=="IRT", "Sample Size Required for Stratified IRT, N_IRT(S)", "Sample Size Required for Stratified CRT, N_CRT(S)"), status="primary", solidHeader=TRUE, width=4,
        h4(paste0(format(ceiling(ResDF$NCRTs), big.mark=","))))
  })
  
  output$NCRT <- renderUI({
    if (input$S==1) {
      ResDF <- ResultsFnCapt(1)
    } else {
      ResDF <- ResultsFnCapt(input$f1target)
    }
    
    # infoBox(
    #   ifelse(input$ICRT=="IRT","N_IRT","N_CRT"), 
    #   paste0(format(ceiling(ResDF$NCRT), big.mark=",")),
    #   color="orange"
    # )
    box(title=ifelse(input$ICRT=="IRT", "Sample Size Required for Unstratified IRT, N_IRT", "Sample Size Required for Unstratified CRT, N_CRT"), status="warning", solidHeader=TRUE, width=4,
        h4(paste0(format(ceiling(ResDF$NCRT), big.mark=","))))
  })
  
  output$RCRT <- renderUI({
    if (input$S==1) {
      ResDF <- ResultsFnCapt(1)
    } else {
      ResDF <- ResultsFnCapt(input$f1target)
    }
    
    # infoBox(
    #   ifelse(input$ICRT=="IRT","R_IRT","R_CRT"), 
    #   paste0(format(round(ResDF$RCRT, digits=3), nsmall=3)),
    #   color="olive"
    # )
    box(title=ifelse(input$ICRT=="IRT", "Ratio of Required Sample Sizes, R_IRT", "Ratio of Required Sample Sizes, R_CRT"), status="success", solidHeader=TRUE, width=4,
        h4(paste0(format(round(ResDF$RCRT, digits=3), nsmall=3))))
  })
  
  output$Beta <- renderUI({
    if (input$S==1) {
      ResDF <- ResultsFnCapt(1)
    } else {
      ResDF <- ResultsFnCapt(input$f1target)
    }
    
    # infoBox(
    #   "\U03B2", 
    #   paste0(format(round(ResDF$logOR, digits=3), nsmall=3)),
    #   color="orange"
    # )
    box(title="Overall Treatment Effect, \U03B2", status="warning", solidHeader=TRUE, width=4,
        h4(paste0(format(round(ResDF$logOR, digits=3), nsmall=3))))
  })
  
  output$BetaStar <- renderUI({
    if (input$S==1) {
      ResDF <- ResultsFnCapt(1)
    } else {
      ResDF <- ResultsFnCapt(input$f1target)
    }
    
    # infoBox(
    #   "\U03B2\U002A", 
    #   paste0(format(round(ResDF$logORst, digits=3), nsmall=3)),
    #   color="purple"
    # )
    box(title="Within-Stratum Treatment Effect, \U03B2\U002A", status="primary", solidHeader=TRUE, width=4,
        h4(paste0(format(round(ResDF$logORst, digits=3), nsmall=3))))
  })
  
  output$UIRHOrow <- renderUI({
    if (input$S==1) {
      ResDF <- ResultsFnCapt(1)
    } else {
      ResDF <- ResultsFnCapt(input$f1target)
    }
    
    switch(input$ICRT,
           "IRT"=NULL,
           "CRT"=fluidRow(
             # infoBox("\U03C1\U2080\U002A",
             #         paste0(format(round(ResDF$rho0s[1], digits=3), nsmall=3)),
             #         color="purple"),
             box(title="Within-Stratum ICC, \U03C1\U2080\U002A", status="primary", solidHeader=TRUE, width=4,
                 h4(paste0(format(round(ResDF$rho0s[1], digits=3), nsmall=3)))),
             box(title="Overall ICC, \U03C1\U2080", status="warning", solidHeader=TRUE, width=4,
                 h4(paste0(format(round(ResDF$rho0, digits=3), nsmall=3))))))
  })
  
  output$UIFrow <- renderUI({
    Fval <- 1+((input$cv^2+1)*input$m-1)*input$rho0
    
    if (input$S==1) {
      ResDF <- ResultsFnCapt(1)
    } else {
      ResDF <- ResultsFnCapt(input$f1target)
    }
    
    switch(input$ICRT,
           "IRT"=NULL,
           "CRT"=fluidRow(
             box(title="Within-Stratum Design Effect, F\U002A", status="primary", solidHeader=TRUE, width=4,
                 h4(paste0(format(round(ResDF$F.strats[1], digits=3), nsmall=3)))),
             box(title="Overall Design Effect, F", status="warning", solidHeader=TRUE, width=4,
                 h4(paste0(format(round(Fval, digits=3), nsmall=3))))))
             # infoBox("F\U002A",
             #         paste0(format(round(ResDF$F.strats[1], digits=3), nsmall=3)),
             #         color="purple"),
             # infoBox("F",
             #         paste0(format(round(Fval, digits=3), nsmall=3)),
             #         color="orange")))
  })

  
  
########  DYNAMIC INPUT UI ###########

  
  #### Treatment Effects #####
  
  output$UIFXa <- renderUI({
    switch(input$S,
           "1"=NULL,
           "2"=box(width=12,
                   helpText("Fix Overall Treatment Effect (\U03B2) or Common Within-Stratum Treatment Effect (\U03B2\U002A)?"),
                   radioButtons("txFX",NULL,choices=list("Overall"="O","Within-Stratum"="S"),
                                selected=c("S"),inline=FALSE)),
           "3"=box(width=12,
                   helpText("Fix Overall Treatment Effect (\U03B2) or Common Within-Stratum Treatment Effect (\U03B2\U002A)?"),
                   radioButtons("txFX",NULL,choices=list("Overall"="O","Within-Stratum"="S"),
                                selected=c("S"),inline=FALSE))
    )
  })
  
  output$UIFXb <- renderUI({
    if (input$S == 1) {
      box(title="Target Effect Size (\U03B2)", status="primary", width=12, solidHeader=TRUE,
          numericInput("betast", NULL, value=log(.5)))
    } else {
      switch(input$txFX,
             "O"=box(title="Target Overall Effect Size (\U03B2)", status="primary",
                     width=12, solidHeader=TRUE,
                     numericInput("beta", NULL, value=log(.5))),
             "S"=box(title="Target Within-Stratum Effect Size (\U03B2\U002A)", status="primary",
                     width=12, solidHeader=TRUE,
                     numericInput("betast", NULL, value=log(.5))))
    }
  })
  
  
  
  #### Stratification Info Column ####
  
  output$UIstrat <- renderUI({
    switch(input$S,
           "1" = box(
             title="Overall Info", status="warning", solidHeader=TRUE,
             width=12,
             sliderInput("pi0","\U03C0\U2080", min=0, max=1, value=.05)
           ),
           "2" = box(width=12,
             helpText("Fix Overall Probability (\U03C0\U2080) or Strata-Specific Probabilities (\U03C0\U2080\U2081, \U03C0\U2080\U2082)?"),
             radioButtons("fix2",NULL,choices=list("Overall"="O","Strata-Specific"="S"),
                          selected=c("O"),inline=FALSE)
           ),
           "3" = box(width=12,
                     helpText("Fix Overall Probability (\U03C0\U2080) or Strata-Specific Probabilities (\U03C0\U2080\U2081, \U03C0\U2080\U2082, \U03C0\U2080\U2083)?"),
                     radioButtons("fix3",NULL,choices=c("Overall"="O","Strata-Specific"="S"),
                                  selected=c("O"),inline=FALSE)
           ))
  })
  
  output$UIstrat2a <- renderUI({
    if (input$S != 2) {
      NULL
      } else {
      switch(input$fix2,
             "O"=box(title="Overall Info", status="warning", solidHeader=TRUE,
                           width=12,
                           sliderInput("pi0","\U03C0\U2080", min=0, max=1, value=.05)),
             "S"=box(title="Stratum 1 Info", status="warning", solidHeader=TRUE,
                                   width=12,
                                   sliderInput("range", "Range of f\U2081", min = 0,
                                               max = 1, value = c(.25, .75)),
                                   sliderInput("f1target", "Target f\U2081", min = 0,
                                               max = 1, value = c(.5)),
                                   sliderInput("pi01","\U03C0\U2080\U2081", min=0, max=1, value=.04)))
    }})
  
  output$UIstrat2b <- renderUI({
    if (input$S != 2) {
      NULL
      } else {
      switch(input$fix2,
             "O"=box(title="Stratum 1 Info", status="warning", solidHeader=TRUE,
                           width=12,
                           sliderInput("range", "Range of f\U2081", min = 0,
                                       max = 1, value = c(.25, .75)),
                           sliderInput("f1target", "Target f\U2081", min = 0,
                                       max = 1, value = c(.5)),
                           sliderInput("pi01","\U03C0\U2080\U2081", min=0, max=1, value=.04)),
             "S"=box(title="Stratum 2 Info", status="warning", solidHeader=TRUE,
                                   width=12,
                                   sliderInput("pi02", "\U03C0\U2080\U2082", min=0, max=1, value=.06)))
    }})
  
  output$UIstrat3a <- renderUI({
    if (input$S != 3) {NULL}
    else {
      switch(input$fix3,
             "O"=box(title="Overall Info", status="warning", solidHeader=TRUE,
                           width=12,
                           sliderInput("pi0","\U03C0\U2080", min=0, max=1, value=.05)),
             "S"=box(title="Stratum 1 Info", status="warning", solidHeader=TRUE,
                                   width=12,
                                   sliderInput("range", "Range of f\U2081", min = 0,
                                               max = 1, value = c(1/6, 1/2)),
                                   sliderInput("f1target", "Target f\U2081", min = 0,
                                               max = 1, value = c(1/3)),
                                   sliderInput("pi01","\U03C0\U2080\U2081", min=0, max=1, value=.04)))
    }})
  
  output$UIstrat3b <- renderUI({
    if (input$S != 3) {NULL}
    else {
      switch(input$fix3,
             "O"=box(title="Stratum 1 Info", status="warning", solidHeader=TRUE,
                           width=12,
                           sliderInput("range", "Range of f\U2081", min = 0,
                                       max = 1, value = c(1/6, 1/2)),
                           sliderInput("f1target", "Target f\U2081", min = 0,
                                       max = 1, value = c(1/3)),
                           sliderInput("pi01","\U03C0\U2080\U2081", min=0, max=1, value=.04)),
             "S"=box(title="Stratum 2 Info", status="warning", solidHeader=TRUE,
                                   width=12,
                                   sliderInput("f2target", "Target f\U2082", min = 0,
                                               max = 1, value=c(1/3)),
                                   sliderInput("pi02", "\U03C0\U2080\U2082", min=0, max=1, value=.06)))
    }})
  
  output$UIstrat3c <- renderUI({
    if (input$S != 3) {NULL}
    else {
      switch(input$fix3,
             "O"=box(title="Stratum 2 Info", status="warning", solidHeader=TRUE,
                           width=12,
                           sliderInput("f2target", "Target f\U2082", min = 0,
                                       max = 1, value = c(1/3)),
                           sliderInput("pi02","\U03C0\U2080\U2082", min=0, max=1, value=.06)),
             "S"=box(title="Stratum 3 Info", status="warning", solidHeader=TRUE,
                                   width=12,
                                   sliderInput("pi03", "\U03C0\U2080\U2083", min=0, max=1, value=.05)))
    }})
  
  
  
  
  #### Cluster Info Column ####
  
  output$UIclust <- renderUI({
    switch(input$ICRT,
           "IRT" = NULL,
           "CRT" = box(
             title="Cluster Information", status="info", solidHeader=TRUE,
             width=12,
             numericInput("m","Mean Cluster Size", value=4),
             numericInput("cv","Coefficient of Variation of Cluster Size", value=0),
             sliderInput("rho0","\U03C1\U2080", min=0, max=1, value=.1)
             #sliderInput("rho0st","rho_0^*", min=0, max=1, value=.07)
           ))
  })


#### Warning/Error Box ####
  
UIerr1 <- function() {
  if (input$S == 1) {
    if (input$betast == 0) {
      return(p("Error: Target Effect Size Must Be Non-Zero"))
    } else {
      return(NULL)
    }
  } else {
    if (input$txFX == "O") {
      if (input$beta == 0) {
        return(p("Error: Target Effect Size Must Be Non-Zero"))
      } else {
        return(NULL)
      }
    } else {
      if (input$betast == 0) {
        return(p("Error: Target Effect Size Must Be Non-Zero"))
      } else {
        return(NULL)
      }
    }
  }
}
  
  UIerr2 <- function() {
    if (input$S==2) {
      if (input$fix2=="O") {
        if ((input$pi0 - input$f1target*input$pi01)/(1-input$f1target) < 0 | (input$pi0 - input$f1target*input$pi01)/(1-input$f1target) > 1) {
          return(p("Error: Gives target \U03C0\U2080\U2082 outside of [0,1]"))
        }
      }
    }
    return(NULL)
  }
  
  UIerr3 <- function() {
    if (input$S==3) {
      if (input$fix3=="O") {
        f3 <- 1 - input$f1target - input$f2target
        pi03 <- (input$pi0 - input$f1target*input$pi01 - input$f2target*input$pi02)/f3
        if (pi03 < 0 | pi03 > 1) {
          return(p("Error: Gives target \U03C0\U2080\U2083 outside of [0,1]"))
        }
      }
    }
    return(NULL)
  }
  
  UIerr4 <- function() {
    if (input$ICRT == "CRT") {
      if (input$m < 1) {
        return(p("Error: Mean Cluster Size Must Be At Least One"))
      }
    }
    return(NULL)
  }
  
  UIerr5 <- function() {
    if (input$ICRT == "CRT") {
      if (input$cv < 0) {
        return(p("Error: Coefficient of Variation of Cluster Size Must Be Non-Negative"))
      }
    }
    return(NULL)
  }
  
output$UIerrs <- renderUI({
  err1 <- UIerr1()
  err2 <- UIerr2()
  err3 <- UIerr3()
  err4 <- UIerr4()
  err5 <- UIerr5()
  if(sum(is.null(err1),is.null(err2),is.null(err3),is.null(err4),is.null(err5)) == 5) {
    NULL
  } else {
    box(title="Parameter Errors", status="danger", solidHeader=TRUE,
        width=12,
        err1,err2,err3,err4,err5)
  }
})
  
output$uiTargErr <- renderUI({
  ResCapt <- ResultsFnCapt(input$f1target)
  if (sum(ResCapt=="A")==length(ResCapt)) {
    box(title="Target Trial Errors", status="danger", solidHeader=TRUE,
        width=12,
        helpText(p("These target parameters do not yield a sample size"),
                        p("Check for parameter errors at the bottom of the parameter selection page."),
                        p("Some parameters in the f\U2081 range may still be functional; see Sensitivity tabs.")))
  } else if (input$S > 1 & input$ICRT == "CRT" & is.na(ResCapt$rho0s[1])) {
    box(title="Target Trial Errors", status="danger", solidHeader=TRUE,
        width=12,
        helpText(p("These target parameters do not yield a common within-stratum ICC."),
                 p("First, check for parameter errors at the bottom of the parameter selection page."),
                 p("If there are no parameter errors, this often indicates \U03C1\U2080 is too low for the variability between strata."),
                 p("In that case, select a larger \U03C1\U2080 or less predictive strata."),
                 p("Some parameters in the f\U2081 range may still be functional; see Sensitivity tabs.")))
  } else {
    NULL
  }
})
})

