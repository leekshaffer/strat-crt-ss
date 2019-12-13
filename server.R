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
  
  ########  DYNAMIC INPUT UI ###########
  
  
  #### Treatment Effects #####
  
  output$UIFXa <- renderUI({
    req(input$S)
    switch(input$S,
           "1"=NULL,
           "2"=box(width=12,
                   helpText("Fix Overall Treatment Effect (\U03B2) or Common Within-Stratum Treatment Effect (\U03B2\U002A)?"),
                   radioButtons("txFX_2",NULL,choices=list("Overall"="O","Within-Stratum"="S"),
                                selected=c("O"),inline=FALSE)),
           "3"=box(width=12,
                   helpText("Fix Overall Treatment Effect (\U03B2) or Common Within-Stratum Treatment Effect (\U03B2\U002A)?"),
                   radioButtons("txFX_3",NULL,choices=list("Overall"="O","Within-Stratum"="S"),
                                selected=c("O"),inline=FALSE))
    )
  })
  
  output$UIFXb <- renderUI({
    req(input$S)
    if (input$S == 1) {
      box(title="Target Effect Size (\U03B2)", status="primary", width=12, solidHeader=TRUE,
          numericInput("betast_1", NULL, value=log(.5)))
    } else if (input$S == 2) {
      req(input$txFX_2)
      switch(input$txFX_2,
             "O"=box(title="Target Overall Effect Size (\U03B2)", status="primary",
                     width=12, solidHeader=TRUE,
                     numericInput("beta_2", NULL, value=log(.5), step=.25)),
             "S"=box(title="Target Within-Stratum Effect Size (\U03B2\U002A)", status="primary",
                     width=12, solidHeader=TRUE,
                     numericInput("betast_2", NULL, value=log(.5), step=.25)))
    } else {
      req(input$txFX_3)
      switch(input$txFX_3,
             "O"=box(title="Target Overall Effect Size (\U03B2)", status="primary",
                     width=12, solidHeader=TRUE,
                     numericInput("beta_3", NULL, value=log(.5), step=.25)),
             "S"=box(title="Target Within-Stratum Effect Size (\U03B2\U002A)", status="primary",
                     width=12, solidHeader=TRUE,
                     numericInput("betast_3", NULL, value=log(.5), step=.25)))
    }
  })
  
  
  
  #### Stratification Info Column ####
  
  output$UIstrat <- renderUI({
    req(input$S)
    switch(input$S,
           "1" = box(
             title="Overall Info", status="warning", solidHeader=TRUE,
             width=12,
             numericInput("pi0_1","\U03C0\U2080", min=0, max=1, value=.05, step=.05)
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
  
  output$UIstrat2Oa <- renderUI({
    req(input$S)
    if (input$S != "2") {
      NULL
    } else {
      req(input$fix2)
      if (input$fix2 == "O") {
        box(title="Overall Info", status="warning", solidHeader=TRUE,
            width=12,
            numericInput("pi0_2","\U03C0\U2080", min=0, max=1, value=.05, step=.05))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat2Ob <- renderUI({
    req(input$S)
    if (input$S != "2") {
      NULL
    } else {
      req(input$fix2)
      if (input$fix2 == "O") {
        box(title="Stratum 1 Info", status="warning", solidHeader=TRUE,
            width=12,
            sliderInput("range_2O", "Range of f\U2081", min = 0,
                        max = 1, value = c(.25, .75)),
            sliderInput("f1target_2O", "Target f\U2081", min = 0,
                        max = 1, value = c(.5)),
            numericInput("pi01_2O","\U03C0\U2080\U2081", min=0, max=1, value=.04, step=.01))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat2Sa <- renderUI({
    req(input$S)
    if (input$S != "2") {
      NULL
    } else {
      req(input$fix2)
      if (input$fix2 == "S") {
        box(title="Stratum 1 Info", status="warning", solidHeader=TRUE,
            width=12,
            sliderInput("range_2S", "Range of f\U2081", min = 0,
                        max = 1, value = c(.25, .75)),
            sliderInput("f1target_2S", "Target f\U2081", min = 0,
                        max = 1, value = c(.5)),
            numericInput("pi01_2S","\U03C0\U2080\U2081", min=0, max=1, value=.04, step=.01))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat2Sb <- renderUI({
    req(input$S)
    if (input$S != "2") {
      NULL
    } else {
      req(input$fix2)
      if (input$fix2 == "S") {
        box(title="Stratum 2 Info", status="warning", solidHeader=TRUE,
            width=12,
            numericInput("pi02_2S", "\U03C0\U2080\U2082", min=0, max=1, value=.06, step=.01))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat3Oa <- renderUI({
    req(input$S)
    if (input$S != "3") {
      NULL
    } else {
      req(input$fix3)
      if (input$fix3 == "O") {
        box(title="Overall Info", status="warning", solidHeader=TRUE,
            width=12,
            numericInput("pi0_3","\U03C0\U2080", min=0, max=1, value=.05, step=.01))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat3Ob <- renderUI({
    req(input$S)
    if (input$S != "3") {
      NULL
    } else {
      req(input$fix3)
      if (input$fix3 == "O") {
        box(title="Stratum 1 Info", status="warning", solidHeader=TRUE,
            width=12,
            sliderInput("range_3O", "Range of f\U2081", min = 0,
                        max = 1, value = c(.16, .5)),
            sliderInput("f1target_3O", "Target f\U2081", min = 0,
                        max = 1, value = c(.33)),
            numericInput("pi01_3O","\U03C0\U2080\U2081", min=0, max=1, value=.04, step=.01))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat3Oc <- renderUI({
    req(input$S)
    if (input$S != "3") {
      NULL
    } else {
      req(input$fix3)
      if (input$fix3 == "O") {
        box(title="Stratum 2 Info", status="warning", solidHeader=TRUE,
            width=12,
            sliderInput("f2target_3O", "Target f\U2082", min = 0,
                        max = 1, value = c(.33)),
            numericInput("pi02_3O","\U03C0\U2080\U2082", min=0, max=1, value=.06, step=.01))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat3Sa <- renderUI({
    req(input$S)
    if (input$S != "3") {
      NULL
    } else {
      req(input$fix3)
      if (input$fix3 == "S") {
        box(title="Stratum 1 Info", status="warning", solidHeader=TRUE,
            width=12,
            sliderInput("range_3S", "Range of f\U2081", min = 0,
                        max = 1, value = c(.16, .5)),
            sliderInput("f1target_3S", "Target f\U2081", min = 0,
                        max = 1, value = c(.33)),
            numericInput("pi01_3S","\U03C0\U2080\U2081", min=0, max=1, value=.04, step=.01))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat3Sb <- renderUI({
    req(input$S)
    if (input$S != "3") {
      NULL
    } else {
      req(input$fix3)
      if (input$fix3 == "S") {
        box(title="Stratum 2 Info", status="warning", solidHeader=TRUE,
            width=12,
            sliderInput("f2target_3S", "Target f\U2082", min = 0,
                        max = 1, value=c(.33)),
            numericInput("pi02_3S", "\U03C0\U2080\U2082", min=0, max=1, value=.06, step=.01))
      } else {
        NULL
      }
    }
  })
  
  output$UIstrat3Sc <- renderUI({
    req(input$S)
    if (input$S != "3") {
      NULL
    } else {
      req(input$fix3)
      if (input$fix3 == "S") {
        box(title="Stratum 3 Info", status="warning", solidHeader=TRUE,
            width=12,
            numericInput("pi03_3S", "\U03C0\U2080\U2083", min=0, max=1, value=.05, step=.01))
      } else {
        NULL
      }
    }
  })
  
  #### Cluster Info Column ####
  
  output$UIclusta <- renderUI({
    req(input$ICRT,input$S)
    if (input$ICRT=="CRT" & input$S != "1") {
      box(width=12,
          helpText("Assume Common Within-Stratum ICC (\U03C1\U2080\U002A) and Cluster Size Distribution or Specify Within-Stratum ICCs?"),
          radioButtons("rhoChoice",NULL,choices=list("Common"="C","Within-Stratum"="S"),
                       selected=c("C"),inline=FALSE))
    } else {
      NULL
    }
  })
  
  output$UIclustb <- renderUI({
    req(input$ICRT,input$S)
    if (input$ICRT == "IRT") {
      NULL
    } else if (input$S == "1") {
      box(
        title="Overall Clustering Information",
        status="info", solidHeader=TRUE, width=12,
        numericInput("m","Mean Cluster Size", value=4, step=1, min=1),
        numericInput("cv","Coefficient of Variation of Cluster Size", value=0, step=.25, min=0),
        numericInput("rho0","\U03C1\U2080", min=0, max=1, value=.1, step=.05))
    } else {
      req(input$rhoChoice)
      if (input$rhoChoice=="C") {
        box(
          title="Overall Clustering Information",
          status="info", solidHeader=TRUE, width=12,
          numericInput("m","Mean Cluster Size", value=4, step=1, min=1),
          numericInput("cv","Coefficient of Variation of Cluster Size", value=0, step=.25, min=0),
          numericInput("rho0","\U03C1\U2080", min=0, max=1, value=.1, step=.05))
      } else {
        box(
          title="Overall Clustering Information",
          status="info", solidHeader=TRUE, width=12,
          numericInput("rho0","\U03C1\U2080", min=0, max=1, value=.1, step=.05))
      }
    }
  })
  
  output$UIclustc <- renderUI({
    req(input$ICRT,input$S, input$rhoChoice)
    if (input$ICRT=="CRT" & input$S == "2" & input$rhoChoice=="S") {
      box(width=12,
          title="Within-Stratum Clustering Information", status="info", solidHeader=TRUE,
          numericInput("m1","Mean Cluster Size in Stratum 1", value=4, step=1, min=1),
          numericInput("cv1","Coefficient of Variation of Cluster Size in Stratum 1", value=0, step=.25, min=0),
          numericInput("rho01","\U03C1\U2080\U2081", min=0, max=1, value=.1, step=.05),
          numericInput("m2","Mean Cluster S   ze in Stratum 2", value=4, step=1, min=1),
          numericInput("cv2","Coefficient of Variation of Cluster Size in Stratum 2", value=0, step=.25, min=0),
          numericInput("rho02","\U03C1\U2080\U2082", min=0, max=1, value=.1, step=.05))
    } else if (input$ICRT=="CRT" & input$S == "3" & input$rhoChoice=="S") {
      req(input$rho0)
      box(width=12,
          title="Within-Stratum Clustering Information", status="info", solidHeader=TRUE,
          numericInput("m1","Mean Cluster Size in Stratum 1", value=4, step=1, min=1),
          numericInput("cv1","Coefficient of Variation of Cluster Size in Stratum 1", value=0, step=.25, min=0),
          numericInput("rho01","\U03C1\U2080\U2081", min=0, max=1, value=.1, step=.05),
          numericInput("m2","Mean Cluster Size in Stratum 2", value=4, step=1, min=1),
          numericInput("cv2","Coefficient of Variation of Cluster Size in Stratum 2", value=0, step=.25, min=0),
          numericInput("rho02","\U03C1\U2080\U2082", min=0, max=1, value=.1, step=.1),
          numericInput("m3","Mean Cluster Size in Stratum 3", value=4, step=1, min=1),
          numericInput("cv3","Coefficient of Variation of Cluster Size in Stratum 3", value=0, step=.25, min=0),
          numericInput("rho03","\U03C1\U2080\U2083", min=0, max=1, value=.1, step=.05))
    } else {
      NULL
    }
  })
  
  
  createDF <- reactive({
    req(input$S,input$ICRT,input$sig,input$power)
    df <- list(S=input$S, ICRT=input$ICRT, sig=input$sig, power=input$power,
               rhoInErr = FALSE, piErr = FALSE, fErr = FALSE, betaErr = FALSE,
               mErr = FALSE, cvErr = FALSE, rhoStErr = FALSE, inErr = FALSE)
    if (df$S == "1") {
      req(input$pi0_1,input$betast_1)
      df$fs <- c(1)
      df$pi0s <- c(input$pi0_1)
      df$betast <- input$betast_1
      df$txFX <- "S"
      df$beta <- NA
      df$fix <- NA
    } else if (df$S == "2") {
      req(input$txFX_2,input$fix2)
      df$txFX <- input$txFX_2
      if (df$txFX == "S") {
        req(input$betast_2)
        df$betast <- input$betast_2
        df$beta <- NA
      } else {
        req(input$beta_2)
        df$betast <- NA
        df$beta <- input$beta_2
      }
      df$fix <- input$fix2
      if (df$fix == "S") {
        req(input$pi01_2S,input$pi02_2S,input$f1target_2S)
        df$pi0s <- c(input$pi01_2S,input$pi02_2S)
        df$fs <- c(input$f1target_2S,1-input$f1target_2S)
      } else {
        req(input$pi0_2,input$pi01_2O,input$f1target_2O)
        df$fs <- c(input$f1target_2O,1-input$f1target_2O)
        pi02 <- (input$pi0_2 - input$pi01_2O*input$f1target_2O)/(1-input$f1target_2O)
        df$pi0s <- c(input$pi01_2O,pi02)
      }
    } else {
      req(input$txFX_3,input$fix3)
      df$txFX <- input$txFX_3
      if (df$txFX == "S") {
        req(input$betast_3)
        df$betast <- input$betast_3
        df$beta <- NA
      } else {
        req(input$beta_3)
        df$betast <- NA
        df$beta <- input$beta_3
      }
      df$fix <- input$fix3
      if (df$fix == "S") {
        req(input$pi01_3S,input$pi02_3S,input$pi03_3S,
            input$f1target_3S,input$f2target_3S)
        df$pi0s <- c(input$pi01_3S,input$pi02_3S,input$pi03_3S)
        df$fs <- c(input$f1target_3S,input$f2target_3S,
                   1-sum(input$f1target_3S,input$f2target_3S))
      } else {
        req(input$pi0_3,input$pi01_3O,input$pi02_3O,
            input$f1target_3O,input$f2target_3O)
        df$fs <- c(input$f1target_3O,input$f2target_3O,
                   1-sum(input$f1target_3O,input$f2target_3O))
        pi03 <- (input$pi0_3-input$pi01_3O*input$f1target_3O-input$pi02_3O*input$f2target_3O)/(1-input$f1target_3O-input$f2target_3O)
        df$pi0s <- c(input$pi01_3O,input$pi02_3O,pi03)
      }
    }
    numStrat <- as.numeric(df$S)
    if (df$ICRT == "IRT") {
      df$ms <- rep(1,numStrat)
      df$cvs <- rep(0,numStrat)
      df$rho0s <- rep(1,numStrat)
      df$rho0 <- 1
      df$rhoChoice <- NA
    } else {
      if (numStrat == 1) {
        req(input$m,input$cv,input$rho0)
        df$ms <- c(input$m)
        df$cvs <- c(input$cv)
        df$rho0s <- c(input$rho0)
        df$rho0 <- input$rho0
        df$rhoChoice <- NA
      } else {
        req(input$rhoChoice,input$rho0)
        df$rhoChoice <- input$rhoChoice
        df$rho0 <- c(input$rho0)
        if (df$rhoChoice == "C") {
          req(input$m,input$cv)
          df$ms <- rep(input$m,numStrat)
          df$cvs <- rep(input$cv,numStrat)
          df$rho0s <- rep(unlist(ICC.common(pi0s=df$pi0s,fs=df$fs,rho0=df$rho0)$rho0st),numStrat)
          if (sum(is.na(df$rho0s)) == length(df$rho0s)) {df$rhoStErr <- TRUE}
        } else {
          if (numStrat == 2) {
            req(input$m1,input$m2,input$cv1,input$cv2,
                input$rho01,input$rho02,input$rho0)
            df$ms <- c(input$m1,input$m2)
            df$cvs <- c(input$cv1,input$cv2)
            df$rho0s <- c(input$rho01,input$rho02)
            df$rho0 <- c(input$rho0)
          } else {
            req(input$m1,input$m2,input$m3,input$cv1,input$cv2,input$cv3,
                input$rho01,input$rho02,input$rho03,input$rho0)
            df$ms <- c(input$m1,input$m2,input$m3)
            df$cvs <- c(input$cv1,input$cv2,input$cv3)
            df$rho0s <- c(input$rho01,input$rho02,input$rho03)
            df$rho0 <- c(input$rho0)
          }
        }
      }
      if (sum(df$ms < 1) > 0) {df$mErr <- TRUE}
      if (sum(df$cvs < 0) > 0) {df$cvErr <- TRUE}
      if (df$rho0 < 0 | df$rho0 > 1) {df$rhoInErr <- TRUE}
      if (sum(is.na(df$rho0s)) == 0) {
        if (sum(df$rho0 < 0) + sum(df$rho0 > 1) > 0) {df$rhoInErr <- TRUE}
      }
    }
    if (sum(df$pi0s < 0) + sum(df$pi0s > 1) > 0) {df$piErr <- TRUE}
    if (sum(df$fs < 0) + sum(df$fs > 1) > 0) {df$fErr <- TRUE}
    if (df$txFX == "S") {
      if (df$betast == 0) {df$betaErr <- TRUE}
    } else {
      if (df$beta == 0) {df$betaErr <- TRUE}
    }
    df$inErr <- df$rhoInErr | df$piErr | df$fErr | df$betaErr | df$mErr | df$cvErr
    return(df)
  })
  
  createRes <- reactive({
    inList <- createDF()
    if (inList$inErr) {
      return(list(NCRTs=NA,NCRT=NA,RCRT=NA,logOR=inList$beta,logORst=inList$betast,
                  rho0=inList$rho0,rho0s=rep(NA,as.numeric(inList$S)),
                  F.strats=rep(NA,as.numeric(inList$S))))
      }
    else if (inList$rhoStErr) {
      if (inList$txFX == "S") {
        betast <- inList$betast
        beta <- unlist(OR.convert(pi0s=inList$pi0s,fs=inList$fs,logORst=inList$betast)$logOR)
      } else {
        beta <- inList$beta
        betast <- unlist(OR.convert(pi0s=inList$pi0s,fs=inList$fs,logOR=beta)$logORst)
      }
      NCRTres <- NCRT(pi0=sum(inList$pi0s*inList$fs),
                  alpha=inList$sig/100,gamma=1-inList$power/100,
                  logOR=beta,
                  mbar=inList$ms[1], rho0=inList$rho0, CV=inList$cvs[1])
      NCRTres$NCRTs <- NA
      NCRTres$RCRT <- NA
      NCRTres$logOR <- beta
      NCRTres$logORst <- betast
      NCRTres$rho0s <- rep(NA,as.numeric(inList$S))
      NCRTres$F.strats <- rep(NA,as.numeric(inList$S))
      return(NCRTres)
    } else {
      return(RCRT(pi0s=inList$pi0s,fs=inList$fs,
                  alpha=inList$sig/100,gamma=1-inList$power/100,
                  logOR=inList$beta,logORst=inList$betast,
                  mbars=inList$ms,rho0s=inList$rho0s,
                  rho0=inList$rho0,CVs=inList$cvs))
    }
  })
  
  createResFn <- function(inList,f1) {
    len <- 7+2*as.numeric(inList$S)
    if (sum(inList$ms < 1) + sum(inList$cvs < 0) > 1) {return(c(f1,rep(NA,len-1)))}
    if (inList$betaErr) {return(c(f1,rep(NA,len-1)))}
    outList <- NULL
    if (inList$S == "2") {
      outList$fs <- c(f1,1-f1)
      if (sum(outList$fs < 0) + sum(outList$fs > 1) > 0) {return(c(f1,rep(NA,len-1)))}
      if (inList$fix == "O") {
        pi0 <- sum(inList$fs*inList$pi0s)
        outList$pi0s <- c(inList$pi0s[1],(pi0-inList$pi0s[1]*f1)/(1-f1))
        if (sum(outList$pi0s < 0) + sum(outList$pi0s > 1) > 0) {return(c(f1,rep(NA,len-1)))}
      } else {
        outList$pi0s <- inList$pi0s
        if (sum(outList$pi0s < 0) + sum(outList$pi0s > 1) > 0) {return(c(f1,rep(NA,len-1)))}
      }
    } else if (inList$S == "3") {
      outList$fs <- c(f1,inList$fs[2],1-sum(f1,inList$fs[2]))
      if (sum(outList$fs < 0) + sum(outList$fs > 1) > 0) {return(c(f1,rep(NA,len-1)))}
      if (inList$fix == "O") {
        pi0 <- sum(inList$fs*inList$pi0s)
        outList$pi0s <- c(inList$pi0s[1],inList$pi0s[2],(pi0-inList$pi0s[1]*outList$fs[1]-inList$pi0s[2]*outList$fs[2])/outList$fs[3])
        if (sum(outList$pi0s < 0) + sum(outList$pi0s > 1) > 0) {return(c(f1,rep(NA,len-1)))}
      } else {
        outList$pi0s <- inList$pi0s
        if (sum(outList$pi0s < 0) + sum(outList$pi0s > 1) > 0) {return(c(f1,rep(NA,len-1)))}
      }
    }
    if (inList$ICRT == "CRT") {
      outList$rho0s <- rep(unlist(ICC.common(outList$pi0s,outList$fs,inList$rho0)$rho0st),as.numeric(inList$S))
      if (sum(is.na(outList$rho0s)) > 0) {return(c(f1,rep(NA,len-1)))}
      F.star <- 1+((inList$cvs[1]^2+1)*inList$ms[1]-1)*outList$rho0s[1]
    } else {
      outList$rho0s <- inList$rho0s
      if (sum(outList$rho0s < 0) + sum(outList$rho0s > 1) > 0) {return(c(f1,rep(NA,len-1)))}
      F.star <- 1
    }
    Res <- RCRT(pi0s=outList$pi0s,fs=outList$fs,
                alpha=inList$sig/100,gamma=1-inList$power/100,
                logOR=inList$beta,logORst=inList$betast,
                mbars=inList$ms,rho0s=outList$rho0s,
                rho0=inList$rho0,CVs=inList$cvs)
    RetVec <- c(outList$fs,outList$pi0s,
                Res$NCRT,Res$NCRTs,Res$RCRT,
                outList$rho0s[1],F.star,Res$logOR,Res$logORst)
    return(RetVec)
  }
  
  CreateResFnCapt <- function(inList,f1) {
    len <- 7+2*as.numeric(inList$S)
    capture.output(ResCapt <- tryCatch({createResFn(inList,f1)},
                                       error=function(err) {c(f1,rep(NA,len-1))}), file="/dev/null")
    return(ResCapt)
  }
  
  rangeDF <- reactive({
    ins <- createDF()
    numStrat <- as.numeric(ins$S)
    if (numStrat == 1) {
      return(NULL)
    } else {
      if (ins$ICRT == "CRT") {
        if (ins$rhoChoice == "S") {
          return(NULL)
        }
      }
      if (numStrat == 2) {
        names <- c("f\U2081","f\U2082","\U03C0\U2080\U2081","\U03C0\U2080\U2082",
                     "N_CRT","N_CRT(S)","R_CRT","\U03C1\U2080\U002A","F\U002A",
                     "\U03B2","\U03B2\U002A")
        if (ins$fix == "O") {
          req(input$range_2O)
          range <- input$range_2O
        } else {
          req(input$range_2S)
          range <- input$range_2S
        }
      } else {
        names <- c("f\U2081","f\U2082","f\U2083",
                     "\U03C0\U2080\U2081","\U03C0\U2080\U2082","\U03C0\U2080\U2083",
                     "N_CRT","N_CRT(S)","R_CRT","\U03C1\U2080\U002A","F\U002A",
                     "\U03B2","\U03B2\U002A")
        if (ins$fix == "O") {
          req(input$range_3O)
          range <- input$range_3O
        } else {
          req(input$range_3S)
          range <- input$range_3S
        }
      }
      r1 <- ifelse(range[1] <= 0, .01, range[1])
      r2 <- ifelse(range[2] >= 1, .99, range[2])
      r <- seq(from=r1, to=r2, by=.01)
      RangeRes <- t(sapply(X=r, FUN=function(z) CreateResFnCapt(ins,z)))
      colnames(RangeRes) <- names
      if (ins$ICRT == "IRT") {
        names[((numStrat*2+1):(numStrat*2+3))] <- c("N_CRT","N_CRT(S)","R_CRT")
        colnames(RangeRes) <- names
        RangeRes <- RangeRes[,-((numStrat*2+4):(numStrat*2+5))]
      }
      return(RangeRes)
    }
  })
  
  output$UIdata <- renderUI({
    ins <- createDF()
    if (ins$S == "1") {
      box(title="No Sensitivity Tables Available for 1-Stratum Calculations",
          width=12)
    } else if (!is.na(ins$rhoChoice) & ins$rhoChoice == "S") {
      box(title="No Sensitivity Tables Available for Specified Within-Stratum ICCs, Since the Overall ICC Depends on f\U2081 in Unknown Way",
          width=12)
    } else {
      box(title="Data to Generate Sensitivity Plots", width=12, renderTable(rangeDF(), digits=3))
    }
  })
  
  output$table <- renderTable(createDF(), digits=2)
  output$restable <- renderTable(createRes(), digits=2)
  output$rangeDF <- renderTable(rangeDF(), digits=3)


  ######## DYNAMIC SENSITIVITY PLOTS UI ###########
  
  output$UIplotsa <- renderUI({
    ins <- createDF()
    if (ins$S == "1") {
      box(title="No Sensitivity Plots Available for 1-Stratum Calculations",
          width=12)
    } else if (!is.na(ins$rhoChoice) & ins$rhoChoice == "S") {
      box(title="No Sensitivity Plots Available for Specified Within-Stratum ICCs, Since the Overall ICC Depends on f\U2081 in Unknown Way",
          width=12)
    } else {
      numStrat <- as.numeric(ins$S)
      rangeRes <- rangeDF()
      if (ins$ICRT == "IRT") {
        NIRTsvals <- rangeRes[,(numStrat*2+2)]
        output$distplot <- renderPlot({plot(x=rangeRes[,1], y=NIRTsvals, type="l",
                                            col=4, lwd=3,
                                            xlim=c(min(rangeRes[,1],na.rm=TRUE),max(rangeRes[,1],na.rm=TRUE)),
                                            ylim=c(min(NIRTsvals, na.rm=TRUE),max(NIRTsvals, na.rm=TRUE)),
                                            xlab=expression("f"["1"]), ylab=expression("N"["IRT(S)"]))})
        fluidRow(
          box(
            title="Plot of N_IRT(S) (Sample Size Required for Stratified IRT) vs. f\U2081 (Proportion of Participants in Stratum 1)",
            width=12,plotOutput("distplot")))
      } else {
        NCRTsvals <- rangeRes[,(numStrat*2+2)]
        output$distplot <- renderPlot({plot(x=rangeRes[,1], y=NCRTsvals, type="l",
                                            col=4, lwd=3,
                                            xlim=c(min(rangeRes[,1],na.rm=TRUE),max(rangeRes[,1],na.rm=TRUE)),
                                            ylim=c(min(NCRTsvals, na.rm=TRUE),max(NCRTsvals, na.rm=TRUE)),
                                            xlab=expression("f"["1"]), ylab=expression("N"["CRT(S)"]))})
        fluidRow(
          box(
            title="Plot of N_CRT(S) (Sample Size Required for Stratified CRT) vs. f\U2081 (Proportion of Participants in Stratum 1)",
            width=12,plotOutput("distplot")))
      }
    }
  })
  
  output$UIplotsb <- renderUI({
    ins <- createDF()
    if (ins$S == "1") {
      NULL
    } else if (!is.na(ins$rhoChoice) & ins$rhoChoice == "S") {
      NULL
    } else {
      numStrat <- as.numeric(ins$S)
      rangeRes <- rangeDF()
      if (ins$ICRT == "IRT") {
        RIRTvals <- rangeRes[,(numStrat*2+3)]
        output$distplotb <- renderPlot({plot(x=rangeRes[,1], y=RIRTvals, type="l",
                                            col=3, lwd=3,
                                            xlim=c(min(rangeRes[,1],na.rm=TRUE),max(rangeRes[,1],na.rm=TRUE)),
                                            ylim=c(min(RIRTvals, na.rm=TRUE),max(RIRTvals, na.rm=TRUE)),
                                            xlab=expression("f"["1"]), ylab=expression("R"["IRT"]))})
        fluidRow(
          box(
            title="Plot of R_IRT (Ratio of Sample Sizes Required for Stratified IRT vs. Unstratified IRT) vs. f\U2081 (Proportion of Participants in Stratum 1)",
            width=12,plotOutput("distplotb")))
      } else {
        RCRTvals <- rangeRes[,(numStrat*2+3)]
        output$distplotb <- renderPlot({plot(x=rangeRes[,1], y=RCRTvals, type="l",
                                            col=3, lwd=3,
                                            xlim=c(min(rangeRes[,1],na.rm=TRUE),max(rangeRes[,1],na.rm=TRUE)),
                                            ylim=c(min(RCRTvals, na.rm=TRUE),max(RCRTvals, na.rm=TRUE)),
                                            xlab=expression("f"["1"]), ylab=expression("R"["CRT"]))})
        fluidRow(
          box(
            title="Plot of R_CRT (Ratio of Sample Sizes Required for Stratified CRT vs. Unstratified CRT) vs. f\U2081 (Proportion of Participants in Stratum 1)",
            width=12,plotOutput("distplotb")))
      }
    }
  })
  

  ###### DYNAMIC BOX RESULTS UI #########

  output$NCRTs <- renderUI({
    ins <- createDF()
    res <- createRes()
    box(title=ifelse(ins$ICRT=="IRT", "Sample Size Required for Stratified IRT, N_IRT(S)", "Sample Size Required for Stratified CRT, N_CRT(S)"), status="primary", solidHeader=TRUE, width=4,
        h4(paste0(format(ceiling(res$NCRTs), big.mark=","))))
  })

  output$NCRT <- renderUI({
    ins <- createDF()
    res <- createRes()
    box(title=ifelse(ins$ICRT=="IRT", "Sample Size Required for Unstratified IRT, N_IRT", "Sample Size Required for Unstratified CRT, N_CRT"), status="warning", solidHeader=TRUE, width=4,
        h4(paste0(format(ceiling(res$NCRT), big.mark=","))))
  })

  output$RCRT <- renderUI({
    ins <- createDF()
    res <- createRes()
    box(title=ifelse(ins$ICRT=="IRT", "Ratio of Required Sample Sizes, R_IRT", "Ratio of Required Sample Sizes, R_CRT"), status="success", solidHeader=TRUE, width=4,
        h4(paste0(format(round(res$RCRT, digits=3), nsmall=3))))
  })

  output$Beta <- renderUI({
    res <- createRes()
    box(title="Overall Treatment Effect, \U03B2", status="warning", solidHeader=TRUE, width=4,
        h4(paste0(format(round(res$logOR, digits=3), nsmall=3))))
  })

  output$BetaStar <- renderUI({
    res <- createRes()
    box(title="Within-Stratum Treatment Effect, \U03B2\U002A", status="primary", solidHeader=TRUE, width=4,
        h4(paste0(format(round(res$logORst, digits=3), nsmall=3))))
  })

  output$UIRHOrow <- renderUI({
    ins <- createDF()
    res <- createRes()
    if (ins$ICRT=="IRT") {
      NULL
    } else if (ins$S == "1") {
      fluidRow(
        box(title="Within-Stratum ICC, \U03C1\U2080\U002A", status="primary", solidHeader=TRUE, width=4,
            h4(paste0(format(round(res$rho0s[1], digits=3), nsmall=3)))),
        box(title="Overall ICC, \U03C1\U2080", status="warning", solidHeader=TRUE, width=4,
            h4(paste0(format(round(ins$rho0, digits=3), nsmall=3)))))
    } else {
      if (ins$rhoChoice=="C") {
      fluidRow(
        box(title="Within-Stratum ICC, \U03C1\U2080\U002A", status="primary", solidHeader=TRUE, width=4,
          h4(paste0(format(round(res$rho0s[1], digits=3), nsmall=3)))),
        box(title="Overall ICC, \U03C1\U2080", status="warning", solidHeader=TRUE, width=4,
          h4(paste0(format(round(ins$rho0, digits=3), nsmall=3)))))
      } else {
      if (ins$S == "2") {
        fluidRow(
          box(title="Within-Stratum ICCs: \U03C1\U2080\U2081, \U03C1\U2080\U2082", status="primary", solidHeader=TRUE, width=4,
            h4(paste0(format(round(res$rho0s[1], digits=3), nsmall=3),", ",format(round(res$rho0s[2], digits=3), nsmall=3)))),
          box(title="Overall ICC, \U03C1\U2080", status="warning", solidHeader=TRUE, width=4,
            h4(paste0(format(round(ins$rho0, digits=3), nsmall=3)))))
      } else if (ins$S == "3") {
        fluidRow(
          box(title="Within-Stratum ICCs: \U03C1\U2080\U2081, \U03C1\U2080\U2082, \U03C1\U2080\U2083", status="primary", solidHeader=TRUE, width=4,
              h4(paste0(format(round(res$rho0s[1], digits=3), nsmall=3),", ",format(round(res$rho0s[2], digits=3), nsmall=3),", ",format(round(res$rho0s[3], digits=3), nsmall=3)))),
          box(title="Overall ICC, \U03C1\U2080", status="warning", solidHeader=TRUE, width=4,
              h4(paste0(format(round(ins$rho0, digits=3), nsmall=3)))))
        }
      }
    }
  })

  output$UIFrow <- renderUI({
    ins <- createDF()
    res <- createRes()
    if (ins$ICRT == "IRT") {
    } else {
      if (ins$S == "1") {
        Fval <- 1+((ins$cvs^2+1)*ins$ms-1)*ins$rho0
      } else {
        if (ins$rhoChoice=="C") {
          Fval <- 1+((ins$cvs[1]^2+1)*ins$ms[1]-1)*ins$rho0
        } else {
          mbar.over <- 1/sum(ins$fs/ins$ms)
          gs <- (ins$fs/ins$ms)/sum(ins$fs/ins$ms)
          CV <- sqrt(sum(gs*(ins$cvs*ins$ms)^2) + sum(gs*(ins$ms-mbar.over)^2))/mbar.over
          Fval <- 1 + ((CV^2+1)*mbar.over-1)*ins$rho0
        }
      }
    }

    if (ins$ICRT=="IRT") {
      NULL
    } else if (ins$S == "1") {
      fluidRow(
        box(title="Within-Stratum Design Effect, F\U002A", status="primary", solidHeader=TRUE, width=4,
            h4(paste0(format(round(res$F.strats[1], digits=3), nsmall=3)))),
        box(title="Overall Design Effect, F", status="warning", solidHeader=TRUE, width=4,
            h4(paste0(format(round(Fval, digits=3), nsmall=3)))))
    } else if (ins$rhoChoice == "C") {
      fluidRow(
        box(title="Within-Stratum Design Effect, F\U002A", status="primary", solidHeader=TRUE, width=4,
            h4(paste0(format(round(res$F.strats[1], digits=3), nsmall=3)))),
        box(title="Overall Design Effect, F", status="warning", solidHeader=TRUE, width=4,
            h4(paste0(format(round(Fval, digits=3), nsmall=3)))))
    } else if (ins$S == "2") {
      fluidRow(
        box(title="Within-Stratum Design Effects: F\U2081, F\U2082", status="primary", solidHeader=TRUE, width=4,
            h4(paste0(format(round(res$F.strats[1], digits=3), nsmall=3),", ",format(round(res$F.strats[2], digits=3), nsmall=3)))),
        box(title="Overall Design Effect, F", status="warning", solidHeader=TRUE, width=4,
            h4(paste0(format(round(Fval, digits=3), nsmall=3)))))
    } else if (ins$S == "3") {
      fluidRow(
        box(title="Within-Stratum Design Effects: F\U2081, F\U2082, F\U2083", status="primary", solidHeader=TRUE, width=4,
            h4(paste0(format(round(res$F.strats[1], digits=3), nsmall=3),", ",format(round(res$F.strats[2], digits=3), nsmall=3),", ",format(round(res$F.strats[3], digits=3), nsmall=3)))),
        box(title="Overall Design Effect, F", status="warning", solidHeader=TRUE, width=4,
            h4(paste0(format(round(Fval, digits=3), nsmall=3)))))
    }
  })






#### Warning/Error Box ####

  UIerr1 <- reactive({
    ins <- createDF()
    if (ins$betaErr) {
      return(p("Error: Target effect size must be non-zero"))
    } else {
      return(NULL)
    }
  })
  
  UIerr2 <- reactive({
    ins <- createDF()
    if (ins$piErr) {
      return(p("Error: Results in a \U03C0 value outside of [0,1]"))
    } else {
      return(NULL)
    }
  })
  
  UIerr3 <- reactive({
    ins <- createDF()
    if (ins$fErr) {
      return(p("Error: Results in a f value outside of [0,1]"))
    } else {
      return(NULL)
    }
  })
  
  UIerr4 <- reactive({
    ins <- createDF()
    if (ins$mErr) {
      return(p("Error: Mean cluster size must be at least one"))
    } else {
      return(NULL)
    }
  })
  
  UIerr5 <- reactive({
    ins <- createDF()
    if (ins$cvErr) {
      return(p("Error: Coefficient of variation of cluster size must be at least one"))
    } else {
      return(NULL)
    }
  })
  
  UIerr6 <- reactive({
    ins <- createDF()
    if (ins$rhoInErr) {
      return(p("Error: \U03C1 values must be within [0,1]"))
    }
  })
  
  output$UIerrs <- renderUI({
    ins <- createDF()
    if (!ins$inErr) {
      NULL
    } else {
      box(title="Parameter Errors", status="danger", solidHeader=TRUE,
          width=12,
          UIerr1(),UIerr2(),UIerr3(),UIerr4(),UIerr5(),UIerr6())
    }
  })
  
  output$uiTargErr <- renderUI({
    ins <- createDF()
    if (ins$inErr) {
      box(title="Target Trial Errors", status="danger", solidHeader=TRUE,width=12,
          helpText(p("These target parameters do not yield a sample size."),
                   p("Check for parameter errors at the top of the parameter selection page.")))
    } else if (ins$rhoStErr) {
      box(title="Target Trial Errors", status="danger", solidHeader=TRUE,width=12,
          helpText(p("These target parameters do not yield a common within-stratum ICC."),
                   p("If there are no parameter errors, this often indicates \U03C1\U2080 is too low for the variability between strata."),
                   p("In that case, select a larger \U03C1\U2080 or less predictive strata."),
                   p("Some parameters in the f\U2081 range may still be functional; see Sensitivity tabs.")))
    } else {
      TargRes <- createRes()
      if (ins$S != "1" & is.na(TargRes$NCRTs)) {
        box(title="Target Trial Errors", status="danger", solidHeader=TRUE,width=12,
            helpText(p("These target parameters do not yield a stratified sample size."),
                     p("Some parameters in the f\U2081 range may still be functional; see Sensitivity tabs.")))
      } else {
        NULL
      }
    }
  })
})

