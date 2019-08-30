#################################
#### R Code for: ################
#### Kennedy-Shaffer & Hughes 2019
#### Shiny App Implementation
#################################



##################################
##### function OR.convert    #####
##### Eqn 4 in Manuscript    #####
##################################

### Inputs:

## All of the following:
# pi0s: vector of within-stratum probabilities of event on control
# fs: vector of fractions of subjects in each stratum, summing to 1

## One of the following:
# OR: overall odds ratio (marginal across clusters)
# logOR: natural log of overall odds ratio
# ORst: common within-stratum odds ratio
# logORst: natural log of common within-stratum odds ratio

### Outputs a list with the following components:
## OR, logOR, ORst, logORst
# pi0: overall probability of event on control
# pi1: overall probability of event on treatment
# fs: as inputted
# pi0s: as inputted
# pi1s: vector of within-stratum probabilities of event on treatment

OR.convert <- function(pi0s,fs,OR=NA,logOR=NA,ORst=NA,logORst=NA) {
  if (sum(fs) != 1) {
    stop("Subject Fractions (fs) Do Not Sum to 1")
  }
  
  if (sum(ifelse(pi0s<0,1,ifelse(pi0s>1,1,0))) > 0) {
    stop("Probabilities (pi0s) Out of Range. Ensure All Are In [0,1]")
    #outerr <- list(NA, NA, NA, NA, NA, NA, fs, pi0s, NA)
    #names(outerr) <- c("OR", "logOR", "ORst", "logORst", "pi0", "pi1", "fs", "pi0s", "pi1s")
    #return(outerr)
  }
  
  if (length(fs) != length(pi0s)) {
    stop("Lengths of Vectors (fs and pi0s) Do Not Match")
  }
  
  pi0=sum(fs*pi0s)
  if (is.na(logOR) & is.na(OR)) {
    logORst <- ifelse(is.na(logORst),log(ORst),logORst)
    ORst <- exp(logORst)
    pi1s <- (ORst*pi0s/(1-pi0s))/(1+(ORst*pi0s/(1-pi0s)))
    pi1 <- sum(fs*pi1s)
    OR <- (pi1/(1-pi1))/(pi0/(1-pi0))
    logOR <- log(OR)
  }
  else {
    logOR <- ifelse(is.na(logOR),log(OR),logOR)
    OR <- exp(logOR)
    pi1 <- (OR*pi0/(1-pi0))/(1+(OR*pi0/(1-pi0)))
    rootfunc <- function(x) pi1-sum(fs*exp(x)*pi0s/(1-pi0s+exp(x)*pi0s))
    intmin <- ifelse(logOR>0,0,-50)
    intmax <- ifelse(logOR>0,50,0)
    logORst <- uniroot(f=rootfunc, interval=c(intmin,intmax))$root
    ORst <- exp(logORst)
    pi1s <- (ORst*pi0s/(1-pi0s))/(1+(ORst*pi0s/(1-pi0s)))
  }
  
  out <- list(OR, logOR, ORst, logORst, pi0, pi1, fs, pi0s, pi1s)
  names(out) <- c("OR", "logOR", "ORst", "logORst", "pi0", "pi1", "fs", "pi0s", "pi1s")
  
  return(out)
}




####################################
##### function ICC.overall     #####
##### Eqn 15/16 in Manuscript  #####
####################################

### Inputs:
# pi0s: vector of within-stratum probabilities of event on control
# fs: vector of fractions of subjects in each stratum, summing to 1
# rho0s: vector of within-stratum ICC values or scalar for common within-stratum ICC value


### Outputs a list with the following components:
# rho0: overall ICC. NA if rho0 falls outside of [0,1].
# pi0: overall probability of event on control
# fs, rho0s, pi0s: as inputted

ICC.overall <- function(pi0s,fs,rho0s) {
  if (sum(fs) != 1) {
    stop("Subject Fractions (fs) Do Not Sum to 1")
  }
  
  if (sum(ifelse(pi0s<0,1,ifelse(pi0s>1,1,0))) > 0) {
    warning("Probabilities (pi0s) Out of Range. Ensure All Are In [0,1]")
    outerr <- list(NA, NA, fs, pi0s, NA)
    names(outerr) <- c("rho0", "pi0", "fs", "pi0s", "rho0s")
    return(outerr)
  }
  
  if (sum(ifelse(rho0s<0,1,ifelse(rho0s>1,1,0))) > 0) {
    warning("Within-Stratum ICCs (rho0s) Out of Range. Ensure All Are In [0,1]")
    outerr <- list(NA, NA, fs, pi0s, NA)
    names(outerr) <- c("rho0", "pi0", "fs", "pi0s", "rho0s")
    return(outerr)
  }
  
  if (length(fs) != length(pi0s)) {
    stop("Lengths of Vectors (fs and pi0s) Do Not Match")
  }
  
  if (length(rho0s) == 1) {
    rho0s.use <- rep(rho0s, length(fs))
  }
  else {rho0s.use <- rho0s}
  
  if (length(fs) != length(rho0s.use)) {
    stop("Lengths of Vectors (fs and rho0s) Do Not Match")
  }
  
  pi0 <- sum(fs*pi0s)
  
  rho0 <- (sum(rho0s.use*fs*pi0s*(1-pi0s)) + sum(fs*(pi0s-pi0)^2))/(pi0*(1-pi0))
  
  rho0 <- ifelse(rho0 < 0 | rho0 > 1, NA, rho0)
  
  out <- list(rho0, pi0, fs, pi0s, rho0s.use)
  names(out) <- c("rho0", "pi0", "fs", "pi0s", "rho0s")
  return(out)
}



##################################
##### function ICC.common    #####
##### Eqn 17 in Manuscript   #####
##################################

### Inputs:
# pi0s: vector of within-stratum probabilities of event on control
# fs: vector of fractions of subjects in each stratum, summing to 1
# rho0: overall ICC


### Outputs a list with the following components:
# rho0st: common within-stratum ICC
# pi0: overall probability of event on control
# fs, rho0, pi0s: as inputted

ICC.common <- function(pi0s,fs,rho0) {
  if (sum(fs) != 1) {
    stop("Subject Fractions (fs) Do Not Sum to 1")
  }
  
  if (sum(ifelse(pi0s<0,1,ifelse(pi0s>1,1,0))) > 0) {
    warning("Probabilities (pi0s) Out of Range. Ensure All Are In [0,1]")
    outerr <- list(NA, NA, fs, pi0s, rho0)
    names(outerr) <- c("rho0st", "pi0", "fs", "pi0s", "rho0")
    return(outerr)
  }
  
  if (rho0<0 | rho0>1) {
    warning("Overall ICC (rho0) Out of Range. Ensure It Is In [0,1]")
    outerr <- list(NA, NA, fs, pi0s, rho0)
    names(outerr) <- c("rho0st", "pi0", "fs", "pi0s", "rho0")
    return(outerr)
  }
  
  if (length(fs) != length(pi0s)) {
    stop("Lengths of Vectors (fs and pi0s) Do Not Match")
  }
  
  pi0 <- sum(fs*pi0s)
  
  rho0st <- (rho0*pi0*(1-pi0) - sum(fs*(pi0s-pi0)^2))/(sum(fs*pi0s*(1-pi0s)))
  rho0st <- ifelse(rho0st < 0 | rho0st > 1, NA, rho0st)
  
  out <- list(rho0st, pi0, fs, pi0s, rho0)
  names(out) <- c("rho0st", "pi0", "fs", "pi0s", "rho0")
  return(out)
}


##################################
##### function NIRT          #####
##### Eqn 1 in Manuscript    #####
##################################

### Inputs:
# pi0: overall probability of event on control
# alpha: Type I Error rate, or Size of Test (default = 0.05)
## One of the Following:
# pi1: hypothesized overall probability of event on treatment to power test
# OR: hypothesized overall odds ratio of treatment vs. control to power test
# logOR: hypothesized log of overall odds ratio of treatment vs. control to power test
## One of the Following:
# gamma: Type II Error rate, or 1-Power of Test (default = 0.1)
# NIRT: sample size (total number of subjects) used in unstratified test


### Outputs a list with the following components:
# pi0: as inputted
# pi1, OR, and logOR
# gamma and NIRT

NIRT <- function(pi0, alpha=.05, pi1=NA, OR=NA, logOR=NA, gamma=.1, NIRT=NA) {
  if (is.na(pi1)) {
    OR <- ifelse(is.na(OR), exp(logOR), OR)
    logOR <- log(OR)
    pi1 <- OR*pi0/(1-pi0+OR*pi0)
  }
  else {
    OR <- (pi1/(1-pi1))/(pi0/(1-pi0))
    logOR <- log(OR)
  }
  
  Zalpha <- qnorm(alpha/2, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  paren <- (2/(pi0*(1-pi0))) + (2/(pi1*(1-pi1)))
  
  if (is.na(NIRT)) {
    Zgamma <- qnorm(gamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
    NIRT <- ((Zalpha+Zgamma)^2/(logOR^2))*paren
  }
  else {
    Zgamma <- sqrt((NIRT*(logOR^2))/paren)-Zalpha
    gamma <- pnorm(Zgamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  }
  
  out <- list(pi0,pi1,OR,logOR,gamma,NIRT)
  names(out) <- c("pi0","pi1","OR","logOR","gamma","NIRT")
  return(out)
}



##################################
##### function NIRTs         #####
##### Eqn 3 in Manuscript    #####
##################################

### Inputs:
# pi0s: vector of within-stratum probabilities of event on control
# fs: vector of fractions of subjects in each stratum, summing to 1
# alpha: Type I Error rate, or Size of Test (default = 0.05)
## One of the Following:
# ORst: hypothesized common within-stratum odds ratio of treatment vs. control to power test
# logORst: hypothesized log of common within-stratum odds ratio of treatment vs. control to power test
## One of the Following:
# gamma: Type II Error rate, or 1-Power of Test (default = 0.1)
# NIRTs: sample size (total number of subjects) used in stratified test


### Outputs a list with the following components:
# pi0s: as inputted
# pi1s: vector of hypothesized within-stratum probabilities of event on treatment
# ORst and logORst
# gamma and NIRTs

NIRTs <- function(pi0s, fs, alpha=.05, ORst=NA, logORst=NA, gamma=.1, NIRT=NA) {
  if (sum(fs) != 1) {
    stop("Subject Fractions (fs) Do Not Sum to 1")
  }
  
  if (sum(ifelse(pi0s<0,1,ifelse(pi0s>1,1,0))) > 0) {
    stop("Probabilities (pi0s) Out of Range. Ensure All Are In [0,1]")
  }
  
  if (length(fs) != length(pi0s)) {
    stop("Lengths of Vectors (fs and pi0s) Do Not Match")
  }
  
  ORst <- ifelse(is.na(ORst), exp(logORst), ORst)
  logORst <- log(ORst)
  pi1s <- ORst*pi0s/(1-pi0s+ORst*pi0s)
  
  Zalpha <- qnorm(alpha/2, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  brackinv <- (sum(fs*((2/(pi0s*(1-pi0s))) + (2/(pi1s*(1-pi1s))))^(-1)))^(-1)
  
  if (is.na(NIRTs)) {
    Zgamma <- qnorm(gamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
    NIRTs <- ((Zalpha+Zgamma)^2/(logORst^2))*brackinv
  }
  else {
    Zgamma <- sqrt((NIRTs*(logORst^2))/brackinv)-Zalpha
    gamma <- pnorm(Zgamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  }
  
  out <- list(pi0s,pi1s,ORst,logORst,gamma,NIRTs)
  names(out) <- c("pi0s","pi1s","ORst","logORst","gamma","NIRTs")
  return(out)
}




#####################################
##### function RIRT             #####
##### Eqns 1,3,5 in Manuscript  #####
#####################################

### Inputs:
# pi0s: vector of within-stratum probabilities of event on control
# fs: vector of fractions of subjects in each stratum, summing to 1
# alpha: Type I Error rate, or Size of Test (default = 0.05)
# gamma: Type II Error rate, or 1-Power of Test (default = 0.1)
## One of the Following:
# OR: hypothesized overall odds ratio of treatment vs. control to power test
# logOR: hypothesized log of overall odds ratio of treatment vs. control to power test
# ORst: hypothesized common within-stratum odds ratio of treatment vs. control to power test
# logORst: hypothesized log of common within-stratum odds ratio of treatment vs. control to power test


### Outputs a list with the following components:
# NIRT: sample size (total number of subjects) used in unstratified test
# NIRTs: sample size (total number of subjects) used in stratified test
# RIRT: ratio of NIRTs to NIRT
# OR, logOR, ORst, logORst

RIRT <- function(pi0s, fs, alpha=.05, gamma=.1, OR=NA, logOR=NA, ORst=NA, logORst=NA) {
  OR.conv <- OR.convert(pi0s=pi0s, fs=fs, OR, logOR, ORst, logORst)
  
  Zalpha <- qnorm(alpha/2, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  Zgamma <- qnorm(gamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  Z2 <- (Zalpha+Zgamma)^2
  paren <- 2/(OR.conv$pi0*(1-OR.conv$pi0))+2/(OR.conv$pi1*(1-OR.conv$pi1))
  brackinv <- (sum(fs*(2/(OR.conv$pi0s*(1-OR.conv$pi0s))+2/(OR.conv$pi1s*(1-OR.conv$pi1s)))^(-1)))^(-1)
  
  NIRT <- (Z2/OR.conv$logOR^2)*paren
  NIRTs <- (Z2/OR.conv$logORst^2)*brackinv
  RIRT <- NIRTs/NIRT
  
  out <- list(NIRT,NIRTs,RIRT,OR.conv$OR,OR.conv$logOR,OR.conv$ORst,OR.conv$logORst,pi0s)
  names(out) <- c("NIRT","NIRTs","RIRT","OR","logOR","ORst","logORst","pi0s")
  return(out)
}



#####################################
##### function RIRT.inc         #####
##### Eqns 1,3,5 in Manuscript  #####
#####################################

### Inputs:
# pi0s: vector of within-stratum probabilities of event on control except last stratum
# fs: vector of fractions of subjects in each stratum except last stratum
# pi0: overall probability of event on control
# alpha: Type I Error rate, or Size of Test (default = 0.05)
# gamma: Type II Error rate, or 1-Power of Test (default = 0.1)
## One of the Following:
# OR: hypothesized overall odds ratio of treatment vs. control to power test
# logOR: hypothesized log of overall odds ratio of treatment vs. control to power test
# ORst: hypothesized common within-stratum odds ratio of treatment vs. control to power test
# logORst: hypothesized log of common within-stratum odds ratio of treatment vs. control to power test


### Outputs a list with the following components:
# NIRT: sample size (total number of subjects) used in unstratified test
# NIRTs: sample size (total number of subjects) used in stratified test
# RIRT: ratio of NIRTs to NIRT
# OR, logOR, ORst, logORst

RIRT.inc <- function(pi0s, fs, pi0, alpha=.05, gamma=.1, OR=NA, logOR=NA, ORst=NA, logORst=NA) {
  if (sum(fs) > 1) {
    stop("Subject Fractions (fs) Sum to Greater Than 1")
  }
  
  fs.new <- 1-sum(fs)
  pi0s.new <- (pi0 - sum(fs*pi0s))/fs.new
  if (pi0s.new < 0 | pi0s.new > 1) {
    warning("Overall Probability (pi0) Incompatible With Within-Stratum Probabilities (pi0s)")
    outerr <- list(NA,NA,NA,NA,NA,NA,NA)
    names(outerr) <- c("NIRT","NIRTs","RIRT","OR","logOR","ORst","logORst")
    return(outerr)
  }
  fs.vec <- c(fs,fs.new)
  pi0s.vec <- c(pi0s,pi0s.new)
  
  out <- RIRT(pi0s=pi0s.vec, fs=fs.vec, alpha, gamma, OR, logOR, ORst, logORst)
  return(out)
}


  

##################################
##### function NCRT          #####
##### Eqn 6 in Manuscript    #####
##################################

### Inputs:
# pi0: overall probability of event on control
# alpha: Type I Error rate, or Size of Test (default = 0.05)
## One of the Following:
# pi1: hypothesized overall probability of event on treatment to power test
# OR: hypothesized overall odds ratio of treatment vs. control to power test
# logOR: hypothesized log of overall odds ratio of treatment vs. control to power test

## Either:
# F: the overall design effect
## Or, to use the design effect F = 1+(mbar-1) rho0:
# mbar: the mean cluster size
# rho0: the overall ICC
### Can optionally additionally specify CV to use design effect 1+((CV^2+1)mbar-1) rho0:
### CV: the coefficient of variation (standard deviation/mean) of cluster sizes

## One of the Following:
# gamma: Type II Error rate, or 1-Power of Test (default = 0.1)
# NCRT: sample size (total number of subjects) used in unstratified test


### Outputs a list with the following components:
# pi0: as inputted
# pi1
# F: as inputted or as determined from mbar, rho0, and (if specified) CV
# OR and logOR
# gamma and NCRT

NCRT <- function(pi0, alpha=.05, pi1=NA, OR=NA, logOR=NA, F=NA, mbar=NA, rho0=NA, CV=NA, gamma=.1, NCRT=NA) {
  if (is.na(pi1)) {
    OR <- ifelse(is.na(OR), exp(logOR), OR)
    logOR <- log(OR)
    pi1 <- OR*pi0/(1-pi0+OR*pi0)
  }
  else {
    OR <- (pi1/(1-pi1))/(pi0/(1-pi0))
    logOR <- log(OR)
  }
  
  if (is.na(F)) {
    if (is.na(mbar) | is.na(rho0)) {
      stop("Must specify either F or both mbar and rho0")
    }
    if (is.na(CV)) {
      F <- 1+(mbar-1)*rho0
    }
    else {
      F <- 1+((CV^2+1)*mbar-1)*rho0
    }
  }
  
  Zalpha <- qnorm(alpha/2, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  paren <- (2/(pi0*(1-pi0))) + (2/(pi1*(1-pi1)))
  
  if (is.na(NCRT)) {
    Zgamma <- qnorm(gamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
    NCRT <- ((Zalpha+Zgamma)^2/(logOR^2))*F*paren
  }
  else {
    Zgamma <- sqrt((NCRT*(logOR^2))/(F*paren))-Zalpha
    gamma <- pnorm(Zgamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  }
  
  out <- list(pi0,pi1,F,OR,logOR,gamma,NCRT)
  names(out) <- c("pi0","pi1","F","OR","logOR","gamma","NCRT")
  return(out)
}



##################################
##### function NCRTs         #####
##### Eqn 11 in Manuscript   #####
##################################

### Inputs:
# pi0s: vector of within-stratum probabilities of event on control
# fs: vector of fractions of subjects in each stratum, summing to 1
# alpha: Type I Error rate, or Size of Test (default = 0.05)
## One of the Following:
# ORst: hypothesized common within-stratum odds ratio of treatment vs. control to power test
# logORst: hypothesized log of common within-stratum odds ratio of treatment vs. control to power test

## Either:
# Fs: vector of within-stratum design effects
## Or, to use the design effect F = 1+(mbar-1) rho0 within each stratum:
# mbars: vector of mean within-stratum cluster sizes
# rho0s: vector of within-stratum ICCs
### Can optionally additionally specify CVs to use design effect 1+((CV^2+1)mbar-1) rho0:
### CVs: vector of the coefficient of variation of cluster sizes
## If any of these are inputted as scalar, that value will be used for all strata

## One of the Following:
# gamma: Type II Error rate, or 1-Power of Test (default = 0.1)
# NCRTs: sample size (total number of subjects) used in stratified test

### Outputs a list with the following components:
# pi0s: as inputted
# pi1s: vector of hypothesized within-stratum probabilities of event on treatment
# Fs: as inputted or as determined from mbars, rho0s, and (if specified) CVs
# ORst and logORst
# gamma and NCRTs

NCRTs <- function(pi0s, fs, alpha=.05, ORst=NA, logORst=NA, Fs=NA, mbars=NA, rho0s=NA, CVs=NA, gamma=.1, NCRTs=NA) {
  if (sum(fs) != 1) {
    stop("Subject Fractions (fs) Do Not Sum to 1")
  }
  
  if (sum(ifelse(pi0s<0,1,ifelse(pi0s>1,1,0))) > 0) {
    stop("Probabilities (pi0s) Out of Range. Ensure All Are In [0,1]")
  }
  
  if (length(fs) != length(pi0s)) {
    stop("Lengths of Vectors (fs and pi0s) Do Not Match")
  }
  
  if (!is.na(Fs[1])) {
    if (length(Fs) == 1) {
      Fs <- rep(Fs,length(fs))
    } else if (length(Fs) != length(fs)) {
      stop("Lengths of Vectors (fs and Fs) Do Not Match")
    }
  } else {
    if(is.na(mbars[1]) | is.na(rho0s[1])) {
      stop("Must Specify either Fs or both mbars and rho0s")
    } else{
      if (length(mbars) == 1) {
        mbars <- rep(mbars, length(fs))
      }
      if (length(rho0s) == 1) {
        rho0s <- rep(rho0s, length(fs))
      }
      if (is.na(CVs[1])) {
        CVs <- rep(0, length(fs))
      } else if (length(CVs) == 1) {
        CVs <- rep(CVs, length(CVs))
      }
      if (!all.equal(length(CVs),length(mbars),length(rho0s),length(fs))) {
        stop("One of mbars, rho0s, and (if specified) CVs is a vector of a different length than fs")
      }
      Fs <- 1 + ((CVs^2+1)*mbars-1)*rho0s
    }
  }
  
  ORst <- ifelse(is.na(ORst), exp(logORst), ORst)
  logORst <- log(ORst)
  pi1s <- ORst*pi0s/(1-pi0s+ORst*pi0s)
  
  Zalpha <- qnorm(alpha/2, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  brackinv <- (sum(fs/Fs*((2/(pi0s*(1-pi0s))) + (2/(pi1s*(1-pi1s))))^(-1)))^(-1)
  
  if (is.na(NCRTs)) {
    Zgamma <- qnorm(gamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
    NCRTs <- ((Zalpha+Zgamma)^2/(logORst^2))*brackinv
  }
  else {
    Zgamma <- sqrt((NCRTs*(logORst^2))/brackinv)-Zalpha
    gamma <- pnorm(Zgamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  }
  
  out <- list(pi0s,pi1s,Fs,ORst,logORst,gamma,NCRTs)
  names(out) <- c("pi0s","pi1s","Fs","ORst","logORst","gamma","NCRTs")
  return(out)
}




######################################
##### function RCRT              #####
##### Eqn 6,11,13 in Manuscript  #####
######################################

### Inputs:
# pi0s: vector of within-stratum probabilities of event on control
# fs: vector of fractions of subjects in each stratum, summing to 1
# alpha: Type I Error rate, or Size of Test (default = 0.05)
# gamma: Type II Error rate, or 1-Power of Test (default = 0.1)
## One of the Following:
# OR: hypothesized overall odds ratio of treatment vs. control to power test
# logOR: hypothesized log of overall odds ratio of treatment vs. control to power test
# ORst: hypothesized common within-stratum odds ratio of treatment vs. control to power test
# logORst: hypothesized log of common within-stratum odds ratio of treatment vs. control to power test

## Either:
# F.strats: vector of within-stratum design effects; and
# Fover: overall design effect
## Or, to use the design effect F = 1+(mbar-1) rho0 within each stratum and overall:
# mbars: vector of mean within-stratum cluster sizes
# rho0s: vector of within-stratum ICCs
# rho0: overall ICC (can be left NA if mbars and CVs are common across strata)
### Can optionally additionally specify CVs to use design effect 1+((CV^2+1)mbar-1) rho0:
### CVs: vector of the coefficient of variation of cluster sizes
## If any of these are inputted as scalar, that value will be used for all strata


### Outputs a list with the following components:
# NCRT: sample size (total number of subjects) used in unstratified test
# NCRTs: sample size (total number of subjects) used in stratified test
# RCRT: ratio of NCRTs to NCRT
# RIRT: ratio of NIRTs to NIRT if these were IRTs
# pi0s: as inputted
# pi1s: vector of hypothesized within-stratum probabilities of event on treatment
# Fs: as inputted or as determined from mbars, rho0s, and (if specified) CVs
# OR, logOR, ORst and logORst

RCRT <- function(pi0s, fs, alpha=.05, gamma=.1, OR=NA, logOR=NA, ORst=NA, logORst=NA, 
                 F.strats=NA, Fover=NA, mbars=NA, rho0s=NA, rho0=NA, CVs=NA) {
  OR.conv <- OR.convert(pi0s=pi0s, fs=fs, OR, logOR, ORst, logORst)
  
  if (sum(is.na(F.strats))==0) {
    if (length(F.strats) == 1) {
      Fs.new <- rep(F.strats,length(fs))
    } else if (length(F.strats) == length(fs)) {
      Fs.new <- F.strats
    } else {
      stop("Lengths of Vectors (fs and Fs) Do Not Match")
    }
    
    if (is.na(Fover)) {
      stop("Must Specify Overall Design Effect (Fover) if Specify Within-Stratum Fs")
    }
  } else {
    if(sum(is.na(mbars)) > 0 | sum(is.na(rho0s)) > 0) {
      stop("Must Specify either Fs or both mbars and rho0s")
    } else if (length(mbars) == 1) {
      mbars <- rep(mbars, length(fs))
    }
    if (length(rho0s) == 1 & sum(is.na(rho0s)) == 0) {
      rho0s <- rep(rho0s, length(fs))
    }
    if (sum(is.na(CVs)) > 0) {
      CVs.new <- rep(0, length(fs))
    } else if (length(CVs) == 1) {
      CVs.new <- rep(CVs, length(fs))
    } else {
      CVs.new <- CVs
    }
    if (!all.equal(length(CVs.new),length(mbars),length(rho0s),length(fs))) {
      stop("One of mbars, rho0s, and (if specified) CVs is a vector of a different length than fs")
    }
    if (is.na(rho0)) {
      if (length(unique(mbars))==1 & length(unique(CVs.new))==1) {
        rho0 <- unlist(ICC.overall(pi0s,fs,rho0s)$rho0)
      } else {
        stop("Must Specify rho0 or Have the Same mbars and CVs Values Across Strata")
      }
    }
    Fs.new <- 1 + ((CVs.new^2+1)*mbars-1)*rho0s
    mbar <- 1/sum(fs/mbars)
    gs <- (fs/mbars)/sum(fs/mbars)
    CV <- sqrt(sum(gs*(CVs*mbars)^2) + sum(gs*(mbars-mbar)^2))/mbar
    Fover <- 1 + ((CV^2+1)*mbar-1)*rho0
  }
  
  Zalpha <- qnorm(alpha/2, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  Zgamma <- qnorm(gamma, mean=0, sd=1, lower.tail=TRUE, log.p=FALSE)
  Z2 <- (Zalpha + Zgamma)^2
  paren <- Fover*(2/(OR.conv$pi0*(1-OR.conv$pi0)) + 2/(OR.conv$pi1*(1-OR.conv$pi1)))
  brackinv <- (sum(fs/Fs.new*((2/(pi0s*(1-pi0s))) + (2/(OR.conv$pi1s*(1-OR.conv$pi1s))))^(-1)))^(-1)
  
  NCRT <- paren*Z2/(OR.conv$logOR^2)
  NCRTs <- brackinv*Z2/(OR.conv$logORst^2)
  RCRT <- NCRTs/NCRT
  
  V <- 1/(OR.conv$pi0*(1-OR.conv$pi0)) + 1/(OR.conv$pi1*(1-OR.conv$pi1))
  Vs <- 1/(OR.conv$pi0s*(1-OR.conv$pi0s)) + 1/(OR.conv$pi1s*(1-OR.conv$pi1s))
  RIRT <- (OR.conv$logOR/OR.conv$logORst)^2/sum(fs*V/Vs)
  
  out <- list(NCRT,NCRTs,RCRT,RIRT,OR.conv$pi0s,OR.conv$pi1s,rho0s,Fs.new,OR.conv$OR,OR.conv$logOR,OR.conv$ORst,OR.conv$logORst)
  names(out) <- c("NCRT","NCRTs","RCRT","RIRT","pi0s","pi1s","rho0s","F.strats","OR","logOR","ORst","logORst")
  return(out)
}





#######################################
##### function RCRT.inc           #####
##### Eqn 6,11,13 in Manuscript   #####
#######################################

### Inputs:
# Strata: number of strata

# pi0s: vector of within-stratum probabilities of event on control of length Strata, OR BOTH:
# pi0s: pi0s but without last stratum AND
# pi0: overall probability of event on control

# fs: vector of fractions of subjects in each stratum of length Strata, summing to 1, OR
# fs: fs but without last stratum

# mbar: cluster size to use within each stratum, OR:
# mbars: vector of mean within-stratum cluster sizes of length Strata, OR BOTH:
# mbars: mbars but without last stratum AND
# mbar: overall mean cluster size

# CV: coefficient of variation of cluster sizes to use within each stratum, OR:
# CVs: vector of within-stratum CV of cluster sizes of length Strata, OR BOTH:
# CVs: CVs but without last stratum AND
# CV: overall CV
# Note: If neither CV nor CVs is specified, a CV of 0 will be used within each stratum.

# rho0: overall ICC (to use when assuming a common within-stratum ICC, requires common mean cluster size/CV), OR BOTH:
# rho0: overall ICC (to use when non-common within-stratum ICC but still common mean cluster size/CV) AND
# rho0s: vector of within-stratum ICCs without last stratum (length Strata-1), OR:
# rho0s: vector of within-stratum ICCs of length Strata (to use when common mean cluster size/CV), OR BOTH:
# rho0s: vector of within-stratum ICCs of length Strata AND
# rho0: overall ICC

# alpha: Type I Error rate, or Size of Test (default = 0.05)
# gamma: Type II Error rate, or 1-Power of Test (default = 0.1)

## One of the Following:
# OR: hypothesized overall odds ratio of treatment vs. control to power test
# logOR: hypothesized log of overall odds ratio of treatment vs. control to power test
# ORst: hypothesized common within-stratum odds ratio of treatment vs. control to power test
# logORst: hypothesized log of common within-stratum odds ratio of treatment vs. control to power test

### Outputs a list with the following components:
# NCRT: sample size (total number of subjects) used in unstratified test
# NCRTs: sample size (total number of subjects) used in stratified test
# RCRT: ratio of NCRTs to NCRT
# RIRT: ratio of NIRTs to NIRT if these were IRTs
# pi0s: as inputted
# pi1s: vector of hypothesized within-stratum probabilities of event on treatment
# Fs: as inputted or as determined from mbars, rho0s, and (if specified) CVs
# OR, logOR, ORst and logORst

RCRT.inc <- function(Strata, pi0s, pi0=NA, fs, mbars=NA, mbar=NA, CV=NA, CVs=NA, rho0=NA, rho0s=NA, 
                     alpha=.05, gamma=.1, OR=NA, logOR=NA, ORst=NA, logORst=NA) {
  if (length(fs) == Strata) {
    fs.full <- fs
  }
  else if (length(fs) == (Strata - 1)) {
    fs.full <- c(fs,1-sum(fs))
  }
  else {
    stop("Length of fs must be either Strata or Strata-1")
  }
  
  if ((sum(!is.finite(fs.full)) + sum(fs.full < 0) + sum(fs.full > 1)) > 0) {
    warning("Inadmissible Parameter Combination: Non-numeric fs or fs < 0 or fs > 1")
    outerr <- list(NA,NA,NA,NA,NA,NA,NA,OR,logOR,ORst,logORst)
    names(outerr) <- c("NCRT","NCRTs","RCRT","RIRT","pi0s","pi1s","Fs","OR","logOR","ORst","logORst")
    return(outerr)
  }
  
  if (length(pi0s) == Strata) {
    pi0s.full <- pi0s
  }
  else if (length(pi0s) == (Strata - 1) & !is.na(pi0) & sum(is.na(pi0s))==0) {
    pi0s.full <- c(pi0s,(pi0-sum(fs.full[-Strata]*pi0s))/fs.full[Strata])
  }
  else {
    stop("Length of pi0s must be either Strata or be Strata-1 and there be a specified pi0")
  }
  
  if ((sum(!is.finite(pi0s.full)) + sum(pi0s.full < 0) + sum(pi0s.full > 1)) > 0) {
    warning("Inadmissible Parameter Combination: Non-numeric pi0s or pi0s < 0 or pi0s > 1")
    outerr <- list(NA,NA,NA,NA,NA,NA,NA,OR,logOR,ORst,logORst)
    names(outerr) <- c("NCRT","NCRTs","RCRT","RIRT","pi0s","pi1s","Fs","OR","logOR","ORst","logORst")
    return(outerr)
  }
  
  if (is.na(mbar) & length(mbars) == Strata) {
    mbars.full <- mbars
  }
  else if (sum(is.na(mbar))==0 & is.na(mbars)) {
    mbars.full <- rep(mbar,Strata)
  }
  else if (!is.na(mbar) & length(mbars) == (Strata-1)) {
    mbars.full <- c(mbars,(mbar-sum(fs.full[-Strata]*mbars))/fs.full[Strata])
  }
  else {
    stop("mbar and mbars specified incorrectly")
  }
  
  if ((sum(!is.finite(mbars.full)) + sum(mbars.full < 1)) > 0) {
    warning("Inadmissible Parameter Combination: Non-numeric mbars or mbars < 1")
    outerr <- list(NA,NA,NA,NA,pi0s.full,NA,NA,OR,logOR,ORst,logORst)
    names(outerr) <- c("NCRT","NCRTs","RCRT","RIRT","pi0s","pi1s","Fs","OR","logOR","ORst","logORst")
    return(outerr)
  }
  
  if (is.na(CV) & sum(is.na(CVs))==0) {
    CVs.full <- rep(0,Strata)
  }
  else if (sum(is.na(CVs))>0) {
    if (is.na(CV)) {
      CV <- 0
    }
    CVs.full <- rep(CV,Strata)
  }
  else if (length(CVs) == Strata) {
    CVs.full <- CVs
  }
  else if (length(CVs) == (Strata - 1) & !is.na(CV)) {
    gs.full <- (fs.full/mbars.full)/sum(fs.full/mbars.full)
    mover <- sum(gs.full*mbars.full)
    SDover <- CV*mover
    gs.S1 <- gs.full[-Strata]
    mbars.S1 <- mbars.full[-Strata]
    CV.S <- sqrt((SDover^2 - sum(gs.full*(mbars.full-mover)^2) - sum(gs.S1*(CVs*mbars.S1)^2))/(gs.full[Strata]*(mbars.full[Strata])^2))
    CVs.full <- c(CVs,CV.S)
  }
  else {
    stop("cvs specified incorrectly")
  }
  
  if ((sum(!is.finite(CVs.full)) + sum(CVs.full < 0)) > 0) {
    warning("Inadmissible Parameter Combination: Non-numeric CVs or CVs < 1")
    outerr <- list(NA,NA,NA,NA,pi0s.full,NA,NA,OR,logOR,ORst,logORst)
    names(outerr) <- c("NCRT","NCRTs","RCRT","RIRT","pi0s","pi1s","Fs","OR","logOR","ORst","logORst")
    return(outerr)
  }
  
  if (xor(is.na(rho0),length(rho0s) != Strata)) {
    if (length(unique(mbars.full))==1 & length(unique(CVs.full))==1) {
      if (sum(is.na(rho0s)) == 0 & length(rho0s) == Strata) {
        rho0s.full <- rho0s
        rho0.full <- unlist(ICC.overall(pi0s = pi0s.full, fs = fs.full, rho0s = rho0s.full)$rho0)
      }
      else if (sum(is.na(rho0s)) == 0 & length(rho0s) == (Strata - 1)) {
        rho0.full <- rho0
        pi0.use <- sum(fs.full*pi0s.full)
        fs.S1 <- fs.full[-Strata]
        rho0s.S1 <- rho0s
        pi0s.S1 <- pi0s.full[-Strata]
        brack <- rho0.full - sum(fs.full*(pi0s.full-pi0.use)^2)/(pi0.use*(1-pi0.use))
        Last <- pi0.use*(1-pi0.use)*brack - sum(fs.S1*rho0s.S1*pi0s.S1*(1-pi0s.S1))
        rho0s.S <- Last/(fs.full[Strata]*pi0s.full[Strata]*(1-pi0s.full[Strata]))
        rho0s.full <- c(rho0s,rho0s.S)
      }
      else if (!is.na(rho0) & sum(is.na(rho0s))>0) {
        rho0.full <- rho0
        rho0s.full <- rep(unlist(ICC.common(pi0s = pi0s.full, fs = fs.full, rho0 = rho0.full)$rho0st),Strata)
      }
      else {
        stop("rho0 and/or rho0s specified incorrectly")
      }
    }
    else {
      stop("Must Have Common Within-Stratum mbars and CVs or Specify rho0 and Full rho0s Vector")
    }
  }
  else if (!is.na(rho0) & length(rho0s) == Strata) {
    rho0s.full <- rho0s
    rho0.full <- rho0
  }
  else {
    stop("rho0 and/or rho0s specified incorrectly")
  }
  
  if (!is.finite(rho0.full) | rho0.full > 1 | rho0.full < 0 | 
      (sum(!is.finite(rho0s.full)) + sum(rho0s.full > 1, na.rm=TRUE) + sum(rho0s.full < 0, na.rm=TRUE)) > 0) {
    warning("Inadmissible Parameter Combination: Non-numeric ICCs or ICCs < 0 or ICCs > 1")
    outerr <- list(NA,NA,NA,NA,pi0s.full,NA,NA,OR,logOR,ORst,logORst)
    names(outerr) <- c("NCRT","NCRTs","RCRT","RIRT","pi0s","pi1s","Fs","OR","logOR","ORst","logORst")
    return(outerr)
  }
  
  out <- RCRT(pi0s = pi0s.full, fs = fs.full, alpha, gamma, OR, logOR, ORst, logORst, F.strats=NA, Fover=NA, 
              mbars=mbars.full, rho0s=rho0s.full, rho0=rho0.full, CVs=CVs.full)
  return(out)
}