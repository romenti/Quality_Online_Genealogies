###############################################################################  
### Subroutine LifeTableFUN_II ################################################
############################################################################### 

  # Representativeness is crucial for inferring demographic processes from online genealogies: evidence from lifespan dynamics
  #
  # Author: Robert Stelter and Diego Alburez-Gutierrez
  # Datum:  21.09.2021
  # Source: The file and all the supplementary material can be found here:
  # https://osf.io/9gkmz/
  # Notes:  This code is required to run the R-script: 02_mortality.R
  #
  #         The orginal version of this code has been provided by:
  #         Camarda, C. G. (2015)). Life expectancy confidence interval, Available from https://sites.google.com/site/carlogiovannicamarda/r-stuff/life-expectancy-confidence-interval
  #
## function for constructing a classic (& rather general) lifetable
lifetable.mx <- function(x, mx, sex="M", ax=NULL){
  m <- length(x)
  n <- c(diff(x), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- ifelse(mx==0,n,n + 1/mx - n /(1-exp(-n*mx)))
      ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="F"){
        if(mx[1]>=0.107){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.053 + 2.800*mx[1]
        }
      }
      if(sex=="M"){
        if(mx[1]>=0.107){
          ax[1] <- 0.330
        }else{
          ax[1] <- 0.045 + 2.684*mx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- 1 / mx[m]
    }
  }
  #qx  <- n*mx / (1 + (n - ax) * mx)
  qx  <- 1-exp(-n*mx)
  #hacked in to avoid negative survival probabilities
  qx <- ifelse(qx>1,1, qx)
  # find the first qx>=1 or NA.
  i <- 1
  while (!is.na(qx[i]) & qx[i]<1 & i<m) {if(is.na(qx[i])){i <- i-1} else{i <- i + 1}}
  # Redefine...   
  ax[i] <- 1/mx[i]
  #qx[i] <- 1
  while(i<m){
    qx[i+1] <- NA
    ax[i+1] <- NA
    mx[i+1] <- NA
    i <- i+1
  }
  
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}


## function for constructing a classic (& rather general) lifetable
lifetable <- function(x, Nx, Dx, sex="M", ax=NULL){
  m <- length(x)
  mx  <- Dx/Nx
  n <- c(diff(x), NA)
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- ifelse(mx==0,5,n + 1/mx - n /(1-exp(-n*mx)))
      #  ax[m] <- 1 / mx[m]
    }else{    
      if(sex=="F"){
        if(mx[1]>=0.107){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.053 + 2.800*mx[1]
        }
      }
      if(sex=="M"){
        if(mx[1]>=0.107){
          ax[1] <- 0.330
        }else{
          ax[1] <- 0.045 + 2.684*mx[1]
        }
      }
      ax[-1] <- n[-1]/2
      #ax[m] <- 1 / mx[m]
    }
  }
  
  #qx  <- n*mx / (1 + (n - ax) * mx)
  qx  <- 1-exp(-n*mx)
  #hacked in to avoid negative survival probabilities
  qx <- ifelse(qx>1,1, qx)
  # find the first qx>=1 or NA.
  i <- 1
  while (!is.na(qx[i]) & qx[i]<1 & i<m) {if(is.na(qx[i])){i <- i-1} else{i <- i + 1}}
  # Redefine...   
  ax[i] <- 1/mx[i]
  #qx[i] <- 1
  while(i<m){
    qx[i+1] <- NA
    ax[i+1] <- NA
    mx[i+1] <- NA
    i <- i+1
  }
  
#  qx[m] <- 1
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]/mx[m]
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, Nx, Dx, mx, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

## function for constructing a lifetable starting from probabilities
lifetable.qx <- function(x, qx, sex="M", ax=NULL, last.ax=5.5){           #Changed last.ax from 5.5
  m <- length(x)
  n <- c(diff(x), NA)
  qx[is.na(qx)] <- 0
  if(is.null(ax)){
    ax <- rep(0,m)
    if(x[1]!=0 | x[2]!=1){
      ax <- n/2
      ax[m] <- last.ax
    }else{    
      if(sex=="F"){
        if(qx[1]>=0.1){
          ax[1] <- 0.350
        }else{
          ax[1] <- 0.05 + 3*qx[1]
        }
      }
      if(sex=="M"){
        if(qx[1]>=0.1){
          ax[1] <- 0.33
        }else{
          ax[1] <- 0.0425 + 2.875*qx[1]
        }
      }
      ax[-1] <- n[-1]/2
      ax[m] <- last.ax
    }
  }
  px  <- 1-qx
  lx  <- cumprod(c(1,px))*100000
  dx  <- -diff(lx)
  Lx  <- n*lx[-1] + ax*dx
  lx <- lx[-(m+1)]
  Lx[m] <- lx[m]*last.ax
  Lx[is.na(Lx)] <- 0 ## in case of NA values
  Lx[is.infinite(Lx)] <- 0 ## in case of Inf values
  Tx  <- rev(cumsum(rev(Lx)))
  ex  <- Tx/lx
  return.df <- data.frame(x, n, ax, qx, px, lx, dx, Lx, Tx, ex)
  return(return.df)
}

### Gini-computation
Gini.fun <- function(LT){       
  LT$d1 <- LT$dx*(LT$x-30 + LT$ax)/LT$Tx[1]
  LT$d2 <- LT$dx/LT$lx[1]
  LT$Fx <- NA
  LT$Fx[1] <- LT$d2[1]
  LT$FFx[1] <- LT$d1[1]
  for(i in 2:(length(LT$Fx)-1)){
    LT$Fx[i] <- LT$Fx[i-1]+LT$d2[i]
    LT$FFx[i] <- LT$FFx[i-1]+LT$d1[i]
  }
  LT$Fx[length(LT$Fx)] <- 1
  LT$FFx[length(LT$Fx)] <- 1
  
  LT$Int <- NA
  for(i in 1:(length(LT$Fx)-1)){
    LT$Int[i] <- (LT$Fx[i+1]-LT$Fx[i])*(LT$FFx[i+1]+LT$FFx[i])
  }
  LT$Int[length(LT$Fx)] <- (LT$Fx[1])*(LT$FFx[1])                                # I put the first observation here
  
  Gini <- 1-sum(LT$Int)
  return(Gini)
}  

## x=agesA
## Nx=myE
## Dx=myD
## ax=rep(0.5,mA)
## ns=100
## level=0.90
## which.x = 31
## sex = "M"
## last.ax=0.5
## general function for getting CI of life expectancy at x
CIex <- function(x, Nx, Dx, sex = "M", ax = NULL,
                 which.x=0, ns=1000, level=0.95){
  ## point-estimated life-table
  LT <- lifetable(x, Nx, Dx, sex = sex, ax = ax)

  ### Gini-computation
  Gini <- Gini.fun(LT)
  
  ## number of ages
  m <- nrow(LT)
  ## estimated probs
  qx <- LT$qx
  ## trials for binomial, rounded
  Ntil <- round(Dx/qx)
  ## ax for last age
  last.ax <- LT$ax[m]
  ## simulated death counts
  ## from Binomial distribution
  Y <- suppressWarnings(matrix(rbinom(m*ns,
                                      Ntil,
                                      qx),
                               m,ns))
  ## simulated probabilities
  QX <- Y/Ntil
  ## which age?
  wh <- which(x==which.x)

  ## for all replicates, compute life expectancy
  ## ## by a for-loop
   exsimA <- rep(0,ns)
   exsimD <- rep(0,ns)
   
   for(s in 1:ns){
     ### Compute the Life table
     exs <-lifetable.qx(x, qx=QX[,s], sex,
                          last.ax=last.ax)
     ### Save the value for the age of interest
     exsimA[s] <- exs$ex[wh]
    
     ### Gini
     exsimD[s] <- Gini.fun(exs)
  }
  
  ## by apply command
  ## (slighly faster, less intuitive)
  ## fun.ex <- function(qx){
  ##  return(lifetable.qx(x=x, qx, sex=sex,
  ##                      last.ax=last.ax)$ex[wh])
  ##}
  ##exsim <- apply(QX, 2, fun.ex)

  ## confidence intervals
  CI_ex <- quantile(exsimA,
                 probs = c((1-level)/2,
                           1 - (1-level)/2))
  CI_Gini <- quantile(exsimD,
                    probs = c((1-level)/2,
                              1 - (1-level)/2))
  
  ## output
  out <- list(ex=LT$ex[wh],
              Gini=Gini,
              mean_ex=mean(exsimA),
              mean_Gini=mean(exsimD),
              CI_ex=CI_ex,
              CI_Gini=CI_Gini,
              exsimA=exsimA,
              exsimD=exsimD,
              which.x=which.x)
  return(out)
}
