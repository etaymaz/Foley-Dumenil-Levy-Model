# Foley-Dumenil-Levy Firm Model
#
# Version 1.0 by
# Erol Taymaz
# Department of Economics
# Middle East Technical University
# Ankara Turkey
# github.com/etaymaz
# 24 July 2015

# Load libraries
library(data.table)
library(ggplot2)


# MODEL
run <- function(ww=0.7, nFirm=10, nIter=1000, setSeed=FALSE) {
  # Run the model
  # ww is the initial value of the wage rate - nominal and real becasue P=1 is constant

  # Set parameters
  setPara(setSeed)

  # Initialize model variables
  initializeMV(ww, nFirm)
  
  # Initialize data collection variables
  initializeDCV(nIter)
  
  # Simulate the model
  for (nTime in c(2:nIter)) iterate(nTime)
  
  # Plot graphics
  pbase()
  }
  
setPara <- function(setSeed=FALSE) {
  # Parameter setting function
  if (setSeed==TRUE) set.seed(123)

    searchTech   <<-  0.025,           # Search span for new technologies
    biasL        <<- -0.005,           # Rate of labor-saving bias
    biasK        <<- -0.005,           # Rate of capital-saving bias
    d            <<- 0.08,             # Depreciation rate
    wrate        <<- 0.03,              # Unemployment wage effect
    wratemax     <<- 0.005,             # Full employment wage increase
    popGr        <<- 0.01,             # Population growth rate
    Beta         <<- 0.5               # Capitalists' investment tendency
  }

initializeMV <- function(ww, nFirm) {
  # Initialize the model
  fData <<-data.table(id=c(1:nf))
  fData[, A := rep(1, nFirm)]                      # Capital productivity, Q/K
  fData[, L := rep(1, nFirm)]                      # Labor productivity, Q/N
  fData[, K := rep(1, nFirm)]                      # Capital stock
  fData[, N := rep(1, nFirm)]                      # Employment
  fData[, Q := pmin(A*K, L*N)]                     # Output
  fData[, w := rep(ww, nFirm)]                     # Wage rate (nominal)
  P <<- 1                                          # Product price
  Pop <<- 1.02*sum(fData$N)                        # Population
  }

initializeDCV <- function(nIter) {
  # Data collection variables
  nd <- rep(0, nIter)
  sData <<- data.table(matrix(0, nrow=nIter, ncol=7))
  names(sData) <- c("A", "L", "K", "Q", "N", "w", "Po")
  sData[1,A := mean(fData$A)]
  sData[1,L := mean(fData$L)]
  sData[1,K := sum(fData$K)]
  sData[1,Q := sum(fData$Q)]
  sData[1,N := sum(fData$N)]
  sData[1,w := mean(fData$w)]
  sData[1,Po := Pop]
  aa <<- 0
  ll <<- 0
  }  

iterate <- function(nTime) {
  # Iterations
  Kmax <- Pop*L/A                               # Max capital
  I <- ifelse(sum(K)<Kmax, Beta*(P*Q-w*N)/P, 0)      # Investment
  C <- Q - I                                    # Consumption

  nT <- newTech(nFirm, searchTech, biasL, biasK, nTime)
  lChange <- nT[,2]/L
  A <<- nT[,1]
  L <<- nT[,2]
  K <<- (1-d)*K + I

  Ndes <- A*K/L
  N <<- pmin(Ndes, Ndes*(Pop/sum(Ndes)))
  # cat(N, "\n")
  Q <<- pmin(A*K, L*N)
  # cat(sum(Ndes), " ", sum(N), "", Pop, "\n")
  # w <<- w*(1+max(wratemax + wrate*log(sum(N)/Pop), 0))   # Flexible wage share model
  # w <<- w*mean(lChange)                                # Constant wage share model
  w <<- w*lChange                                      # Constant firm-level wage share model
  Pop <<- Pop*(1+popGr)


  AllA[nTime] <<- sum(A*K)/sum(K)
  AllL[nTime] <<- sum(L*N)/sum(N)
  AllK[nTime] <<- sum(K)
  AllQ[nTime] <<- sum(Q)
  AllN[nTime] <<- sum(N)
  Allw[nTime] <<- sum(w*N)/sum(N)
  AllPop[nTime] <<- Pop
  }

newTech <- function(n=1, sT=.1, bL=0, bK=0, nTime) {
  # Adopt new technology
  tt <- genTech(n, sT, bL, bK)          # Generate new technology
  l <- tt[,1]                           # Growth rate of L-productivity
  a <- tt[,2]                           # Growth rate of K-productivity
  mu <- (1 - w/L) / (w/L)               # Rate of surplus value
  selFront <- mu*a + l                  # Selection frontier
  newA <- A
  newL <- L
  innova <- selFront>=0
  newA[innova] <- (A*(1 + a))[innova]
  newL[innova] <- (L*(1 + a))[innova]

  aa[nTime+1] <<- mean(a[innova])
  ll[nTime+1] <<- mean(l[innova])
  
  return(matrix(c(newA, newL),nrow=nFirm, ncol=2))
  }

genTech <- function(nn=1, searchTech=0.1, biasL=0, biasK=0) {
  # Generate new technologies
  rad <- runif(nn, 0, searchTech)
  deg <- 2*pi*runif(nn)
  l   <- biasL + (sin(deg)*rad)
  a   <- biasK + (cos(deg)*rad)
  return(matrix(c(l,a), nrow=nn, ncol=2))
  }

# GRAPHICS FUNCTIONS
pbase <- function() {
  # Create a layout for plots
  # Four charts will be plotted on the same page
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  plot(log(Allw), type="l", main="Wages", xlab="Time", ylab="Wage rate (log)")
  plot(AllN/AllPop, type="l", ylim=c(0, max(AllN/AllPop)),main="Employment rate", xlab="Time", ylab="Employment rate")
  plot(AllA, AllL, type="l", main="Technological change", xlab="Capital productivity", ylab="Labor productivity")
  plot(log(AllQ), type="l", main="Output", xlab="Time", ylab="Output (log)")
 }

pgr <- function() {
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  plot(ma(growth(Allw),50), type="l", main="Wage growth", xlab="Time", ylab="Wage growth")
  plot(ma(growth(AllN),50), type="l", main="Employment growth", xlab="Time", ylab="Employment growth")
  plot(ma(growth(AllQ),50), type="l", main="Output growth", xlab="Time", ylab="Output growth")
  plot(ma(growth(AllK),50), type="l", main="Capital stock growth", xlab="Time", ylab="Capital stock growth")
  }

ptech <- function() {
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  plot(aa,ll, main="Bias in technical change", xlab="Captial saving", ylab="Labor saving")
  plot(Allw*AllN/AllQ, type="l", main="Wage share", xlab="Time", ylab="Wage share in total output")
  plot(ma(growth(AllL),50), type="l", main="Labor productivity growth", xlab="Time", ylab="Labor productivity growth")
  plot(ma(growth(AllA),50), type="l", main="Capital productivity growth", xlab="Time", ylab="Capital productivity growth")
  }

# Additional auxiliary functions
growth <- function(a) {
  n <- length(a)
  g <- 100*log(a[c(2:n)]/a[1:(n-1)])
  return(g)
  }

ma <- function(x, n) {
  r <- x
  if (n > 0) {
    s <- length(x)
    r <- rep(0, s+1-n)
    for (i in c(1:n)) r <- r + x[c(i:(s+i-n))]
    r <- r/n
    }
  return(r)
  }

monte <- function(times=100) {
  # Monte-Carlo simulations
  # Replace "output" with any variable that will be analyzed
  Output <<- 0
  for (k in c(1:times)) {
    winit <<- .1 + .008*k
    run(winit, 1000)
    Output[k] <<- AllQ[times]
    }
  plot(Output, type="l")
  }

