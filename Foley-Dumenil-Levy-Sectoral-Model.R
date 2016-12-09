# Foley-Dumenil-Levy Sectoral Model
#
# Version 1.0 by
# Erol Taymaz
# Department of Economics
# Middle East Technical University
# Ankara Turkey
# www.metu.edu.tr/~etaymaz/nwm
# 22 September 2011

# DEBUGGING OPTIONS
# Error recover option 
# options(error=recover)
# Convert warnings to errors
# options(warn=2)

# MODEL
run <- function(ww=0.6, nIter=1000, setSeed=FALSE) {
  # Run the model
  # ww is the initial value of the wage rate - nominal and real becasue P=1 is constant
  # Set parameters
  param <<- setPara(setSeed)

  # Attach param list so that each paremeter will be used by its name
  attach(param)
  
  # Initialize model variables
  initializeMV(ww)
  
  # Initialize data collection variables
  initializeDCV()
  
  # Simulate the model
  for (nTime in c(2:nIter)) iterate(nTime)
  
  # Detach parameter list
  detach(param)
  
  # Plot graphics
  pbase()
  }
  
setPara <- function(setSeed=FALSE) {
  # Parameter setting function
  if (setSeed==TRUE) set.seed(123)
  parameters <- list(
    searchTech   =  0.02,            # Search span for new technologies
    biasL        = -0.008,           # Rate of labor-saving bias
    biasK        = -0.008,           # Rate of capital-saving bias
    d            = 0.07,             # Depreciation rate
    wrate        = 0.05,             # Unemployment wage effect
    wratemax     = 0.002,            # Full employment wage increase
    popGr        = 0.01,             # Population growth rate
    Beta         = 0.7               # Capitalists' investment tendency
    )
    return(parameters)
  }

initializeMV <- function(ww) {
  # Initialize the model
  A <<- .5                     # Capital productivity, Q/K
  L <<- 1                      # Labor productivity, Q/N
  K <<- 1/A                    # Capital stock
  N <<- 1                      # Employment
  Q <<- min(A*K, L*N)          # Output
  w <<- ww                     # Wage rate (nominal)
  P <<- 1                      # Product price
  Pop <<- 1.02                 # Population
  }

initializeDCV <- function() {
  # Data collection variables
  AllA <<- A
  AllL <<- L
  AllK <<- K
  AllQ <<- Q
  AllN <<- N
  Allw <<- w
  AllPop <<- Pop
  aa <<- 0
  ll <<- 0
  }  

iterate <- function(nTime) {
  # Iterations
  # New technologies
  nT <- newTech(1, searchTech, biasL, biasK, nTime)
  lChange <- nT[2]/L
  A <<- nT[1]
  L <<- nT[2]
  # Capital stock
  Kmax <- Pop*L/A                               # Max capital
  I <- Beta*(P*Q-w*N)/P                         # Investment
  C <- Q - I                                    # Consumption (not used anywhere)
  K <<- pmin((1-d)*K + I, Kmax)
  # Employment
  Ndes <- A*K/L
  N <<- min(Ndes, Pop)
  # Output
  Q <<- min(A*K, L*N)
  # Wages - there are two different spec
  w <<- w*(1+max(wratemax + wrate*log(N/Pop), 0))   # Flexible wage share model
  # w <<- w*lChange                                 # Constant wage share model
  # Population update
  Pop <<- Pop*(1+popGr)
  # Data collection
  AllA[nTime] <<- A
  AllL[nTime] <<- L
  AllK[nTime] <<- K
  AllQ[nTime] <<- Q
  AllN[nTime] <<- N
  Allw[nTime] <<- w
  AllPop[nTime] <<- Pop
  }

newTech <- function(n=1, sT=.1, bL=0, bK=0, nTime) {
  # Adopt new technology
  tt <- genTech(n, sT, bL, bK)          # Generate new technology
  l <- tt[,1]                           # Growth rate of L-productivity
  a <- tt[,2]                           # Growth rate of K-productivity
  mu <- (1 - w/L) / (w/L)               # Rate of surplus value
  selFront <- mu*a + l                  # Selection frontier
  if (selFront>=0) {
    newA <- A*(1 + a)
    newL <- L*(1 + l)
    aa[nTime+1] <<- a
    ll[nTime+1] <<- l
    }
    else {
      newA <- A
      newL <- L
      aa[nTime+1] <<- 0
      ll[nTime+1] <<- 0
      }
  return(c(newA, newL))
  }

genTech <- function(nn=1, searchTech=0.1, biasL=0, biasK=0) {
  # Generate new technologies
  rad <- runif(nn, 0, 1)
  rad <- sqrt(rad)*searchTech
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

monte <- function(times=110) {
  # Monte-Carlo simulations
  # Replace "output" with any variable that will be analyzed
  Output <<- 0
  for (k in c(1:times)) {
    winit <<- .1 + .008*k
    run(winit, 1000)
    Output[k] <<- Q
    }
  plot(pmax(0,log(Output)), type="l")
  }


pCh <- function(x1,x2,x3,x4) {
  layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
  plot(x1, type="l")
  plot(x2, type="l")
  plot(x3, type="l")
  plot(x4, type="l")
  }
