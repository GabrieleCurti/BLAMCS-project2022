#Posterior distribution plots and analysis

#-------------------------------------------------------------------------------
#
# Install 'kyusque/DBDA2E-utilities' package by using:
#
# install.packages("remotes")
# remotes::install_github("kyusque/DBDA2E-utilities")
#
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
# package to install: 'ellipse'
#
# want = c("ellipse")
# have = want %in% rownames(installed.packages())
# if ( any(!have) ) { install.packages( want[!have] ) }
#-------------------------------------------------------------------------------

save(results, file='chains/modelSelection.dat')

# Load chains
load('chains/modelSelection.dat')

# Look at the structure of the output is an mcmc object of the library coda
str(results)

K <- 1
plot(results[,(4*K+1):(4*(K+1))])
summary(results)

# Convergence diagnostics:
parameterNames = varnames(results) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=results , parName=parName )
}

# Examine the posterior distribution:
mcmcMat = as.matrix(results)
chainLength = nrow(mcmcMat)
Nvar = ncol(X)
# Create subsequence of steps through chain for plotting:
stepVec = floor(seq(1,chainLength,length=20))

# Make plots of posterior distribution:


# Preparation -- define useful functions:
library(ellipse)
expandRange = function( x , exMult=0.2 ) {
  lowVal = min(x)
  highVal = max(x)
  wid = max(x)-min(x)
  return( c( lowVal - exMult*wid , highVal + exMult*wid ) )
}

openGraph = function( width=7 , height=7 , mag=1.0 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    tryInfo = try( X11( width=width*mag , height=height*mag , type="cairo" ,
                        ... ) )
    if ( class(tryInfo)=="try-error" ) {
      lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
      graphics.off()
      X11( width=width*mag , height=height*mag , type="cairo" , ... )
    }
  } else { # Windows OS
    tryInfo = try( windows( width=width*mag , height=height*mag , ... ) )
    if ( class(tryInfo)=="try-error" ) {
      lineInput = readline("WARNING: Previous graphics windows will be closed because of too many open windows.\nTO CONTINUE, PRESS <ENTER> IN R CONSOLE.\n")
      graphics.off()
      windows( width=width*mag , height=height*mag , ... )
    }
  }
}

plotPost = function( paramSampleVec , cenTend=c("mode","median","mean")[1] ,
                     compVal=NULL, ROPE=NULL, credMass=0.95, HDItextPlace=0.7,
                     xlab=NULL , xlim=NULL , yaxt=NULL , ylab=NULL ,
                     main=NULL , cex=NULL , cex.lab=NULL ,
                     col=NULL , border=NULL , showCurve=FALSE , breaks=NULL ,
                     ... ) {
  # Override defaults of hist function, if not specified by user:
  # (additional arguments "..." are passed to the hist function)
  if ( is.null(xlab) ) xlab="Param. Val."
  if ( is.null(cex.lab) ) cex.lab=1.5
  if ( is.null(cex) ) cex=1.4
  if ( is.null(xlim) ) xlim=range( c( compVal , ROPE , paramSampleVec ) )
  if ( is.null(main) ) main=""
  if ( is.null(yaxt) ) yaxt="n"
  if ( is.null(ylab) ) ylab=""
  if ( is.null(col) ) col="skyblue"
  if ( is.null(border) ) border="white"
  
  # convert coda object to matrix:
  if ( class(paramSampleVec) == "mcmc.list" ) {
    paramSampleVec = as.matrix(paramSampleVec)
  }
  
  summaryColNames = c("ESS","mean","median","mode",
                      "hdiMass","hdiLow","hdiHigh",
                      "compVal","pGtCompVal",
                      "ROPElow","ROPEhigh","pLtROPE","pInROPE","pGtROPE")
  postSummary = matrix( NA , nrow=1 , ncol=length(summaryColNames) ,
                        dimnames=list( c( xlab ) , summaryColNames ) )
  
  # require(coda) # for effectiveSize function
  postSummary[,"ESS"] = effectiveSize(paramSampleVec)
  
  postSummary[,"mean"] = mean(paramSampleVec)
  postSummary[,"median"] = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  postSummary[,"mode"] = mcmcDensity$x[which.max(mcmcDensity$y)]
  
  HDI = HDIofMCMC( paramSampleVec , credMass )
  postSummary[,"hdiMass"]=credMass
  postSummary[,"hdiLow"]=HDI[1]
  postSummary[,"hdiHigh"]=HDI[2]
  
  # Plot histogram.
  cvCol = "darkgreen"
  ropeCol = "darkred"
  if ( is.null(breaks) ) {
    if ( max(paramSampleVec) > min(paramSampleVec) ) {
      breaks = c( seq( from=min(paramSampleVec) , to=max(paramSampleVec) ,
                       by=(HDI[2]-HDI[1])/18 ) , max(paramSampleVec) )
    } else {
      breaks=c(min(paramSampleVec)-1.0E-6,max(paramSampleVec)+1.0E-6)
      border="skyblue"
    }
  }
  if ( !showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , xlab=xlab , yaxt=yaxt , ylab=ylab ,
                     freq=F , border=border , col=col ,
                     xlim=xlim , main=main , cex=cex , cex.lab=cex.lab ,
                     breaks=breaks , ... )
  }
  if ( showCurve ) {
    par(xpd=NA)
    histinfo = hist( paramSampleVec , plot=F )
    densCurve = density( paramSampleVec , adjust=2 )
    plot( densCurve$x , densCurve$y , type="l" , lwd=5 , col=col , bty="n" ,
          xlim=xlim , xlab=xlab , yaxt=yaxt , ylab=ylab ,
          main=main , cex=cex , cex.lab=cex.lab , ... )
  }
  cenTendHt = 0.9*max(histinfo$density)
  cvHt = 0.7*max(histinfo$density)
  ROPEtextHt = 0.55*max(histinfo$density)
  # Display central tendency:
  mn = mean(paramSampleVec)
  med = median(paramSampleVec)
  mcmcDensity = density(paramSampleVec)
  mo = mcmcDensity$x[which.max(mcmcDensity$y)]
  if ( cenTend=="mode" ){
    text( mo , cenTendHt ,
          bquote(mode==.(signif(mo,3))) , adj=c(.5,0) , cex=cex )
  }
  if ( cenTend=="median" ){
    text( med , cenTendHt ,
          bquote(median==.(signif(med,3))) , adj=c(.5,0) , cex=cex , col=cvCol )
  }
  if ( cenTend=="mean" ){
    text( mn , cenTendHt ,
          bquote(mean==.(signif(mn,3))) , adj=c(.5,0) , cex=cex )
  }
  # Display the comparison value.
  if ( !is.null( compVal ) ) {
    pGtCompVal = sum( paramSampleVec > compVal ) / length( paramSampleVec )
    pLtCompVal = 1 - pGtCompVal
    lines( c(compVal,compVal) , c(0.96*cvHt,0) ,
           lty="dotted" , lwd=2 , col=cvCol )
    text( compVal , cvHt ,
          bquote( .(round(100*pLtCompVal,1)) * "% < " *
                    .(signif(compVal,3)) * " < " *
                    .(round(100*pGtCompVal,1)) * "%" ) ,
          adj=c(pLtCompVal,0) , cex=0.8*cex , col=cvCol )
    postSummary[,"compVal"] = compVal
    postSummary[,"pGtCompVal"] = pGtCompVal
  }
  # Display the ROPE.
  if ( !is.null( ROPE ) ) {
    pInROPE = ( sum( paramSampleVec > ROPE[1] & paramSampleVec < ROPE[2] )
                / length( paramSampleVec ) )
    pGtROPE = ( sum( paramSampleVec >= ROPE[2] ) / length( paramSampleVec ) )
    pLtROPE = ( sum( paramSampleVec <= ROPE[1] ) / length( paramSampleVec ) )
    lines( c(ROPE[1],ROPE[1]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol )
    lines( c(ROPE[2],ROPE[2]) , c(0.96*ROPEtextHt,0) , lty="dotted" , lwd=2 ,
           col=ropeCol)
    text( mean(ROPE) , ROPEtextHt ,
          bquote( .(round(100*pLtROPE,1)) * "% < " * .(ROPE[1]) * " < " *
                    .(round(100*pInROPE,1)) * "% < " * .(ROPE[2]) * " < " *
                    .(round(100*pGtROPE,1)) * "%" ) ,
          adj=c(pLtROPE+.5*pInROPE,0) , cex=1 , col=ropeCol )
    
    postSummary[,"ROPElow"]=ROPE[1]
    postSummary[,"ROPEhigh"]=ROPE[2]
    postSummary[,"pLtROPE"]=pLtROPE
    postSummary[,"pInROPE"]=pInROPE
    postSummary[,"pGtROPE"]=pGtROPE
  }
  # Display the HDI.
  lines( HDI , c(0,0) , lwd=4 , lend=1 )
  text( mean(HDI) , 0 , bquote(.(100*credMass) * "% HDI" ) ,
        adj=c(.5,-1.7) , cex=cex )
  text( HDI[1] , 0 , bquote(.(signif(HDI[1],3))) ,
        adj=c(HDItextPlace,-0.5) , cex=cex )
  text( HDI[2] , 0 , bquote(.(signif(HDI[2],3))) ,
        adj=c(1.0-HDItextPlace,-0.5) , cex=cex )
  par(xpd=F)
  #
  return( postSummary )
}


DbdaAcfPlot = function( codaObject , parName=varnames(codaObject)[1] , plColors=NULL ) {
  if ( all( parName != varnames(codaObject) ) ) {
    stop("parName must be a column name of coda object")
  }
  nChain = length(codaObject)
  if ( is.null(plColors) ) plColors=1:nChain
  xMat = NULL
  yMat = NULL
  for ( cIdx in 1:nChain ) {
    acfInfo = acf(codaObject[,c(parName)][[cIdx]],plot=FALSE)
    xMat = cbind(xMat,acfInfo$lag)
    yMat = cbind(yMat,acfInfo$acf)
  }
  matplot( xMat , yMat , type="o" , pch=20 , col=plColors , ylim=c(0,1) ,
           main="" , xlab="Lag" , ylab="Autocorrelation" )
  abline(h=0,lty="dashed")
  EffChnLngth = effectiveSize(codaObject[,c(parName)])
  text( x=max(xMat) , y=max(yMat) , adj=c(1.0,1.0) , cex=1.25 ,
        labels=paste("ESS =",round(EffChnLngth,1)) )
}

HDIofMCMC = function( sampleVec , credMass=0.95 ) {
  sortedPts = sort( sampleVec )
  ciIdxInc = ceiling( credMass * length( sortedPts ) )
  nCIs = length( sortedPts ) - ciIdxInc
  ciWidth = rep( 0 , nCIs )
  for ( i in 1:nCIs ) {
    ciWidth[ i ] = sortedPts[ i + ciIdxInc ] - sortedPts[ i ]
  }
  HDImin = sortedPts[ which.min( ciWidth ) ]
  HDImax = sortedPts[ which.min( ciWidth ) + ciIdxInc ]
  HDIlim = c( HDImin , HDImax )
  return( HDIlim )
}

DbdaDensPlot = function( codaObject , parName=varnames(codaObject)[1] , plColors=NULL ) {
  if ( all( parName != varnames(codaObject) ) ) {
    stop("parName must be a column name of coda object")
  }
  nChain = length(codaObject) # or nchain(codaObject)
  if ( is.null(plColors) ) plColors=1:nChain
  xMat = NULL
  yMat = NULL
  hdiLims = NULL
  for ( cIdx in 1:nChain ) {
    densInfo = density(codaObject[,c(parName)][[cIdx]])
    xMat = cbind(xMat,densInfo$x)
    yMat = cbind(yMat,densInfo$y)
    hdiLims = cbind(hdiLims,HDIofMCMC(codaObject[,c(parName)][[cIdx]]))
  }
  matplot( xMat , yMat , type="l" , col=plColors ,
           main="" , xlab="Param. Value" , ylab="Density" )
  abline(h=0)
  points( hdiLims[1,] , rep(0,nChain) , col=plColors , pch="|" )
  points( hdiLims[2,] , rep(0,nChain) , col=plColors , pch="|" )
  text( mean(hdiLims) , 0 , "95% HDI" , adj=c(0.5,-0.2) )
  EffChnLngth = effectiveSize(codaObject[,c(parName)])
  MCSE = sd(as.matrix(codaObject[,c(parName)]))/sqrt(EffChnLngth)
  text( max(xMat) , max(yMat) , adj=c(1.0,1.0) , cex=1.25 ,
        paste("MCSE =\n",signif(MCSE,3)) )
}


diagMCMC = function( codaObject , parName=varnames(codaObject)[1] ,
                     saveName=NULL , saveType="jpg" ) {
  DBDAplColors = c("skyblue","black","royalblue","steelblue")
  openGraph(height=5,width=7)
  par( mar=0.5+c(3,4,1,0) , oma=0.1+c(0,0,2,0) , mgp=c(2.25,0.7,0) ,
       cex.lab=1.5 )
  layout(matrix(1:4,nrow=2))
  # traceplot and gelman.plot are from CODA package:
  require(coda)
  coda::traceplot( codaObject[,c(parName)] , main="" , ylab="Param. Value" ,
                   col=DBDAplColors )
  tryVal = try(
    coda::gelman.plot( codaObject[,c(parName)] , main="" , auto.layout=FALSE ,
                       col=DBDAplColors )
  )
  # if it runs, gelman.plot returns a list with finite shrink values:
  if ( class(tryVal)=="try-error" ) {
    plot.new()
    print(paste0("Warning: coda::gelman.plot fails for ",parName))
  } else {
    if ( class(tryVal)=="list" & !is.finite(tryVal$shrink[1]) ) {
      plot.new()
      print(paste0("Warning: coda::gelman.plot fails for ",parName))
    }
  }
  DbdaAcfPlot(codaObject,parName,plColors=DBDAplColors)
  DbdaDensPlot(codaObject,parName,plColors=DBDAplColors)
  mtext( text=parName , outer=TRUE , adj=c(0.5,0.5) , cex=2.0 )
  if ( !is.null(saveName) ) {
    saveGraph( file=paste0(saveName,"Diag",parName), type=saveType)
  }
}

for ( varIdx in 1:Nvar ) {
  openGraph(width=7,height=3.5)
  par( mar=c(3.5,3,2,1) , mgp=c(2.0,0.7,0) )
  layout(matrix(1:2,nrow=1))
  # Marginal posterior on means:
  plotPost( mcmcMat[ , paste0("mu[",varIdx,"]") ] ,
            xlab=paste0("mu[",varIdx,"]") ,
            main=paste( "Mean of" , colnames(y)[varIdx] ) )
  # Marginal posterior on standard deviations:
  plotPost( mcmcMat[ , paste0("sigma[",varIdx,"]") ] ,
            xlab=paste0("sigma[",varIdx,"]") ,
            main=paste( "SD of" , colnames(y)[varIdx] ) )
}

for ( varIdx1 in 1:(Nvar-1) ) {
  for ( varIdx2 in (varIdx1+1):Nvar ) {
    openGraph(width=7,height=3.5)
    par( mar=c(3.5,3,2,1) , mgp=c(2.0,0.7,0) )
    layout(matrix(1:2,nrow=1))
    # Marginal posterior on correlation coefficient
    plotPost( mcmcMat[ , paste0("rho[",varIdx1,",",varIdx2,"]") ] ,
              xlab=paste0("rho[",varIdx1,",",varIdx2,"]") ,
              main=paste( "Corr. of" , colnames(y)[varIdx1] ,
                          "and" , colnames(y)[varIdx2] ) )
    # Data with posterior ellipse
    ellipseLevel = 0.90
    plot( y[,c(varIdx1,varIdx2)] , # pch=19 ,
          xlim=expandRange(y[,varIdx1]) , ylim=expandRange(y[,varIdx2]) ,
          xlab=colnames(y)[varIdx1] , ylab=colnames(y)[varIdx2] ,
          main=bquote("Data with posterior "*.(ellipseLevel)*" level contour") )
    # Posterior ellipses:
    for ( stepIdx in stepVec ) {
      points( ellipse( mcmcMat[ stepIdx ,
                                paste0("rho[",varIdx1,",",varIdx2,"]") ] ,
                       scale=mcmcMat[ stepIdx ,
                                      c( paste0("sigma[",varIdx1,"]") ,
                                         paste0("sigma[",varIdx2,"]") ) ] ,
                       centre=mcmcMat[ stepIdx ,
                                       c( paste0("mu[",varIdx1,"]") ,
                                          paste0("mu[",varIdx2,"]") ) ] ,
                       level=ellipseLevel ) ,
              type="l" , col="skyblue" , lwd=1 )
    }
    # replot data:
    points( y[,c(varIdx1,varIdx2)] )
  }
}

# Show data descriptives on console:
cor( X )
apply(X,2,mean)
apply(X,2,sd)

#-----------------------------------------------------------------------------
# Difference of correlations. Change the specification of indices as
# appropriate to your variable names and interests.
#-----------------------------------------------------------------------------
# Specify indices of desired correlations:
rhoAidx1 = which(colnames(X)=="year")
rhoAidx2 = which(colnames(X)=="mileage")
rhoBidx1 = which(colnames(X)=="tax")
rhoBidx2 = which(colnames(X)=="engineSize")
# Make plot of posterior difference:
openGraph(height=3.5,width=4)
par( mar=c(3.5,1,2,1) , mgp=c(2.0,0.7,0) )
plotPost( mcmcMat[ , paste0("rho[",rhoAidx1,",",rhoAidx2,"]") ]
          - mcmcMat[ , paste0("rho[",rhoBidx1,",",rhoBidx2,"]") ],
          xlab=paste0(
            "rho[",rhoAidx1,",",rhoAidx2,"] - rho[",rhoBidx1,",",rhoBidx2,"]" ) ,
          main=bquote( rho[ list( .(colnames(X)[rhoAidx1]) ,
                                  .(colnames(X)[rhoAidx2]) ) ] 
                       - rho[ list( .(colnames(X)[rhoBidx1]) ,
                                    .(colnames(X)[rhoBidx2]) ) ] ) ,
          cex.main=1.5 , ROPE=c(-0.1,0.1) )