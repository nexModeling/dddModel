#' Get and process the parameters
#'
#' Get and process the parameters from a R-source file.
#' Two files are required:
#' - one for the calibrated parameters (paramCalibrated.R)
#' - one for the GIS-related parameters (paramGIS.R)
#' The files are present in the data folder of this package.
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#' @param path path of the directory where to find the two files
#' @return The output is a list of processed parameters to be build the model
#' @keywords model
#' @export
#' @examples
#' \dontrun{
#' getParam()
#' }
getParam <- function(Timeresinsec,path) {

  source(paste0(path,"paramCalibrated.R"),local=TRUE)
  source(paste0(path,"paramGIS.R"),local=TRUE)

  # Process some paramaters
  hfeltmid <- hfelt
  hfelt <- rep(0,nbLevelZone)
  for (i in 1:nbLevelZone) {
     hfelt[i] <-a00[(1+(i-1))]+(a00[(2+(i-1))]-a00[(1+(i-1))])/2
  }

  # parameters with different names
  totarea <-area      # total area in m2
  dummy3 <- Dummy     #Celertity of lake NOT USED

  tprm <- c(CX,TS,TX,pkorr, pro, tgrad, pgrad, cea) #parameters of your choosing to be written to the R2 file

  #Middelhoyde metstasjoner
  midmetp <- (hst1*vp1)+(hst2*vp2)
  midmett <- (hst1*vt1)+(hst2*vt2)

  #Constants
  lpdel <- 0.8 # ??

  nobognoglacarea <-(1-(bogfrac+glacfrac))*totarea    #iarea without bog area and glaciated area neither
  slopesriverarea <- (1-(bogfrac))*totarea            #area in which we have hillslope process and rivernetwork processes
  bogarea         <- bogfrac*totarea                  #bog area
  glacarea        <-glacfrac *totarea                 #glaciated area
  elevarea        <- (totarea/nbLevelZone)                   #area for each level zone

  gca <- c(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10)            # fraction of glacier pr elevation zone, fraction of glacier covered area pr elevation zone
  soilca <-1-gca

  #determines weights used to estimate the average glaciermelt. Finds the fraction of glaciers in each elevation zone in relation to total glacierarea
  # gwgt is the fraction of glaciated area pr elevation zone in relation to total glaciated area
  if(glacarea>0) {
    gwgt <-gca*elevarea/glacarea
  } else gwgt <- rep(0,nbLevelZone)

  swgt <-soilca*elevarea/(area + bogarea) #Finds the fraction of soils (and bogs) in each elevation zone in relation to total soil (and bog) area

  mLam       <- GshInt*GscInt
  varLam     <- GshInt*(GscInt)^2 #Yevjevich p.145
  cvLam      <- varLam^0.5/mLam
  meanIntk   <- mLam*midDL/Timeresinsec #middel hastighet beregent med Integrated Celerity

  unitsnow <- 0.1

  res <- list(nbLevelZone=nbLevelZone, # BEGIN claibrated param file
             Ws =Ws,  # pro
             Tlr=Tlr, # tgrad
             Plr=Plr, # pgrad
             Pc =Pc,  # pkorr
             Sc=Sc,   # skorr
             TX=TX,
             TS=TS,
             CX=CX,
             CGLAC=CGLAC,
             CFR=CFR,
             NoL=NoL,
             cea=cea,
             R=R,
             Gsh=Gsh, # Gshape
             Gsc=Gsc, # Gscale
             gtcel=gtcel,
             GshInt=GshInt,
             GscInt=GscInt,
             CV=CV,   # cvHBV
             a0=a0,
             d=d,
             Dummy=Dummy,
             rv=rv,
             Timeresinsec=Timeresinsec,
             MAD=MAD,
             totdef=totdef,
             D = D,
             UP = UP,# END claibrated param file
             hfelt=hfelt, # BEGIN gis param file
             maxLbog=maxLbog,
             midLbog=midLbog,
             bogfrac=bogfrac,
             zsoil=zsoil,
             zbog=zbog,
             midFL=midFL,
             stdFL=stdFL,
             maxFL=maxFL,
             maxDL=maxDL,
             midDL=midDL,
             glacfrac=glacfrac,
             midGl=midGl,
             stdGl=stdGl,
             maxGl=maxGl, # END gis param file
             hfeltmid=hfeltmid,
             totarea=totarea,
             dummy3=dummy3,
             tprm=tprm,
             midmetp=midmetp,
             midmett=midmett,
             lpdel=lpdel, #!!!!!
             nobognoglacarea=nobognoglacarea,
             slopesriverarea=slopesriverarea,
             bogarea=bogarea,
             glacarea=glacarea,
             elevarea=elevarea,
             gca=gca,
             soilca=soilca,
             gwgt=gwgt,
             swgt=swgt,
             mLam=mLam,
             varLam=varLam,
             cvLam=cvLam,
             meanIntk=meanIntk,
             unitsnow=unitsnow)

  return(res)
}
