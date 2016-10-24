#' Get the parameters
#'
#' Get and process, if needed, the parameters to build the ddd models.
#' @param method method for getting parameters, "manual", "load", "processedNVE"
#' @param path Directory where to get the files
#' @param filename Filename (path included) of the file to be read
#' @param SAVE Save the results, Boolean
#' @param pathResults Path of the results. By default: $HOME
#' @param Timeresinsec Time resolution of the process in second (1hour: 3600s, ... etc)
#' @param catchment Name of the catchment as string of character
#' @param nbLevelZone Number of level zone
#' @param Ws Max Liquid water content in snow. Percentage of Snow water equivalent, SWE
#' @param Tlr Temperature gradient. Degrees pr 100 meters
#' @param Plr Precipitation gradient. Fraction pr 100 meters
#' @param Pc Liquid precpititan correction
#' @param Sc Solid precpititan correction
#' @param TX Threshold temp for snow/rain
#' @param TS Threshold temp for snowmelt
#' @param CX Degree day factor for snowmelt. Unit: mm/degree/day
#' @param CGLAC Degree day factor for glacial melt
#' @param CFR Factor for refreezing. Unit:mm/degree/day
#' @param NoL Number of saturation layers
#' @param cea Degree day factor for evpotranspiration. Unit:mm/degree/day
#' @param R Ratio defining field capacity: fracion of D
#' @param Gsh Shape parameter of the Gamma distribution of celerities
#' @param Gsc Scale parameter of the Gamma distribution of celerities
#' @param gtcel Quantile in celerity distribution for overland flow
#' @param GshInt Shape pararameter of the Gamma distribution of Lambda
#' @param GscInt Scale pararameter of the Gamma distribution of Lambda
#' @param CV CV for log normal snow distribution
#' @param a0 Scale parameter of unit precipitation.
#' @param d Decorrelation length of spatial precipitation.
#' @param rv Celerity for river flow. Unit: m/s
#' @param MAD Mean Annual Discharge
#' @param hfelt Mean elevation of catchment
#' @param maxLbog Max distance bogs-river
#' @param midLbog Mean distance bogs-river
#' @param bogfrac Areal fraction of bogs
#' @param zsoil arealfraction of DD for soils
#' @param zbog arealfraction of DD for bogs
#' @param midFL Mean distance from the river network
#' @param stdFL Standard deviation of distance from the river network
#' @param maxFL Max distance from the river network
#' @param maxDL Max distance of soil point to the river network
#' @param midDL Mean distance  of soil point to the river network
#' @param glacfrac Glacier fraction
#' @param midGl Mean distance of glacier point to the river network
#' @param stdGl Standard deviation of distance of glacier point to the river network
#' @param maxGl Max distance of glacier point to the river network
#' @param hfeltmid Mean elevation of catchment
#' @param totarea Area in m2
#' @param midmetp Mean elevation precipitation meteorological station
#' @param midmett Mean elevation temperature meteorological station
#' @param nobognoglacarea Area without bog area and glaciated area neither in m2
#' @param slopesriverarea Area in which we have hillslope process and rivernetwork processes. Unit:m2
#' @param bogarea Bog area. Unit: in m2
#' @param glacarea Glaciated area. Unit: m2
#' @param elevarea Area for each level zone. Unit: m2
#' @param gca fraction of glacier pr elevation zone, fraction of glacier covered area pr elevation zone
#' @param soilca 1-gca
#' @param gwgt Fraction of glaciated area pr elevation zone in relation to total glaciated area
#' @param swgt Fraction of soils and bogs in each elevation zone in relation to total soil and bog area
#' @param mLam mean Lambda
#' @param varLam variance Lambda
#' @param cvLam CV Lambda
#' @param meanIntk Average speed calculated with Integrated Celerity
#' @param unitsnow Snow unit
#' @return The output is a list of processed parameters to build models
#' @keywords model
#' @export
#' @examples
#' \dontrun{
#' getParam()
#' }
getParam <- function(method=NULL,path=NULL,filename=NULL,SAVE=FALSE,pathResults="~/",Timeresinsec=NULL,catchment=NULL,
                     nbLevelZone=NULL,Ws=NULL,Tlr=NULL,Plr=NULL,Pc=NULL,Sc=NULL,
                     TX=NULL,TS=NULL,CX=NULL,CGLAC=NULL,CFR=NULL,NoL=NULL,cea=NULL,
                     R=NULL,Gsh=NULL,Gsc=NULL,gtcel=NULL,GshInt=NULL,GscInt=NULL,
                     CV=NULL,a0=NULL,d=NULL,rv=NULL,
                     MAD=NULL,hfelt=NULL,maxLbog=NULL,midLbog=NULL,
                     bogfrac=NULL,zsoil=NULL,zbog=NULL,midFL=NULL,
                     stdFL=NULL,maxFL=NULL,maxDL=NULL,midDL=NULL,glacfrac=NULL,midGl=NULL,
                     stdGl=NULL,maxGl=NULL,hfeltmid=NULL,totarea=NULL,
                     midmetp=NULL,midmett=NULL,nobognoglacarea=NULL,
                     slopesriverarea=NULL,bogarea=NULL,glacarea=NULL,elevarea=NULL,gca=NULL,
                     soilca=NULL,gwgt=NULL,swgt=NULL,mLam=NULL,varLam=NULL,cvLam=NULL,meanIntk=NULL,
                     unitsnow=NULL) {

    inputParam <- switch(method,
                   "manual"        = getParam.manual(catchment=catchment,nbLevelZone=nbLevelZone,Ws=Ws,Tlr=Tlr,Plr=Plr,Pc=Pc,Sc=Sc,
                                        TX=TX,TS=TS,CX=CX,CGLAC=CGLAC,CFR=CFR,NoL=NoL,cea=cea,
                                        R=R,Gsh=Gsh,Gsc=Gsc,gtcel=gtcel,GshInt=GshInt,GscInt=GscInt,
                                        CV=CV,a0=a0,d=d,rv=rv,Timeresinsec=Timeresinsec,
                                        MAD=MAD,hfelt=hfelt,maxLbog=maxLbog,midLbog=midLbog,
                                        bogfrac=bogfrac,zsoil=zsoil,zbog=zbog,midFL=midFL,
                                        stdFL=stdFL,maxFL=maxFL,maxDL=maxDL,midDL=midDL,glacfrac=glacfrac,midGl=midGl,
                                        stdGl=stdGl,maxGl=maxGl,hfeltmid=hfeltmid,totarea=totarea,
                                        midmetp=midmetp,midmett=midmett,nobognoglacarea=nobognoglacarea,
                                        slopesriverarea=slopesriverarea,bogarea=bogarea,glacarea=glacarea,elevarea=elevarea,gca=gca,
                                        soilca=soilca,gwgt=gwgt,swgt=swgt,mLam=mLam,varLam=varLam,cvLam=cvLam,meanIntk=meanIntk,
                                        unitsnow=unitsnow,SAVE=SAVE,pathResults=pathResults),
                   "load"          = getParam.load(path=path,filename=filename,SAVE=SAVE,pathResults=pathResults),
                   "processedNVE"  = getParam.processedNVE(path=path,filename=filename,SAVE=SAVE,pathResults=pathResults),
                   (message=paste0("Invalid method:", method,".")))

    return(inputParam)
}


getParam.load <- function(path,filename,SAVE,pathResults){

  load(paste0(path,"inputParam.rda"))

  if (SAVE){
    pathParam <- paste0(pathResults,"inputParam/")
    dir.create(pathParam, showWarnings = FALSE)
    do.call("save", list(obj="inputParam", file=paste0(pathParam,"inputParam.rda")))
  }
  return(inputParam)
}


getParam.manual <- function(catchment,nbLevelZone,Ws,Tlr,Plr,Pc,Sc,TX,TS,CX,CGLAC,CFR,NoL,cea,
                            R,Gsh,Gsc,gtcel,GshInt,GscInt,CV,a0,d,rv,Timeresinsec,
                            MAD,hfelt,maxLbog,midLbog,bogfrac,zsoil,zbog,midFL,
                            stdFL,maxFL,maxDL,midDL,glacfrac,midGl,stdGl,maxGl,hfeltmid,
                            totarea,midmetp,midmett,nobognoglacarea,
                            slopesriverarea,bogarea,glacarea,elevarea,gca,soilca,
                            gwgt,swgt,mLam,varLam,cvLam,meanIntk,unitsnow,SAVE,pathResults){

   inputParam <- list(catchment=catchment,nbLevelZone=nbLevelZone,Ws=Ws,Tlr=Tlr,Plr=Plr,Pc=Pc,Sc=Sc,
                TX=TX,TS=TS,CX=CX,CGLAC=CGLAC,CFR=CFR,NoL=NoL,cea=cea,
                R=R,Gsh=Gsh,Gsc=Gsc,gtcel=gtcel,GshInt=GshInt,GscInt=GscInt,
                CV=CV,a0=a0,d=d,rv=rv,Timeresinsec=Timeresinsec,
                MAD=MAD,hfelt=hfelt,maxLbog=maxLbog,midLbog=midLbog,
                bogfrac=bogfrac,zsoil=zsoil,zbog=zbog,midFL=midFL,
                stdFL=stdFL,maxFL=maxFL,maxDL=maxDL,midDL=midDL,glacfrac=glacfrac,midGl=midGl,
                stdGl=stdGl,maxGl=maxGl,hfeltmid=hfeltmid,totarea=totarea,
                midmetp=midmetp,midmett=midmett,nobognoglacarea=nobognoglacarea,
                slopesriverarea=slopesriverarea,bogarea=bogarea,glacarea=glacarea,elevarea=elevarea,gca=gca,
                soilca=soilca,gwgt=gwgt,swgt=swgt,mLam=mLam,varLam=varLam,cvLam=cvLam,meanIntk=meanIntk,
                unitsnow=unitsnow)
   if (SAVE){
     pathParam <- paste0(pathResults,"inputParam/")
     dir.create(pathParam, showWarnings = FALSE)
     do.call("save", list(obj="inputParam", file=paste0(pathParam,"inputParam.rda")))
   }

   return(inputParam)
}


getParam.processedNVE <- function(path,filename,SAVE,pathResults){

   ext <- tools::file_ext(paste0(path,filename))
   tmp <-switch(ext,
      "txt" = read.table(paste0(path,filename),sep="\t"),
      "csv" = read.csv(paste0(path,filename),header=FALSE))

   catchment <- as.character(tmp[1,1])
   for (i in 2:nrow(tmp)) {
     assign(as.character(tmp[i,1]),tmp[i,2])
   }

   nbLevelZone <- 10
   Ws <- pro
   Tlr <- tgrad
   Plr <- pgrad
   Pc <- pkorr
   Sc <- skorr
   Gsh <- Gshape
   Gsc <- Gscale
   CV <- cvHBV
   maxGL <- maxGl

   unitsnow <- 0.1

   a00<-c(a00,a01,a02,a03,a04,a05,a06,a07,a08,a09,a10)

   hfeltmid <- hfelt
   hfelt <- rep(0,nbLevelZone)
   for (i in 1:nbLevelZone) {
      hfelt[i] <-a00[(1+(i-1))]+(a00[(2+(i-1))]-a00[(1+(i-1))])/2
   }

   # parameters with different names
   totarea <-area      # total area in m2

   #tprm <- c(CX,TS,TX,pkorr, pro, tgrad, pgrad, cea) #parameters of your choosing to be written to the R2 file

   #Middelhoyde metstasjoner
   midmetp <- (hst1*vp1)+(hst2*vp2)
   midmett <- (hst1*vt1)+(hst2*vt2)

   nobognoglacarea <- (1-(bogfrac+glacfrac))*totarea   #area without bog area and glaciated area neither
   slopesriverarea <- (1-(bogfrac))*totarea            #area in which we have hillslope process and rivernetwork processes
   bogarea         <- bogfrac*totarea                  #bog area
   glacarea        <-glacfrac *totarea                 #glaciated area
   elevarea        <- (totarea/nbLevelZone)            #area for each level zone

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

   inputParam <- list(catchment=catchment,nbLevelZone=nbLevelZone,Ws=Ws,Tlr=Tlr,Plr=Plr,Pc=Pc,Sc=Sc,
               TX=TX,TS=TS,CX=CX,CGLAC=CGLAC,CFR=CFR,NoL=NoL,cea=cea,
               R=R,Gsh=Gsh,Gsc=Gsc,gtcel=gtcel,GshInt=GshInt,GscInt=GscInt,
               CV=CV,a0=a0,d=d,rv=rv,Timeresinsec=Timeresinsec,
               MAD=MAD,D=D,UP=UP,hfelt=hfelt,maxLbog=maxLbog,midLbog=midLbog,
               bogfrac=bogfrac,zsoil=zsoil,zbog=zbog,midFL=midFL,
               stdFL=stdFL,maxFL=maxFL,maxDL=maxDL,midDL=midDL,glacfrac=glacfrac,midGl=midGl,
               stdGl=stdGl,maxGl=maxGl,hfeltmid=hfeltmid,totarea=totarea,
               midmetp=midmetp,midmett=midmett,nobognoglacarea=nobognoglacarea,
               slopesriverarea=slopesriverarea,bogarea=bogarea,glacarea=glacarea,elevarea=elevarea,gca=gca,
               soilca=soilca,gwgt=gwgt,swgt=swgt,mLam=mLam,varLam=varLam,cvLam=cvLam,meanIntk=meanIntk,
               unitsnow=unitsnow)

   if (SAVE){
     pathParam <- paste0(pathResults,"inputParam/")
     dir.create(pathParam, showWarnings = FALSE)
     do.call("save", list(obj="inputParam", file=paste0(pathParam,"inputParam.rda")))
   }

   return(inputParam)
}
