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
  env <- environment()
  path <- normalizePath(file.path(path,"inputParam.rda"),mustWork = FALSE)
  load(path, env=env)
  inputParam <- get("inputParam",envir = env)

  if (SAVE){
    pathParam <- normalizePath(file.path(pathResults,"inputParam"),mustWork = FALSE)
    dir.create(pathParam, showWarnings = FALSE, recursive = TRUE)
    do.call("save", list(obj="inputParam", file=normalizePath(file.path(pathParam,"inputParam.rda"),mustWork = FALSE)))
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
     pathParam <- normalizePath(file.path(pathResults,"inputParam"),mustWork = FALSE)
     dir.create(pathParam, showWarnings = FALSE)
     do.call("save", list(obj="inputParam", file=normalizePath(file.path(pathParam,"inputParam.rda"),mustWork = FALSE)))
   }

   return(inputParam)
}


getParam.processedNVE <- function(path,filename,SAVE,pathResults){

   env <- environment()

   ext <- tools::file_ext(paste0(path,filename))
   tmp <-switch(ext,
      "txt" = utils::read.table(paste0(path,filename),sep="\t"),
      "csv" = utils::read.csv(paste0(path,filename),header=FALSE))

   for (i in 1:nrow(tmp)) {
     assign(as.character(tmp[i,1]),tmp[i,2],envir=env)
   }

   catchment <- as.character(tmp[1,1])
   rm(tmp)

   nbLevelZone <- 10
   UP <- 0
   Ws <-  get("pro", envir = env)
   Tlr <- get("tgrad", envir = env)
   Plr <- get("pgrad", envir = env)
   Pc <-  get("pkorr", envir = env)
   Sc <-  get("skorr", envir = env)
   Gsh <- get("Gshape", envir = env)
   Gsc <- get("Gscale", envir = env)
   CV <-  get("cvHBV", envir = env)
   maxDL <- get("maxDl", envir = env)
   maxGL <- get("maxGl", envir = env)
   midFL <- get("midFl", envir = env)

   unitsnow <- 0.1

   tmp<-c(get("a00", envir = env),
          get("a01", envir = env),
          get("a02", envir = env),
          get("a03", envir = env),
          get("a04", envir = env),
          get("a05", envir = env),
          get("a06", envir = env),
          get("a07", envir = env),
          get("a08", envir = env),
          get("a09", envir = env),
          get("a10", envir = env))

   hfeltmid <- get("hfelt", envir = env)
   hfelt <- rep(0,nbLevelZone)
   for (i in 1:nbLevelZone) {
      hfelt[i] <-tmp[(1+(i-1))]+(tmp[(2+(i-1))]-tmp[(1+(i-1))])/2
   }

   # parameters with different names
   totarea <-get("area", envir = env)      # total area in m2

   #tprm <- c(CX,TS,TX,pkorr, pro, tgrad, pgrad, cea) #parameters of your choosing to be written to the R2 file

   #Middelhoyde metstasjoner
   midmetp <- (get("hst1", envir = env)*get("vp1", envir = env))+
              (get("hst2", envir = env)*get("vp2", envir = env))

   midmett <- (get("hst1", envir = env)*get("vt1", envir = env))+
              (get("hst2", envir = env)*get("vt2", envir = env))

   nobognoglacarea <- (1-(get("bogfrac", envir = env)+get("glacfrac", envir = env)))*get("totarea", envir = env)   #area without bog area and glaciated area neither
   slopesriverarea <- (1-(get("bogfrac", envir = env)))*get("totarea", envir = env)            #area in which we have hillslope process and rivernetwork processes
   bogarea         <- get("bogfrac", envir = env)*get("totarea", envir = env)                  #bog area
   glacarea        <- get("glacfrac", envir = env)*get("totarea", envir = env)                 #glaciated area
   elevarea        <- (get("totarea", envir = env)/nbLevelZone)            #area for each level zone

   gca <-c(get("g1", envir = env),
           get("g2", envir = env),
           get("g3", envir = env),
           get("g4", envir = env),
           get("g5", envir = env),
           get("g6", envir = env),
           get("g7", envir = env),
           get("g8", envir = env),
           get("g9", envir = env),
           get("g10", envir = env))
              # fraction of glacier pr elevation zone, fraction of glacier covered area pr elevation zone
   soilca <-1-gca

   #determines weights used to estimate the average glaciermelt. Finds the fraction of glaciers in each elevation zone in relation to total glacierarea
   # gwgt is the fraction of glaciated area pr elevation zone in relation to total glaciated area
   if(glacarea>0) {
     gwgt <-gca*elevarea/glacarea
   } else gwgt <- rep(0,nbLevelZone)

   swgt <-soilca*elevarea/(nobognoglacarea + bogarea) #Finds the fraction of soils (and bogs) in each elevation zone in relation to total soil (and bog) area

   mLam       <- get("GshInt",envir=env)*get("GscInt",envir=env)
   varLam     <- get("GshInt",envir=env)*(get("GscInt",envir=env))^2 #Yevjevich p.145
   cvLam      <- varLam^0.5/mLam
   meanIntk   <- mLam*midDL/get("Timeresinsec",envir=env) #middel hastighet beregent med Integrated Celerity

   inputParam <- list(catchment=catchment,
                      nbLevelZone=nbLevelZone,
                      Ws=Ws,
                      Tlr=Tlr,
                      Plr=Plr,
                      Pc=Pc,
                      Sc=Sc,
                      TX=get("TX",envir=env),
                      TS=get("TS",envir=env),
                      CX=get("CX",envir=env),
                      CGLAC=get("CGLAC",envir=env),
                      CFR=get("CFR",envir=env),
                      NoL=get("NoL",envir=env),
                      cea=get("cea",envir=env),
                      R=get("R",envir=env),
                      Gsh=Gsh,
                      Gsc=Gsc,
                      gtcel=get("gtcel",envir=env),
                      GshInt=GshInt,
                      GscInt=GscInt,
                      CV=CV,
                      a0=get("a0",envir=env),
                      d=get("d",envir=env),
                      rv=get("rv",envir=env),
                      Timeresinsec=get("Timeresinsec",envir=env),
                      MAD=get("MAD",envir=env),
                      hfelt=hfelt,
                      maxLbog=get("maxLbog",envir=env),
                      midLbog=get("midLbog",envir=env),
                      bogfrac=bogfrac,
                      zsoil=get("zsoil",envir=env),
                      zbog=get("zbog",envir=env),
                      midFL=midFL,
                      stdFL=get("stdFL",envir=env),
                      maxFL=get("maxFL",envir=env),
                      maxDL=maxDL,
                      midDL=get("midDL",envir=env),
                      glacfrac=glacfrac,
                      midGl=get("midGl",envir=env),
                      stdGl=get("stdGl",envir=env),
                      maxGl=maxGl,
                      hfeltmid=hfeltmid,
                      totarea=totarea,
                      midmetp=midmetp,
                      midmett=midmett,
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
                      unitsnow=unitsnow,
                      UP=UP)

   if (SAVE){
     pathParam <- normalizePath(file.path(pathResults,"inputParam"),mustWork = FALSE)
     dir.create(pathParam, showWarnings = FALSE)
     do.call("save", list(obj="inputParam", file=normalizePath(file.path(pathParam,"inputParam.rda"),mustWork = FALSE)))
   }

   return(inputParam)
}
