#' Get the models to run ddd
#'
#' Get all the models to run ddd
#' Two options:
#' - build from a set of parameters
#' - load from a rda file
#' @param method Method to get the model parameters: "buildNVE" or "load"
#' @param path Directory where to get the files
#' @param inputParam List input parameters
#' @param Timeresinsec Time resolution of the process in second (1hour: 3600s, ... etc)
#' @param SAVE Save the results, Boolean
#' @param pathResults Path of the results. By default: $HOME
#' @return a list of all the models used to run ddd
#' @keywords model
#' @export
#' @examples
#' \dontrun{
#' getModel()
#' }
getModel <-function(method=NULL,path=NULL,inputParam=NULL,Timeresinsec=NULL,SAVE=FALSE,pathResults="~/"){

   res <- switch(method,
     "processedNVE"  = getModel.processedNVE(inputParam=inputParam,Timeresinsec=Timeresinsec,SAVE=SAVE,pathResults=pathResults),
     "load"          = getModel.load(path=path,SAVE=SAVE,pathResults=pathResults),
     (message=paste0("Invalid method of building models:", getModel,".")))
   return(res)
}

getModel.load<-function(path,SAVE,pathResults) {
  load(paste0(path,"models.rda"))

  if (SAVE){
    pathModel <- normalizePath(file.path(pathResults,"models"),mustWork = FALSE)
    dir.create(pathModel, showWarnings = FALSE)
    do.call("save", list(obj="models", file=normalizePath(file.path(pathModel,"models.rda"),mustWork = FALSE)))
  }

  res <- models
  return(res)
}


getModel.processedNVE<-function(inputParam,Timeresinsec,SAVE,pathResults){

  if (is.null(Timeresinsec)) {
    Timeresinsec <- inputParam$Timeresinsec
  } else Timeresinsec <- Timeresinsec

  # MODEL SATURATION
  modelSaturation <- list(gtcel=inputParam$gtcel,CapacityUpperLevel=2000 ,mLam=inputParam$mLam,varLam=inputParam$varLam,distr="qgamma")

  # MODEL k
  modelk <- list(gtcel=inputParam$gtcel,Gsh=inputParam$Gsh,Gsc=inputParam$Gsc,midDL=inputParam$midDL)

  # MODEL LAYERS
  modelLayer <- list(maxL=inputParam$maxDL,speed=NULL,nbStepsDelay= NULL,z=inputParam$zsoil,distr="dexp",param=c(inputParam$midDL), NoL=inputParam$NoL)
  k <- dddCelerity::ck(NoL=inputParam$NoL,gtcel=modelk$gtcel,Gsh=modelk$Gsh,Gsc=modelk$Gsc,midDL=modelk$midDL,Timeresinsec=Timeresinsec)
  nbStepsLevel <- dddCelerity::nbSteps(maxL=modelLayer$maxL,speed=k,Timeresinsec=Timeresinsec)
  modelLayer$speed <- k
  modelLayer$nbStepsDelay <- nbStepsLevel

  # MODEL RIVER
  modelRiver <- list(maxL=(inputParam$maxFL+inputParam$maxGl),speed=inputParam$rv,nbStepsDelay=NULL ,z=0,distr="dnorm",param=c((inputParam$midFL + inputParam$midGl),(inputParam$stdFL + inputParam$stdGl)))
  nbStepsRiv <- dddCelerity::nbSteps(maxL=modelRiver$maxL,speed=modelRiver$speed,Timeresinsec=Timeresinsec)
  modelRiver$nbStepsDelay <- nbStepsRiv

  # MODEL BOG
  modelBog <- list(maxL=inputParam$maxLbog,speed=NULL,nbStepsDelay=NULL,z=inputParam$zbog,distr="dexp",param=c(inputParam$midLbog))
  bogSpeed <- k[1]*1
  nbStepsBog <- dddCelerity::nbSteps(maxL=modelBog$maxL,speed=bogSpeed,Timeresinsec=Timeresinsec)
  modelBog$speed <- bogSpeed
  modelBog$nbStepsDelay <- nbStepsBog

  # MODEL MAD
  modelMAD <- list(maxL=inputParam$maxDL,speed=inputParam$meanIntk,nbStepsDelay= NULL,z=0,distr="dexp",param=c(inputParam$midDL))
  nbStepsMAD <- dddCelerity::nbSteps(maxL=modelMAD$maxL,speed=modelMAD$speed,Timeresinsec=Timeresinsec)
  modelMAD$nbStepsDelay <- nbStepsMAD

  # MODEL SOIL MOISTURE
  modelSoilMoisture   <- list(swgt=inputParam$swgt,gwgt=inputParam$gwgt)

  # MODEL SOIL WATER
  modelSoilWater      <- list(R=inputParam$R)

  # MODEL SOIL DISCHARGE
  modelSoilDischarge  <- list(MAD=inputParam$MAD)

  # MODEL SOIL
  modelSoil           <- list(glacfrac=inputParam$glacfrac) #...

  # MODEL EVAPOTRANSPIRATION
  modelET             <- list(cea=inputParam$cea)

  # MODEL SNOW
  modelSnow           <- list(nbLevelZone=inputParam$nbLevelZone,
                              unitsnow = inputParam$unitsnow,
                              n0 = inputParam$unitsnow*inputParam$a0,
                              Ws = inputParam$Ws,
                              TS = inputParam$TS,
                              CX = inputParam$CX,
                              CFR = inputParam$CFR,
                              CGLAC = inputParam$CGLAC,
                              gca =inputParam$gca,
                              UP = inputParam$UP)

  # MODEL TEMPERATURE FOR DIFFERENT LEVEL ZONES
  modelTempLZ        <- list(nbLevelZone=inputParam$nbLevelZone,Tlr=inputParam$Tlr,hfelt=inputParam$hfelt,midmett=inputParam$midmett)

  # MODEL PRECIPITATION FOR DIFFERENT LEVEL ZONES
  modelPrecipLZ      <- list(nbLevelZone=inputParam$nbLevelZone,Plr=inputParam$Plr,hfelt=inputParam$hfelt,midmetp=inputParam$midmetp,
                             TX=inputParam$TX,Pc=inputParam$Pc,Sc=inputParam$Sc,
                             a0 = inputParam$a0, d = inputParam$d)

  # MODEL PRECIPITATION
  # modelPrecipitation<- list(TX=inputParam$TX,Pc=inputParam$Pc,Sc=inputParam$Sc,
  #                             a0 = inputParam$a0, d = inputParam$d)

  # MODEL AREA
  modelArea          <- list(totarea = inputParam$totarea,
                             slopesriverarea=inputParam$slopesriverarea,
                             nobognoglacarea=inputParam$nobognoglacarea,
                             bogarea=inputParam$bogarea)


  models <- list( modelk          = modelk,
               modelSaturation = modelSaturation,
               modelSoilMoisture=modelSoilMoisture,
               modelSoilWater  = modelSoilWater,
               modelSoilDischarge = modelSoilDischarge,
               modelSoil       = modelSoil,
               modelLayer      = modelLayer,
               modelRiver      = modelRiver,
               modelBog        = modelBog,
               modelMAD        = modelMAD,
               modelET         = modelET,
               modelSnow       = modelSnow,
               modelTempLZ     = modelTempLZ,
               modelPrecipLZ   = modelPrecipLZ,
               modelArea       = modelArea)

  if (SAVE){
    pathModel <- normalizePath(file.path(pathResults,"models"),mustWork = FALSE)
    dir.create(pathModel, showWarnings = FALSE)
    do.call("save", list(obj="models", file=normalizePath(file.path(pathModel,"models.rda"),mustWork = FALSE)))
  }

  return(models)
}
