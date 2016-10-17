#' Get the models to run ddd
#'
#' Get all the models to run ddd
#' Three options:
#' - build from a set of parameters
#' - load from a rda file
#' - load from a R-file
#' @param getModel method to get the model parameters ("build", "load" or "source")
#' @param path directory where to get the files
#' @param Timeresinsec time resolution of the process in second (1hour: 3600s, ... etc)
#'  The value can be NULL if the getModel method is "load" or "source"
#' @return a list of all the models used to run ddd
#' @keywords model
#' @export
#' @examples
#' \dontrun{
#' getModel()
#' }
getModel <-function(getModel,path,Timeresinsec=NULL){

  ######################################
  # Load rda files                     #
  ######################################
  getModel.load<-function(path) {
    load(paste0(path,"models.rda"))
    res <- models
    return(res)
  }


  #######################################
  # Load a R source file                #
  #######################################
  getModel.source<-function(path) {
    source(paste0(path,"dddModel.R"),local=TRUE)
    res <- models
    return(res)
  }


  ####################################
  # DEFAULT BUILDING MODEL CONDITION #
  ####################################
  getModel.buildModel<-function(path,Timeresinsec){
    res <- buildModel(path=path,Timeresinsec=Timeresinsec)
    return(res)
  }



   #############################
   # DIFFERENT CHOICES         #
   #############################
   res <- switch(getModel,
     "build"     = getModel.buildModel(path=path,Timeresinsec=Timeresinsec),
     "load"      = getModel.load(path=path),
     "source"    = getModel.source(path=path),
     (message=paste0("Invalid method of building models:", getModel,".")))
   return(res)

}
