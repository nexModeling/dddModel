library("dddModel")

path <- paste0(.libPaths()[1],"/dddModel/data/")
filename <- "paramNVE.csv"

inputParam <- getParam(method="load",path=path, SAVE=FALSE)
str(inputParam)

models <- getModel(method="processedNVE",inputParam=inputParam,SAVE=TRUE,pathResults="~/")
str(models)
