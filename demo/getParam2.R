library("dddModel")

path <- paste0(.libPaths()[1],"/dddModel/data/")
filename <- "paramNVE.txt"

inputParam <- getParam(method="processedNVE",path=path, filename=filename,SAVE=TRUE,pathResults="~/")
str(inputParam)
