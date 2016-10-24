library("dddModel")

path <- paste0(.libPaths()[1],"/dddModel/data/")
filename <- "paramNVE.txt"

inputParam <- getParam(method="processedNVE",path=path, filename=filename,SAVE=TRUE,pathResults="~/")
str(inputParam)


path <- "C:/Users/jm/Google Drive/nexModeling/2. Product/R/Rlibraries_ddd/DATA fromTS/"

#setwd(path)
filename <- "best_par_50.13_24h.txt"

inputParam <- getParam(method="processedNVE",path=path, filename=filename,SAVE=TRUE,pathResults="~/")
str(inputParam)
