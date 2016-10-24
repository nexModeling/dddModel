library("dddModel")

path <- paste0(.libPaths()[1],"/dddModel/data/")

models <- getModel(method="load",path=path,SAVE=FALSE)
str(models)
