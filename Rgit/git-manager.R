require(gWidgets)
require(gWidgetsRGtk2)
## source("methods.R")

## function to find offspring
offspring <- function(path, user.data=NULL) {
  if(length(path) > 0) 
    directory <- paste(getwd(), .Platform$file.sep,
                       paste(path,collapse=.Platform$file.sep),
                       sep="")
  else
    directory <- getwd()
  
  files <- gitLsFiles(directory, c("stage", "others"))
  data.frame(Filename=files$file,
             Mode=gitMode2Str(files$mode), mode=files$mode)
}
hasOffspring <- function(children,user.data=NULL, ...) {
  return(children$mode == 160000)
}
icon.FUN <- function(children,user.data=NULL, ...) {
  x <- rep("file", length=nrow(children))
  str(children)
  x[children$mode == 160000] <- "directory"
  return(x)
}

## does not show isdir directory, as hasOffspring=NULL and
## second column is a logical
w <- gwindow("GUI Test", toolkit=guiToolkit("RGtk2"))
tr <- gtree(offspring, hasOffspring, icon.FUN = icon.FUN, container=w)

addHandlerDoubleclick(tr, handler=function(h,...) {
  print(svalue(h$obj))		# the key
  print(h$obj[])		# vector of keys
})

add3rdMousePopupmenu(tr, list(test=list(handler=function(h, ...) print(h))), action="TEST")
