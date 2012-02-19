require(gWidgets)
require(gWidgetsRGtk2)
require(gtools) ## for mixedsort
require(RGtk2)

## source("methods.R")

##' readRepo
##'
##' Function to read in the files and status of a 
##' git repository.
##' @param dir repository directory
##' @return data.frame
readRepo <- function(dir=getwd()) {
  ## files in stage
  files <- gitLsFiles(dir, "stage")
  ## get status (also shows ignored and untracked files)
  status <- gitStatus(dir, ignored=TRUE)
  ## add ignored and untracked files to files
  if ("??" %in% status$XY)
    files <- rbind(files,
                   data.frame(tag = "", mode = NA, object = "", stage = NA,
                              file = with(status, file[XY == "??"])))
  if ("!!" %in% status$XY)
    files <- rbind(files,
                   data.frame(tag = "", mode = NA, object = "", stage = NA,
                              file = with(status, file[XY == "!!"])))
  ## add directories
  files$directory = sub("/[^/]*$", "", files$file)
  files <- within(files, directory[directory == file] <- "")
  dirs <- unique(files$directory)
  dirs <- dirs[dirs != ""]
  if (length(dirs) > 0) 
    files <- rbind(files,
                   data.frame(tag = "", mode = 040000, object = "", stage = NA,
                              file = dirs, directory = sub("(/?)[^/]+/?", "\\1", dirs)))
  ## add status
  files$was <- files$status <- ""
  for (file in status$file) {
    idx1 <- status$file == file
    idx2 <- files$file == file
    files$status[idx2] <- status$XY[idx1]
    files$was[idx2] <- status$was[idx1]
  }
  ## sort by directory, filename
  if (nrow(files) > 1) files <- files[with(files, mixedorder(file)),]
  ## remove directory from filename
  files$filename <- sub(".*/([^/]+/?)$", "\\1", files$file)
  files
}

## function to find offspring
offspring <- function(path, user.data=NULL) {
  if(length(path) > 0) 
    directory <- paste(getwd(), .Platform$file.sep,
                       paste(path,collapse=.Platform$file.sep),
                       sep="")
  else
    directory <- getwd()
  
  files <- readRepo(directory)
  ## drop all files in directories
  files <- files[files$directory == "", ]
  data.frame(Filename=files$filename, Status = gitStatus2Str(files$status),
             Mode=gitMode2Str(files$mode))
}
hasOffspring <- function(children,user.data=NULL, ...) {
  return(children$Mode %in% gitMode2Str(c(160000, 40000)))
}
icon.FUN <- function(children,user.data=NULL, ...) {
  x <- rep("file", length=nrow(children))
  x[children$Mode == gitMode2Str(160000)] <- "jump-to"
  x[children$Mode == gitMode2Str(40000)] <- "directory"
  return(x)
}

w <- gwindow("git manager", toolkit=guiToolkit("RGtk2"))
tr <- gtree(offspring, hasOffspring = hasOffspring, icon.FUN = icon.FUN, container=w)

## add basic doubleclick handler
addHandlerDoubleclick(tr, handler=function(h,...) {
  print(svalue(h$obj))		     # the key
  print(paste(h$obj[], collapse="/")) # vector of keys
})

## add Context Menu
## like add3rdMousePopupMenu but with dynamic menu
contextMenu <- function(h, widget, event, action=NULL, ...) {
  ## Mac use ctrl - button 1
  if(event$GetButton() == 3 ||
     (event$GetState() == GdkModifierType['control-mask'] &&
      event$GetButton() == 1)) {
    obj <- h$action$actualobj
    ## update current selection
    path <- obj$getPathAtPos(event$x, event$y)$path
    sel <- obj$getSelection()
    sel$unselectAll()
    sel$selectPath(path)
    menulist <- list(list(handler=function(x, ...) print(paste(obj[], collapse="/"))))
    names(menulist) <- svalue(obj)
    mb = gmenu(menulist, popup = TRUE)
    mb = tag(mb,"mb")                 # actual widget
    gtkMenuPopup(mb,button = event$GetButton(),
                 activate.time=event$GetTime())
  } else {
    return(FALSE)
  }
}
addHandler(tr@widget, signal="button-press-event",
           handler=contextMenu, action=list(actualobj=tr))


