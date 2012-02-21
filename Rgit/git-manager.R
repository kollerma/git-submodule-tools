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
  #cat("requested repo for", dir, "\n")
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
  ## get Status
  Status <- gitStatus2Str(files$status)
  Staged <- grepl("(to|in) index", Status)
  Modified <- grepl("in work tree", Status)
  ## for modified submodules, get separate status
  idx <- files$mode == 160000 & substring(files$status, 2, 2) == "M"
  if (any(idx)) {
    Status[idx] <- gitStatus2Str(sprintf("%s ", substring(files$status[idx], 1, 1)))
    Status[idx] <- paste(Status[idx], gitSubmoduleStatus(directory, files$file[idx]), sep=", ")
    Status[idx] <- sub(", $", "", sub("^, ", "", Status[idx]))
  }
  data.frame(Filename=files$filename, Staged = Staged, Modified = Modified,
             Status = Status, Mode=gitMode2Str(files$mode), mode=files$mode)
}
hasOffspring <- function(children,user.data=NULL, ...) {
  return(children$mode == 160000 | children$mode == 40000)
}
icon.FUN <- function(children,user.data=NULL, ...) {
  x <- rep("file", length=nrow(children))
  x[children$mode == 160000] <- "jump-to"
  x[children$mode == 40000] <- "directory"
  return(x)
}

##' Add a context menu
##'
##' Function that adds Context Menu
##' like add3rdMousePopupMenu but with dynamic menu.
##' @param h handler list
##' @param widget (unused)
##' @param event gtkEvent
##' @param action (unused)
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

##' Get expanded rows
##'
##' Returns the paths of the rows currently expanded.
##' @param obj gTreeRGtk object
##' @return vector of paths of expanded rows
getExpandedRows <- function(obj) {
  tag(obj, "expandedRows") <- NULL
  obj@widget$MapExpandedRows(function(obj, path, action) {
    obj <- action$actualobj
    parent.iter <- tag(obj, "store")$GetIter(path)
    iter <- parent.iter$iter
    #print(tag(obj, "store")$GetStringFromIter(iter))
    ## build file key
    repoPath <- ""
    while (parent.iter$retval) {
      repoPath <- paste(tag(obj, "store")$GetValue(parent.iter$iter,
                                                   tag(obj, "iconFudge"))$value,
                        repoPath, sep="/")
      parent.iter <- tag(obj, "store")$IterParent(parent.iter$iter)
    }
    tag(obj, "expandedRows") <- c(tag(obj, "expandedRows"), sub("/$", "", repoPath))
  }, list(actualobj=obj))
  return(tag(obj, "expandedRows"))
}

##' Expand Rows
##'
##' Restore the saved expansion state.
##' @param obj gTreeRGtk object
##' @param iter iter thingy
##' @param expandedRows vector of paths to expand
##' @param root used in recursive calling of the function
expandRows <- function(obj, iter, expandedRows, root=NULL) {
  continue <- TRUE
  while(continue) {
    path <- tag(obj, "store")$GetValue(iter, tag(obj, "iconFudge"))$value
    if (!is.null(root)) path <- paste(root, path, sep="/")
    if (path %in% expandedRows) {
      #cat("expand row", path, "\n")
      obj@widget$ExpandRow(tag(obj, "store")$GetPath(iter), FALSE)
      lexpandedRows <- grep(sprintf("^%s/", path), expandedRows, value=TRUE)
      if (length(lexpandedRows) > 0) {
        child.iter = tag(obj, "store")$IterChildren(iter)
        if (child.iter$retval) {
          expandRows(obj, child.iter$iter, lexpandedRows, path)
        }
      }
    }
    continue <- tag(obj, "store")$IterNext(iter)
  }
}

getOffSpringIcons <- gWidgetsRGtk2:::getOffSpringIcons
addChildren <- gWidgetsRGtk2:::addChildren
##' Update gtree
##'
##' Updates the gtree (replaces function in gtreeRGtk).
##' It does the same, but also takes care of the expanded rows
##' and updates the values displayed in the other columns.
##' @param object gTreeRGtk object
##' @param toolkit guiWidgetsToolkitRGtk2
##' @param ... (unused)
setMethod(".update",
          signature(toolkit="guiWidgetsToolkitRGtk2",object="gTreeRGtk"),
          function(object, toolkit, ...) {
            obj <- object
            ## first get a list of expanded rows
            expandedRows <- getExpandedRows(obj)
            #print(expandedRows)
            ## collapse all rows (is this needed?)
            obj@widget$CollapseAll()
            ## remove all rows
            tag(obj, "store")$Clear()
            ## put in children again
            children <- tag(obj, "offspring")("")
            lst <- getOffSpringIcons(children, tag(obj, "hasOffspring"),
                                     tag(obj, "icon.FUN"))
            children <- lst$children
            doExpand <- lst$doExpand
            addChildren(tag(obj, "store"), children, doExpand,
                        tag(obj, "iconFudge"), parent.iter=NULL)
            ## restore expanded rows
            iter <- tag(obj, "store")$GetIterFirst()
            if (length(expandedRows) > 0 && iter$retval)
              expandRows(obj, iter$iter, expandedRows)
          })


## open the window, add a gtree, add handlers
w <- gwindow("git manager", toolkit=guiToolkit("RGtk2"))
tr <- gtree(offspring, hasOffspring = hasOffspring, icon.FUN = icon.FUN, container=w)
## add basic doubleclick handler
addHandlerDoubleclick(tr, handler=function(h,...) {
  print(svalue(h$obj))		     # the key
  print(paste(h$obj[], collapse="/")) # vector of keys
})
## add Context Menu
addHandler(tr@widget, signal="button-press-event",
           handler=contextMenu, action=list(actualobj=tr))
## Hide mode column
tr@widget@widget$GetColumn(6)$SetVisible(FALSE)
## change cellrenderer of Staged column
cellrenderer <- gtkCellRendererToggleNew()
#cellrenderer$activatable <- TRUE
cellrenderer$radio <- TRUE
column <- tr@widget@widget$GetColumn(2)
column$Clear()
column$PackStart(cellrenderer, TRUE)
column$AddAttribute(cellrenderer, "active", 2)
## change cellrenderer of Modified column
## cellrenderer <- gtkCellRendererToggleNew()
## #cellrenderer$activatable <- TRUE
## cellrenderer$radio <- TRUE
column <- tr@widget@widget$GetColumn(3)
column$Clear()
column$PackStart(cellrenderer, TRUE)
column$AddAttribute(cellrenderer, "active", 3)

## can update tree view while keeping the rows expanded
update(tr@widget)
