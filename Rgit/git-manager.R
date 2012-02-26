require(gWidgets)
require(gWidgetsRGtk2)
require(gtools) ## for mixedsort
require(RGtk2)
options(guiToolkit="RGtk2")

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

menuOpen <- function(h, action, ...) str(h)
menuDelete <- function(h, action, ...) str(h)
menuIgnore <- function(h, action, ...) str(h)
menuAdd <- function(h, action, ...) str(h)
menuUnadd <- function(h, action, ...) str(h)
menuReset <- function(h, action, ...) str(h)
menuMove <- function(h, action, ...) str(h)
menuRpull <- function(h, action, ...) str(h)
menuRpush <- function(h, action, ...) str(h)
menuRcommit <- function(h, action, ...) str(h)
menuRcheckout <- function(h, action, ...) str(h)
menuLog <- function(h, action, ...) str(h)
menuMake <- function(h, action, ...) str(h)

##' Generate menulist for context menu
##'
##' Depending on type of file, generate a different context menu.
##' @param obj gTreeRGtk object
genMenulist <- function(obj) {
  path <- paste(obj[], collapse="/")
  filename <- svalue(obj)
  #cat("Generating menu for", filename, "at", path, "\n")

  sel <- obj$getSelection()$getSelected()
  mode <- sel$model$getValue(sel$iter, 6)$value
  staged <- sel$model$getValue(sel$iter, 2)$value
  modified <- sel$model$getValue(sel$iter, 3)$value
  action <- list(obj=obj, path=path, filename=filename,
                 mode=mode, staged=staged, modified=modified)
  
  menulist <-
    list(Open=gaction("Open", tooltip = "Open using external program", icon = "open",
           handler = menuOpen, action = action),
         Delete=gaction("Delete", tooltip = "Delete in work tree", icon = "delete",
           handler = menuDelete, action = action))
  if (is.na(mode)) { ## an untracked file
    menulist$Ignore <- gaction("Ignore", tooltip = "Add to .gitignore", icon = "stop",
                               handler = menuIgnore, action = action)
  }
  if (is.na(mode) || modified) { ## a modified or untracked file
    menulist$Add <- gaction("Add", tooltip = "Add to staging area", icon = "add",
                              handler = menuAdd, action = action)
  }
  if (!is.na(mode)) { ## a tracked file
    if (staged) {
      menulist$Unadd <- gaction("Unadd", tooltip = "Remove from stagin area", icon = "remove",
                                handler = menuUnadd, action = action)
    }
    if (modified) {
      menulist$Reset <- gaction("Reset", tooltip = "Reset to version in index", icon = "revert-to-saved",
                                handler = menuReset, action = action)
    }
    ## menulist$Move$icon <- 
    menulist$Move <- gaction("Move", tooltip = "Rename",
                             handler = menuMove, action = action)
  }
  if (!is.na(mode) && mode == 160000) { ## submodule
    menulist$Rpull <- gaction("Rpull", tooltip = "Pull updates from server, recursively", icon = "go-down",
                              handler = menuRpull, action = action)
    menulist$Rpush <- gaction("Rpush", tooltip = "Push commits to server, recursively", icon = "go-up",
                              handler = menuRpush, action = action)
    if (modified) {
      menulist$Rcommit <- gaction("Rcommit", tooltip = "Commit, recursively", icon = "apply",
                                  handler = menuRcommit, action = action)
    }
    menulist$Rcheckout <- gaction("Rcheckout", tooltip = "Checkout another branch/tag",
                                  handler=menuRcheckout, action = action)
    menulist$Log <- gaction("Log", tooltip = "Display commit log", icon = "info",
                            handler = menuLog, action = action)
  }
  ## try to find a Makefile
  if (!is.na(mode) && (mode == 160000 || mode == 40000)) {
    candidates <- paste(path, c("Makefile", "makefile"), sep="/")
    makefile <- candidates[file.exists(candidates)][1]
  } else if (casefold(filename) == "makefile") {
    makefile <- path
  } else makefile <- NA
  if (!is.na(makefile)) {
    targets <- makeGetTargets(makefile)
    menulist$Make <- list()
    for (target in targets) {
      menulist$Make[[target]] <- gaction(target, icon = switch(target,
                                                   view=menulist$Make[[target]]$icon <- "open",
                                                   clean=menulist$Make[[target]]$icon <- "clear",
                                                   all=menulist$Make[[target]]$icon <- "execute",
                                                   edit=menulist$Make[[target]]$icon <- "edit", NULL),
                                         handler = menuMake, action = c(action, list(target=target)))
    }
  }
  
  menulist
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
    menulist <- genMenulist(obj)
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
w <- gwindow("git manager")
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
