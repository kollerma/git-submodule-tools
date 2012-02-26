require(gWidgets)
require(gWidgetsRGtk2)
require(gtools) ## for mixedsort
require(RGtk2)
options(guiToolkit="RGtk2")

source("methods.R")
source("gWidgetsRGtk2-hacks.R")
source("menu.R")

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
                              file = dirs,
                              directory = sub("(/?)[^/]+/?", "\\1", dirs)))
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
    Status[idx] <- paste(Status[idx],
                         gitSubmoduleStatus(directory, files$file[idx]), sep=", ")
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
    path <- obj$tr$getPathAtPos(event$x, event$y)$path
    sel <- obj$tr$getSelection()
    sel$unselectAll()
    sel$selectPath(path)
    menulist <- genContextMenulist(obj)
    mb = gmenu(menulist, popup = TRUE)
    mb = tag(mb,"mb")                 # actual widget
    gtkMenuPopup(mb,button = event$GetButton(),
                 activate.time=event$GetTime())
  } else {
    return(FALSE)
  }
}

##' gitManager reference class
##'
##' Contains all information about a git manager window.
##' @slot path root path of repository
##' @slot w gWindow
##' @slot m guiComponent
##' @slot tb gToolbar
##' @slot grp gGroup containing gTree and loading animation
##' @slot la loading animation image
##' @slot tr gTree
##' @slot s gStatus
##' @slot position window position
setRefClass("gitManager",
            fields = list(
              path = "character",
              w = "gWindow",
              m = "guiComponent",
              tb = "gToolbar",
              grp = "gGroup",
              la = "gImage",
              tr = "gTree",
              s = "gStatusbar",
              position = "list"
              ),
            methods = list(
              getGTree = function() {
                'Return GTree object'
                tr@widget
                },
              getTreeView = function() {
                'Return gtkTreeView object'
                tr@widget@widget
              },
              refresh = function() {
                'Refresh gTree'
                .self$status("Refreshing...")
                .self$hide()
                while(gtkEventsPending()) gtkMainIteration()
                update(.self$getGTree())
                .self$status("Refreshed view.")
                .self$show()
                },
              status = function(value) {
                'Set or get status bar message'
                if (missing(value)) return(svalue(s))
                svalue(s) <<- value
                invisible(svalue(s))
              },
              hide = function() {
                'Hide tree view and display loading animation'
                grp$Remove(tr@widget@block@widget@widget)
                grp$Add(la@widget@block)
              },
              show = function() {
                'Show tree view and hide loading animation'
                grp$Remove(la@widget@block)
                grp$Add(tr@widget@block@widget@widget)
              }
              ))

##' Create GUI
##'
##' Opens the window, adds all gui elements and
##' handlers.
##' @param path path to repo
##' @return gitManager object
createGUI <- function(path=getwd()) {
  ## open the window, add a gtree, add handlers
  w <- gwindow("git manager", visible=FALSE)
  obj <- new("gitManager", path=path, w=w)
  ## add basic menu
  obj$m <- gmenu(genMenulist(obj), container=w)
  ## add basic toolbar
  obj$tb <- gtoolbar(genToolbar(obj), style="icons", container=w)
  ## create ggroup that will hold tree view and loading animation
  obj$grp <- ggroup(horizontal=FALSE, spacing=0, container=w)
  ## create loadingAnimation but do not display yet
  obj$la <- gimage(system.file("images/loading.gif", package="traitr"),
                   expand = TRUE)
  ## create tree view
  obj$tr <- gtree(offspring, hasOffspring = hasOffspring,
                  icon.FUN = icon.FUN, container=obj$grp,
                  action = list(obj = obj), expand=TRUE)
  obj$s <- gstatusbar("Initializing...", container=w)
  ## add basic doubleclick handler
  addHandlerDoubleclick(obj$getGTree(), handler=function(h, ...) {
    print(svalue(h$obj))		     # the key
    print(paste(h$obj[], collapse="/")) # vector of keys
  }, action=list(actualobj=obj))
  ## add Context Menu
  addHandler(obj$getGTree(), signal="button-press-event",
             handler=contextMenu, action=list(actualobj=obj))
  ## alter gTree
  ## Hide mode column
  tv <- obj$getTreeView()
  tv$GetColumn(6)$SetVisible(FALSE)
  ## change cellrenderer of Staged column
  cellrenderer <- gtkCellRendererToggleNew()
  cellrenderer$radio <- TRUE
  column <- tv$GetColumn(2)
  column$Clear()
  column$PackStart(cellrenderer, TRUE)
  column$AddAttribute(cellrenderer, "active", 2)
  ## change cellrenderer of Modified column
  column <- tv$GetColumn(3)
  column$Clear()
  column$PackStart(cellrenderer, TRUE)
  column$AddAttribute(cellrenderer, "active", 3)

  ## set status
  obj$status("Initialized.")
  ## now show it
  visible(obj$w) <- TRUE
  obj
}
  
