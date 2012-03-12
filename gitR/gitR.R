require(gWidgets)
require(gWidgetsRGtk2)
require(gtools) ## for mixedsort
require(RGtk2)
options(guiToolkit="RGtk2")

source("methods.R")
source("gWidgetsRGtk2-hacks.R")
source("menu.R")
source("widgets.R")

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
  if ("??" %in% status$XY) {
    tmp <- data.frame(tag = "", mode = NA, object = "", stage = NA,
                      file = with(status, file[XY == "??"]))
    files <- if (nrow(files) > 0) rbind(files, tmp) else tmp
  }
  if ("!!" %in% status$XY) {
    tmp <- data.frame(tag = "", mode = NA, object = "", stage = NA,
                      file = with(status, file[XY == "!!"]))
    files <- if (nrow(files) > 0) rbind(files,tmp) else tmp
  }
  if (nrow(files) == 0) return(data.frame())
  ## FIXME: what to do with deleted files?
  ## add directories
  ## FIXME: what to do with orphaned submodules?
  files$directory = sub("/[^/]*$", "", files$file)
  files <- within(files, directory[directory == file] <- "")
  dirs <- unique(files$directory)
  dirs <- dirs[dirs != ""]
  if (length(dirs) > 0) 
    files <- rbind(files,
                   data.frame(tag = "", mode = 040000, object = "", stage = NA,
                              file = dirs,
                              directory = sub("/?[^/]+/?$", "", dirs)))
  ## add status
  files$was <- files$status <- ""
  for (file in status$file) {
    idx1 <- status$file == file
    idx2 <- files$file == file
    files$status[idx2] <- status$XY[idx1]
    files$was[idx2] <- status$was[idx1]
  }
  ## add status for directories
  for (dir in dirs) {
    statuses <- subset(files, directory == dir)$status
    ## if a directory only contains untracked files, mark it as untracked
    if (all(statuses == "??"))
      files$status[files$file == dir] <- "??"
    ## if a directory contains only renamed files, mark it as renamed
    if (all(grepl("R.", statuses)))
      files$status[files$file == dir] <- "R "
  }
  ## sort by directory, filename
  if (nrow(files) > 1) files <- files[with(files, mixedorder(file)),]
  ## remove directory from filename
  files$filename <- sub(".*/", "", files$file)
  files
}

## function to find offspring
offspring <- function(path, user.data, ...) {
  obj <- user.data$obj
  if(length(path) > 0) 
    directory <- obj$absPath(path)
  else {
    ## return the root module, wo that everything expands from there
    directory <- obj$absPath()
    dirty <- gitIsDirty(directory)
    Branch <- gitBranch(directory)
    Upstream <- gitUpstream(directory)
    status <- c("dirty", "detached HEAD", "unpushed commits", "unpulled commits")
    return(data.frame(Filename = obj$repo, mode=0,
                      Staged = FALSE, Modified = dirty,
                      Upstream = Upstream, Branch = Branch,
                      Status = paste(status[c(dirty, nchar(Branch)==0,
                        grepl("\\+", Upstream), grepl("\\-", Upstream))],
                        collapse=", "),
                      Mode=gitMode2Str(0)))
  }
  
  files <- readRepo(directory)
  ## drop all files in directories
  files <- files[files$directory == "", ,drop=FALSE]
  ## get branch name
  Branch <- rep("", nrow(files))
  idx <- !is.na(files$mode) & files$mode == 160000
  if (any(idx)) 
    Branch[idx] <- sapply(paste(directory, files$filename[idx],
                                sep = .Platform$file.sep), gitBranch)
  ## get Status
  Status <- gitStatus2Str(files$status)
  Staged <- grepl("(to|in) index", Status)
  Modified <- grepl("in work tree", Status)
  if (any(idx))
    Status[idx] <- ifelse(nchar(Branch[idx]) == 0, "detached HEAD", "")
  ## for modified submodules, get separate status
  idx <- files$mode == 160000 & substring(files$status, 2, 2) == "M"
  if (any(idx)) {
    Status[idx] <- paste(Status[idx],
                         gitStatus2Str(sprintf("%s ", substring(files$status[idx], 1, 1))),
                         gitSubmoduleStatus(directory, files$file[idx]), sep=", ")
  }
  ## get upstream status
  idx <- Branch != ""
  Upstream <- Branch
  Upstream[idx] <- sapply(paste(directory, files$filename[idx],
                                sep = .Platform$file.sep), gitUpstream)
  Status[idx] <- paste(Status[idx],
                       ifelse(grepl("\\+", Upstream[idx]), "unpushed commits", ""),
                       ifelse(grepl("\\-", Upstream[idx]), "unpulled commits", ""), sep=", ")
  ## clean status
  Status <- sub(", $", "", sub("^, ", "", gsub(", , ", ", ", Status)))
  data.frame(Filename=files$filename, mode = as.numeric(files$mode),
             Staged = Staged, Modified = Modified, Upstream = Upstream,
             Branch = Branch, Status = Status, Mode=gitMode2Str(files$mode))
}
hasOffspring <- function(children,user.data=NULL, ...) {
  return(children$mode %in% c(0, 160000, 40000))
}
icon.FUN <- function(children,user.data=NULL, ...) {
  x <- rep("file", length=nrow(children))
  x[children$mode == 0] <- "about"
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

##' gitR reference class
##'
##' Contains all information about a git manager window.
##' @name gitR-class
##' @rdname gitR-class
##' @slot path directory containing repository
##' @slot repo name of repository
##' @slot w gWindow
##' @slot m guiComponent
##' @slot tb gToolbar
##' @slot grp gGroup containing gTree and loading animation
##' @slot la loading animation image
##' @slot tr gTree
##' @slot s gStatus
##' @slot position window position
##' @slot lastout output of last git command
setRefClass("gitR",
            fields = list(
              path = "character",
              repo = "character",
              w = "gWindow",
              m = "guiComponent",
              tb = "gToolbar",
              grp = "gGroup",
              la = "gImage",
              tr = "gTree",
              s = "gStatusbar",
              position = "list",
              lastout = "character"
              ),
            methods = list(
              getWindow = function() {
                'Return gtkWindow object'
                w@widget@widget
              },
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
                update(.self$getGTree(), user.data = list(obj=.self))
                },
              status = function(...) {
                'Set or get status bar message'
                values <- list(...)
                if (length(values) == 0) return(svalue(s))
                svalue(s) <<- paste(values, collapse=" ")
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
              },
              absPath = function(...) {
                'Return absolute path'
                paths <- unlist(list(...))
                rPath <- if (length(paths) == 0) {
                  rPath <- repo
                } else paste(paths, collapse=.Platform$file.sep)
                paste(path, rPath, sep=.Platform$file.sep)
              }
              ))

##' Create GUI
##'
##' Opens the window, adds all gui elements and
##' handlers.
##' @param path path to repo
##' @return gitR object
##' @export
createGUI <- function(path=getwd()) {
  ## open the window, add a gtree, add handlers
  w <- gwindow("gitR", visible=FALSE)
  obj <- new("gitR", w=w,
             path = sub("(.*)/[^/]+/?$", "\\1", path),
             repo = sub(".*/([^/]+)/?$", "\\1", path))
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
                  offspring.data = list(obj = obj), expand=TRUE)
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
  tv$GetColumn(2)$SetVisible(FALSE)
  ## change cellrenderer of Staged column
  cellrenderer <- gtkCellRendererToggleNew()
  cellrenderer$radio <- TRUE
  column <- tv$GetColumn(3)
  column$Clear()
  column$PackStart(cellrenderer, TRUE)
  column$AddAttribute(cellrenderer, "active", 3)
  ## TODO: remove cellrenderer in first cell (root repo)
  ## change cellrenderer of Modified column
  column <- tv$GetColumn(4)
  column$Clear()
  column$PackStart(cellrenderer, TRUE)
  column$AddAttribute(cellrenderer, "active", 4)

  ## expand root repository
  obj$tr$ExpandRow(gtkTreePathNewFromString("0"), FALSE)

  ## set status
  obj$status("Initialized.")
  ## resize window
  w$resize(1000, 600)
  ## now show it
  visible(obj$w) <- TRUE
  obj
}
  
