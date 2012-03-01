.genMenulist <- function(what, action)
  list(Add=gaction("Add", tooltip = "Add to staging area",
         icon = "add",
         handler = function(...) menu("Add", ...), action = action),
       Delete=gaction("Delete", tooltip = "Delete in work tree",
         icon = "delete",
         handler = function(...) menu("Delete", ...), action = action),
       Ignore=gaction("Ignore", tooltip = "Add to .gitignore",
         icon = "stop",
         handler = function(...) menu("Ignore", ...), action = action),
       Info=gaction("Info", tooltip = "Display info",
         icon = "info",
         handler = function(...) menu("Info", ...), action=action),
       Log=gaction("Log", tooltip = "Display commit log",
         icon = "justify-left",
         handler = function(...) menu("Log", ...), action = action),
       LongCMD = gaction("Long Command", tooltip="test systemWithSleep",
         handler = function(...) menu("LongTest", ...), action = action),
       Move=gaction("Move", tooltip = "Rename",
         handler = function(...) menu("Move", ...), action = action),
       Open=gaction("Open", tooltip = "Open using external program",
           icon = "open",
         handler = function(...) menu("Open", ...), action = action),
       Quit = gaction("Quit", tooltip = "Quit git manager",
         icon = "quit",
         handler = function(...) menu("Quit", ...), action = action),
       Refresh = gaction("Refresh", tooltip = "Refresh view",
         icon = "refresh",
         handler = function(...) menu("Refresh", ...), action = action),
       Reset=gaction("Reset", tooltip = "Reset to version in index",
         icon = "revert-to-saved",
         handler = function(...) menu("Reset", ...), action = action),
       Rcheckout=gaction("Rcheckout",
         tooltip = "Checkout another branch/tag",
         handler=function(...) menu("Rcheckout", ...), action = action),
       Rcommit=gaction("Rcommit", tooltip = "Commit, recursively",
         icon = "apply",
         handler = function(...) menu("Rcommit", ...), action = action),
       Rpull=gaction("Rpull", tooltip = "Pull updates from server, recursively",
         icon = "go-down",
         handler = function(...) menu("Rpull", ...), action = action),
       Rpush=gaction("Rpush", tooltip = "Push commits to server, recursively",
         icon = "go-up",
         handler = function(...) menu("Rpush", ...), action = action),
       Unadd=gaction("Unadd", tooltip = "Remove from stagin area",
         icon = "remove",
         handler = function(...) menu("Unadd", ...), action = action)
       )[what]

##' Generate menulist for regular menu
##'
##' File menu, etc
##' @param obj gitManager object
genMenulist <- function(obj) {
  action <- list(obj=obj)
  list(File=.genMenulist(c("Refresh", "Quit"), action))
}

##' Generate menulist for toolbar
##'
##' Toolbar with repo wide tasks
genToolbar <- function(obj) {
  action <- list(obj=obj)
  .genMenulist(c("Rpull", "Refresh", "Quit"), action)
}

##' Generate menulist for context menu
##'
##' Depending on type of file, generate a different context menu.
##' @param obj gitManager object
genContextMenulist <- function(obj) {
  tr <- obj$tr
  path <- paste(tr[], collapse=.Platform$file.sep)
  filename <- svalue(tr)
  #cat("Generating menu for", filename, "at", path, "\n")

  sel <- tr$getSelection()$getSelected()
  mode <- sel$model$getValue(sel$iter, 2)$value
  staged <- sel$model$getValue(sel$iter, 3)$value
  modified <- sel$model$getValue(sel$iter, 4)$value
  action <- list(obj=obj, path=path, filename=filename,
                 mode=mode, staged=staged, modified=modified)
  
  menulist <- "Open"
  if (!is.na(mode) && mode != 0)
    menulist <- c(menulist, "Delete")
  if (is.na(mode)) { ## an untracked file
    menulist <- c(menulist, "Ignore")
  }
  if (is.na(mode) || (modified && mode != 0)) { ## a modified or untracked file
    menulist <- c(menulist, "Add")
  }
  if (!is.na(mode) && mode != 0) { ## a tracked file
    if (staged) {
      menulist <- c(menulist, "Unadd")
    }
    if (modified) {
      menulist <- c(menulist, "Reset")
    }
    menulist <- c(menulist, "Move")
  }
  if (!is.na(mode) && mode %in% c(0, 160000)) { ## repo or submodule
    menulist <- c(menulist, "Rpull", "Rpush")
    if (modified && mode != 0) {
      menulist <- c(menulist, "Rcommit")
    }
    menulist <- c(menulist, "Rcheckout", "Log", "Info")
  }
  menulist <- .genMenulist(menulist, action)
  ## try to find a Makefile
  if (!is.na(mode) && (mode %in% c(0, 160000, 40000))) {
    candidates <- paste(obj$path, path, c("Makefile", "makefile"), sep=.Platform$file.sep)
    makefile <- candidates[file.exists(candidates)][1]
  } else if (casefold(filename) == "makefile") {
    makefile <- paste(obj$path, path, sep=.Platform$file.sep)
  } else makefile <- NA
  if (!is.na(makefile)) {
    targets <- makeGetTargets(makefile)
    menulist$Make <- list()
    for (target in targets) {
      menulist$Make[[target]] <-
        gaction(target,
                icon = switch(target,
                  view=menulist$Make[[target]]$icon <- "open",
                  clean=menulist$Make[[target]]$icon <- "clear",
                  all=menulist$Make[[target]]$icon <- "execute",
                  edit=menulist$Make[[target]]$icon <- "edit", NULL),
                handler = function(...) menu("Make", ...),
                action = c(action, list(target=target)))
    }
  }
  
  menulist
}

menu <- function(type, h, ...) {
  obj <- h$action$obj
  ## first treat types that do not require a loading animation,
  ## and set the status for all other
  switch(type,
         Info = showInfo(h$action),
         LongTest = obj$status("Calling systemWithSleep..."),
         Refresh = obj$status("Refreshing..."),
         Rpull = obj$status("Running 'git rpull'..."),
         Quit = dispose(obj$w))
  ## exit if no loading animation needed
  if (type %in% c("Quit", "Log", "Info")) return()
  ## other types need a loading animation
  obj$hide()
  on.exit(obj$show())
  while(gtkEventsPending()) gtkMainIteration()
  ## now do the work
  ret <- switch(type,
                LongTest = systemWithSleep("sleep", "10"),
                Rpull = gitSystemLong("rpull"))
  ## fetch errors
  if (attr(ret, "exitcode") != 0) {
    stop("Caught bad exitcode ", attr(ret, "exitcode"), " with message:\n",
         attr(ret, "stderr"))
  }
  ## now refresh display the treeview again
  obj$refresh()
  ## update status
  switch(type,
         LongTest = obj$status("Test successfull."),
         Refresh = obj$status("Refreshed"),
         Rpull = obj$statu("Rpull finished"))
  return()
}

showInfo <- function(action) {
  obj <- action$obj
  dir <- obj$absPath(action$path)
  print(dir)
}
