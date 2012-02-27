##' Generate menulist for regular menu
##'
##' File menu, etc
##' @param obj gitManager object
genMenulist <- function(obj) {
  action <- list(obj=obj)
  list(File=list(
         Refresh = gaction("Refresh", tooltip = "Refresh view",
           icon = "refresh",
           handler = function(...) menu("Refresh", ...), action=action),
         Quit = gaction("Quit", tooltip = "Quit git manager",
           icon = "quit",
           handler = function(...) menu("Quit", ...), action=action)
         ))
}

##' Generate menulist for toolbar
##'
##' Toolbar with repo wide tasks
genToolbar <- function(obj) {
  action <- list(obj=obj)
  list(Refresh = gaction("Refresh", tooltip = "Refresh view",
         icon = "refresh",
         handler = function(...) menu("Refresh", ...), action=action),
       Quit = gaction("Quit", tooltip = "Quit git manager",
         icon = "quit",
         handler = function(...) menu("Quit", ...), action=action))
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
  
  menulist <-
    list(Open=gaction("Open", tooltip = "Open using external program",
           icon = "open", handler = function(...) menu("Open", ...), action = action))
  if (!is.na(mode) && mode != 0)
    menulist$Delete=gaction("Delete", tooltip = "Delete in work tree",
      icon = "delete", handler = function(...) menu("Delete", ...), action = action)
  if (is.na(mode)) { ## an untracked file
    menulist$Ignore <- gaction("Ignore", tooltip = "Add to .gitignore",
                               icon = "stop",
                               handler = function(...) menu("Ignore", ...), action = action)
  }
  if (is.na(mode) || (modified && mode != 0)) { ## a modified or untracked file
    menulist$Add <- gaction("Add", tooltip = "Add to staging area", icon = "add",
                              handler = function(...) menu("Add", ...), action = action)
  }
  if (!is.na(mode) && mode != 0) { ## a tracked file
    if (staged) {
      menulist$Unadd <- gaction("Unadd", tooltip = "Remove from stagin area",
                                icon = "remove",
                                handler = function(...) menu("Unadd", ...), action = action)
    }
    if (modified) {
      menulist$Reset <- gaction("Reset", tooltip = "Reset to version in index",
                                icon = "revert-to-saved",
                                handler = function(...) menu("Reset", ...), action = action)
    }
    menulist$Move <- gaction("Move", tooltip = "Rename",
                             handler = function(...) menu("Move", ...), action = action)
  }
  if (!is.na(mode) && mode %in% c(0, 160000)) { ## repo or submodule
    menulist$Rpull <- gaction("Rpull",
                              tooltip = "Pull updates from server, recursively",
                              icon = "go-down",
                              handler = function(...) menu("Rpull", ...), action = action)
    menulist$Rpush <- gaction("Rpush",
                              tooltip = "Push commits to server, recursively",
                              icon = "go-up",
                              handler = function(...) menu("Rpush", ...), action = action)
    if (modified && mode != 0) {
      menulist$Rcommit <- gaction("Rcommit", tooltip = "Commit, recursively",
                                  icon = "apply",
                                  handler = function(...) menu("Rcommit", ...), action = action)
    }
    menulist$Rcheckout <- gaction("Rcheckout",
                                  tooltip = "Checkout another branch/tag",
                                  handler=function(...) menu("Rcheckout", ...), action = action)
    menulist$Log <- gaction("Log", tooltip = "Display commit log",
                            icon = "info",
                            handler = function(...) menu("Log", ...), action = action)
  }
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
                handler = function(...) menu("Make", ...), action = c(action, list(target=target)))
    }
  }
  
  menulist
}

menu <- function(type, h, ...) {
  obj <- h$action$obj
  ## first treat types that do not require a loading animation,
  ## and set the status for all other
  switch(type,
         Quit = dispose(obj$w),
         Refresh = obj$status("Refreshing..."))
  ## exit if no loading animation needed
  if (type %in% c("Quit", "Log")) return()
  ## other types need a loading animation
  obj$hide()
  on.exit(obj$show())
  while(gtkEventsPending()) gtkMainIteration()
  ## now do the work
  ## switch(type,
  ##        )
  ## now refresh display the treeview again
  obj$refresh()
  ## update status
  switch(type,
         Refresh = obj$status("Refreshed"))
  return()
}
