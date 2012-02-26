##' Generate menulist for regular menu
##'
##' File menu, etc
##' @param obj gitManager object
genMenulist <- function(obj) {
  action <- list(obj=obj)
  list(File=list(
         Refresh = gaction("Refresh", tooltip = "Refresh view",
           icon = "refresh",
           handler = menuRefresh, action=action),
         Quit = gaction("Quit", tooltip = "Quit git manager",
           icon = "quit",
           handler = menuQuit, action=action)
         ))
}

##' Generate menulist for toolbar
##'
##' Toolbar with repo wide tasks
genToolbar <- function(obj) {
  action <- list(obj=obj)
  list(Refresh = gaction("Refresh", tooltip = "Refresh view",
         icon = "refresh",
         handler = menuRefresh, action=action),
       Quit = gaction("Quit", tooltip = "Quit git manager",
         icon = "quit",
         handler = menuQuit, action=action))
}

##' Generate menulist for context menu
##'
##' Depending on type of file, generate a different context menu.
##' @param obj gitManager object
genContextMenulist <- function(obj) {
  tr <- obj$tr
  path <- paste(tr[], collapse="/")
  filename <- svalue(tr)
  #cat("Generating menu for", filename, "at", path, "\n")

  sel <- tr$getSelection()$getSelected()
  mode <- sel$model$getValue(sel$iter, 6)$value
  staged <- sel$model$getValue(sel$iter, 2)$value
  modified <- sel$model$getValue(sel$iter, 3)$value
  action <- list(obj=obj, path=path, filename=filename,
                 mode=mode, staged=staged, modified=modified)
  
  menulist <-
    list(Open=gaction("Open", tooltip = "Open using external program",
           icon = "open", handler = menuOpen, action = action),
         Delete=gaction("Delete", tooltip = "Delete in work tree",
           icon = "delete", handler = menuDelete, action = action))
  if (is.na(mode)) { ## an untracked file
    menulist$Ignore <- gaction("Ignore", tooltip = "Add to .gitignore",
                               icon = "stop",
                               handler = menuIgnore, action = action)
  }
  if (is.na(mode) || modified) { ## a modified or untracked file
    menulist$Add <- gaction("Add", tooltip = "Add to staging area", icon = "add",
                              handler = menuAdd, action = action)
  }
  if (!is.na(mode)) { ## a tracked file
    if (staged) {
      menulist$Unadd <- gaction("Unadd", tooltip = "Remove from stagin area",
                                icon = "remove",
                                handler = menuUnadd, action = action)
    }
    if (modified) {
      menulist$Reset <- gaction("Reset", tooltip = "Reset to version in index",
                                icon = "revert-to-saved",
                                handler = menuReset, action = action)
    }
    menulist$Move <- gaction("Move", tooltip = "Rename",
                             handler = menuMove, action = action)
  }
  if (!is.na(mode) && mode == 160000) { ## submodule
    menulist$Rpull <- gaction("Rpull",
                              tooltip = "Pull updates from server, recursively",
                              icon = "go-down",
                              handler = menuRpull, action = action)
    menulist$Rpush <- gaction("Rpush",
                              tooltip = "Push commits to server, recursively",
                              icon = "go-up",
                              handler = menuRpush, action = action)
    if (modified) {
      menulist$Rcommit <- gaction("Rcommit", tooltip = "Commit, recursively",
                                  icon = "apply",
                                  handler = menuRcommit, action = action)
    }
    menulist$Rcheckout <- gaction("Rcheckout",
                                  tooltip = "Checkout another branch/tag",
                                  handler=menuRcheckout, action = action)
    menulist$Log <- gaction("Log", tooltip = "Display commit log",
                            icon = "info",
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
      menulist$Make[[target]] <-
        gaction(target,
                icon = switch(target,
                  view=menulist$Make[[target]]$icon <- "open",
                  clean=menulist$Make[[target]]$icon <- "clear",
                  all=menulist$Make[[target]]$icon <- "execute",
                  edit=menulist$Make[[target]]$icon <- "edit", NULL),
                handler = menuMake, action = c(action, list(target=target)))
    }
  }
  
  menulist
}

menuQuit <- function(h, ...) dispose(h$action$obj$w)
menuRefresh <- function(h, action) h$action$obj$refresh()
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
