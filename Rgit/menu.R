##' Generate menulist
##'
##' Simple function to collect all the actions in one place.
##' This method is called by genMenulist and similar commands.
##' @param what vector of actions requested
##' @param action action list to append to the actions
##' @return list that can be used in gmenu, etc.
.genMenulist <- function(what, action)
  list(Add=gaction("Add", tooltip = "Add to staging area",
         icon = "add",
         handler = function(...) menu("Add", ...), action = action),
       `Add submodule`=gaction("Add submodule", tooltip = "Add a submodule",
         icon = "jump-to",
         handler = function(...) menu("AddSubmodule", ...), action = action),
       Clean=gaction("Clean", tooltip = "Remove untracked files",
         icon = "clear",
         handler = function(...) menu("Clean", ...), action = action),
       Delete=gaction("Delete", tooltip = "Delete in work tree",
         icon = "delete",
         handler = function(...) menu("Delete", ...), action = action),
       Ignore=gaction("Ignore", tooltip = "Add to .gitignore",
         icon = "stop",
         handler = function(...) menu("Ignore", ...), action = action),
       Info=gaction("Info", tooltip = "Display info",
         icon = "info",
         handler = function(...) menu("Info", ...), action=action),
       `Last git output`=gaction("Last git output", tooltip = "Output of last git command",
         handler = function(...) menu("LastGitOutput", ...), action=action),
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
       Rfetch=gaction("Rfetch", tooltip = "Fetch updates from server",
         icon = "goto-bottom",
         handler = function(...) menu("Rfetch", ...), action=action),
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
  list(File=.genMenulist(c("Refresh", "Quit"), action),
       Debug=.genMenulist(c("LongCMD", "Last git output"), action))
}

##' Generate menulist for toolbar
##'
##' Toolbar with repo wide tasks
genToolbar <- function(obj) {
  action <- list(obj=obj)
  .genMenulist(c("Rfetch", "Rpull", "Rpush", "Refresh", "Quit"), action)
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
    menulist <- c(menulist, "Ignore", "Delete")
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
    menulist <- c(menulist, "Rcheckout", "Clean", "Add submodule", "Log", "Info")
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

##' Menu handler
##'
##' This function handles all the buttons pressed in a menu.
##' It first sets the status (indicating that the action is performed)
##' or executes a short action immediately. In the former case the TreeView
##' is then replaced by a spinner and the action is performed. In the end,
##' the status is updated with a sucess message. It also captures errors and
##' shows them in a dialog box.
##' @param type type of menu button pressed
##' @param h user info that contains the action list, etc.
##' @return NULL
menu <- function(type, h, ...) {
  obj <- h$action$obj
  rpath <- if (is.null(h$action$path)) gui$repo else h$action$path
  path <- obj$absPath(rpath)
  ## first treat types that do not require a loading animation,
  ## and set the status for all other
  val <- switch(type,
                Info = showInfo(h$action),
                LastGitOutput = showGitOutput(obj),
                Log = showGitLog(path, obj),
                LongTest = obj$status("Calling systemWithSleep..."),
                Refresh = obj$status("Refreshing..."),
                Rfetch = obj$status("Running 'git rfetch' in", rpath, "..."),
                Rpull = obj$status("Running 'git rpull' in", rpath, "..."),
                Rpush = obj$status("Running 'git rpush' in", rpath, "..."),
                Rcheckout = {
                  obj$status("Select branch or tag to checkout...")
                  selectBranchTag(path, obj)
                },
                Quit = dispose(obj$w),
                stop("Unknown action type: ", type))
  ## exit if no loading animation needed
  if (type %in% c("Quit", "LastGitOutput", "Log", "Info")) return()
  ## other types need a loading animation
  obj$hide()
  on.exit({ obj$refresh(); obj$show() })
  while(gtkEventsPending()) gtkMainIteration()
  ## now do the work
  ret <- switch(type,
                LongTest = systemWithSleep("sleep", "10"),
                Rfetch = gitSystemLong("rfetch", path),
                Rpull = gitSystemLong("rpull", path),
                Rpush = gitSystemLong("rpush", path),
                Rcheckout = {
                  if (!is.null(val)) {
                    obj$status("Checking out", val, "...")
                    gitSystemLong(paste("rcheckout", val), path)
                  } else {
                    obj$status("Rcheckout cancelled")
                  }
                })
  if (!is.null(ret)) obj$lastout <- ret
  ## fetch errors
  if (!is.null(attr(ret, "exitcode")) && attr(ret, "exitcode") != 0) {
    showMessage("<b>Git Error</b>\n",
                escape(attr(ret, "stderr")), type="error",
                obj=obj)
    obj$status("Error")
    return()
  }
  ## update status
  switch(type,
         LongTest = obj$status("Test successful."),
         Refresh = obj$status("Refreshed."),
         Rfetch = obj$status("Rfetch in", rpath, "sucessfully finished."),
         Rpull = obj$status("Rpull in", rpath, "successfully finished."),
         Rpush = obj$status("Rpush in", rpath, "successfully finished."),
         Rcheckout = {
           if (!is.null(val)) obj$status("Rcheckout successful.")
         })
  return()
}

##' Escape a string
##'
##' Escapes a string for display within message that is
##' in Pango text markup language format.
##' @param string to escape
##' @return escaped string
escape <- function(string) {
  ## FIXME: escape all non ascii codes as well
  gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", string)))
}

##' Show Info about repository
##'
##' Shows basic info about a repository in a separate window.
##' @param action action list containing obj
showInfo <- function(action) {
  obj <- action$obj
  dir <- obj$absPath(action$path)
  showMessageNewWindow(paste("<b>Info about '", escape(action$path), "':</b>\n", sep=""),
              "\n<b>Status:</b>\n",escape(gitSystem("status", dir)),
              "\n<b>Remotes:</b>\n",escape(gitSystem("remote -v", dir)), obj=obj)
  ## showMessage(paste("<big>Info about '", escape(action$path), "':</big>\n", sep=""),
  ##             "\n<b>Status:</b>\n",escape(gitSystem("status", dir)),
  ##             "\n<b>Remote:</b>\n",escape(gitSystem("remote -v", dir)), obj=obj)
}

##' Display last git output
##'
##' Displays the output of the last git command in
##' a separate window.
##' @param obj gitManager object
showGitOutput <- function(obj) {
  if (length(obj$lastout) == 0 && is.null(attributes(obj$lastout))) {
    showMessage("No git output available yet.", obj=obj)
    return()
  }
  output <- c("<b>Command:</b>\n",
              paste(escape(attr(obj$lastout, "cmd")), "(in",
                    escape(attr(obj$lastout, "dir")), ")"),
              "\n<b>Stdout:</b>\n", escape(obj$lastout),
              "\n<b>Stderr:</b>\n", escape(attr(obj$lastout, "stderr")),
              paste("\n<b>Exit code:</b>", attr(obj$lastout, "exitcode")))
  showMessageNewWindow(output, title="Last git output", obj=obj)
}

##' Display git log
##'
##' Displays a nicely formatted git log in a separate window
##' @param dir repository directory
##' @param obj gitManager object
showGitLog <- function(dir, obj) {
  message <- gitLog(dir)
  showMessageNewWindow(escape(message), title=paste("Git log of", dir),
                       use.scrollwindow=TRUE, obj=obj)
}
