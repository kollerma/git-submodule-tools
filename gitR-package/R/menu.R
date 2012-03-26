##' Generate menulist
##'
##' Simple function to collect all the actions in one place.
##' This method is called by genMenulist and similar commands.
##' @param what vector of actions requested
##' @param action action list to append to the actions
##' @return list that can be used in gmenu, etc.
.genMenulist <- function(what, action)
  list(`About gitR`=gaction("About gitR", tooltip = "About gitR",
         icon = "about",
         handler = function(...) menu("About", ...), action = action),
       Add=gaction("Add", tooltip = "Add to staging area",
         icon = "add",
         handler = function(...) menu("Add", ...), action = action),
       `Add submodule`=gaction("Add submodule", tooltip = "Add a submodule",
         icon = "jump-to",
         handler = function(...) menu("AddSubmodule", ...), action = action),
       Clean=gaction("Clean", tooltip = "Remove untracked files",
         icon = "clear",
         handler = function(...) menu("Clean", ...), action = action),
       Commit=gaction("Commit", tooltip = "Commit (recursively)",
         icon = "apply",
         handler = function(...) menu("Commit", ...), action = action),
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
       `Open another repository`=gaction("Open another repository", icon = "open",
         handler = function(...) menu("OpenRepo", ...), action = action),
       Quit = gaction("Quit", tooltip = "Quit git manager",
         icon = "quit",
         handler = function(...) menu("Quit", ...), action = action),
       Rdiff = gaction("Rdiff", tooltip = "Show recursive diff",
         icon = "find",
         handler = function(...) menu("Rdiff", ...), action = action),
       Refresh = gaction("Refresh", tooltip = "Refresh view",
         icon = "refresh",
         handler = function(...) menu("Refresh", ...), action = action),
       Reset=gaction("Reset", tooltip = "Reset to version in index",
         icon = "revert-to-saved",
         handler = function(...) menu("Reset", ...), action = action),
       Rcheckout=gaction("Rcheckout",
         tooltip = "Checkout another branch/tag",
         handler=function(...) menu("Rcheckout", ...), action = action),
       Rfetch=gaction("Rfetch", tooltip = "Fetch updates from server",
         icon = "goto-bottom",
         handler = function(...) menu("Rfetch", ...), action=action),
       Rpull=gaction("Rpull", tooltip = "Pull updates from server, recursively",
         icon = "go-down",
         handler = function(...) menu("Rpull", ...), action = action),
       Rpush=gaction("Rpush", tooltip = "Push commits to server, recursively",
         icon = "go-up",
         handler = function(...) menu("Rpush", ...), action = action),
       Separator=list(separator=TRUE),
       Unadd=gaction("Unadd", tooltip = "Remove from stagin area",
         icon = "remove",
         handler = function(...) menu("Unadd", ...), action = action)
       )[what]

##' Generate git manual pages menu
##'
##' Generates a submenu to access the git man pages.
##' @param obj gitR object
.genGitManMenu <- function(obj) {
  all <- .genMenulist(action=list())
  gitMan <-  list(Add="git-add",
                  `Add submodule`="git-submodule",
                  Clean="git-clean",
                  Commit="git-commit",
                  Delete="git-rm",
                  Log="git-log",
                  Move="git-mv",
                  Rdiff="git-rdiff",
                  Reset="git-reset",
                  Rcheckout="git-rcheckout",
                  Rfetch="git-rfetch",
                  Rpull="git-rpull",
                  Rpush="git-rpush",
                  Unadd="git-reset")
  ret <- lapply(names(gitMan), function(x)
                gaction(x, handler=function(...) menu("Man", ...),
                        action = list(obj=obj, man=gitMan[[x]])))
  names(ret) <- names(gitMan)
  ret
}

##' Generate menulist for regular menu
##'
##' File menu, etc
##' @param obj gitR object
genMenulist <- function(obj) {
  action <- list(obj=obj)
  list(File=.genMenulist(c("Open another repository", "Refresh", "Quit"), action),
       Preferences=genPrefMenu(obj),
       Help=c(list(`Git Help`=.genGitManMenu(obj)),
              .genMenulist(c("Last git output", "About gitR"), action)))
}

##' Generate menulist for toolbar
##'
##' Toolbar with repo wide tasks
genToolbar <- function(obj) {
  action <- list(obj=obj)
  .genMenulist(c("Rpull", "Commit", "Rpush", "Separator",
                 "Rfetch", "Rdiff",
                 "Separator", "Refresh", "Quit"), action)
}

##' Generate menulist for context menu
##'
##' Depending on type of file, generate a different context menu.
##' @param obj gitR object
genContextMenulist <- function(obj) {
  tr <- obj$tr
  path <- paste(tr[], collapse=.Platform$file.sep)
  filename <- svalue(tr)
  #cat("Generating menu for", filename, "at", path, "\n")

  sel <- tr$getSelection()$getSelected()
  mode <- sel$model$getValue(sel$iter, 2)$value
  staged <- sel$model$getValue(sel$iter, 3)$value
  modified <- sel$model$getValue(sel$iter, 4)$value
  status <- sel$model$getValue(sel$iter, 7)$value
  action <- list(obj=obj, path=path, filename=filename,
                 mode=mode, staged=staged, modified=modified,
                 status = status)  
  menulist <- "Open"
  if (is.na(mode) && status == gitStatus2Str("??")) { ## an untracked file
    menulist <- c(menulist, "Add", "Ignore")
  }
  if (!is.na(mode) && mode != 0) { ## a tracked file or submodule
    if (modified) {
      menulist <- c(menulist, "Add")
    }
    if (staged) {
      menulist <- c(menulist, "Unadd")
    }
  }
  if (!is.na(mode) && mode %in% c(0, 160000)) { ## repo or submodule
    if (modified) {
      menulist <- c(menulist, "Commit")
    }
    menulist <- c(menulist, "Rpull", "Rpush", "Rcheckout", "Clean")
  }
  if (!is.na(mode) && mode != 0) { ## a tracked file
    if (modified) {
      menulist <- c(menulist, "Reset")
    }
  }
  if (!grepl(paste("(", gitStatus2Str(c("D ", " D")), ")", collapse="|", sep=""),
             status)) { ## not a deleted file
    menulist <- c(menulist, "Move", "Delete")
  } else {
     menulist <- unique(c(menulist, "Reset"))
  }
  if (!is.na(mode) && (mode %in% c(0, 160000, 40000)))
    menulist <- c(menulist, "Add submodule")
  if (!is.na(mode) && mode %in% c(0, 160000)) { ## repo or submodule
    menulist <- c(menulist, "Log", "Info")
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
  #str(h$action, 1)
  rpath <- if (is.null(h$action$path)) obj$repo else h$action$path
  path <- obj$absPath(rpath)
  dir <- sub("[^/]*$", "", path)
  ## first treat types that do not require a loading animation,
  ## and set the status for all other
  force <- FALSE
  status <- "Aborted"
  val <- switch(type,
                About = showAbout(obj),
                Add = {
                  if (grepl(gitStatus2Str(" D"), h$action$status))
                    ## need to use git rm, not git add
                    gitRm(h$action$file, dir, statusOnly=TRUE, stopOnError=TRUE) 
                  else
                    gitAdd(h$action$file, dir)
                },
                AddSubmodule = showAddSubmodule(obj, rpath, path),
                Clean = {
                  msg <- gitSystem("clean -n -d", path)
                  if (length(msg) > 0) {
                    gconfirm(paste(c("Git clean...\n", msg), collapse="\n"))
                  } else {
                    status <- "Nothing to clean"
                    FALSE
                  }
                },
                Delete = if (!is.na(h$action$mode) && h$action$mode == 40000) {
                  gconfirm(sprintf("Really delete '%s' and all contained files?", rpath))
                } else if (!is.na(h$action$mode) &&
                           grepl(paste("(", gitStatus2Str(c("M ", " M", "A ")), ")",
                                       collapse="|", sep=""),
                                 h$action$status)) {
                  force <- TRUE
                  gconfirm(sprintf("Really delete '%s'?\n Local modifications will be lost!",
                                   rpath))
                } else {
                  gconfirm(sprintf("Really delete '%s'?", rpath))
                },
                Ignore = obj$status(sprintf("Adding '%s' to .gitignore...", rpath)),
                Info = showInfo(h$action),
                LastGitOutput = showGitOutput(obj),
                Log = showGitLog(path, obj),
                LongTest = obj$status("Calling systemWithSleep..."),
                Make = {
                  obj$status("Running make", h$action$target,"in the background.")
                  ldir <- getwd()
                  setwd(if (file.info(path)$isdir) path else dir)
                  system2("make", args=h$action$target, wait=FALSE)
                  setwd(ldir)
                },
                Man = showMan(h$action$man),
                Move = ginput("Please enter new name", text=h$action$filename,
                  title="Move", icon="question", parent=obj$w),
                Prefs = togglePref(h$action),
                Open = system2("open", path, wait=FALSE), ## FIXME: not portable
                OpenRepo = {
                  dir <- gfile("Select repository to open.", type="selectdir",
                               parent=obj$w)
                  if (!is.na(dir))
                    createGUI(dir)
                },
                Rdiff = showRdiff(obj),
                Refresh = obj$status("Refreshing..."),
                Reset = {
                  if (!is.na(h$action$mode) && h$action$mode %in% c(160000, 0)) {
                    showGitReset(obj, rpath)
                  } else {
                    obj$status("Resetting", rpath, "...")
                  }
                },
                Rfetch = obj$status("Running 'git rfetch' in", rpath, "..."),
                Rpull = obj$status("Running 'git rpull' in", rpath, "..."),
                Rpush = obj$status("Running 'git rpush' in", rpath, "..."),
                Rcheckout = {
                  obj$status("Select branch or tag to checkout...")
                  selectBranchTag(path, obj, rpath)
                },
                Commit = showGitCommit(obj, rpath),
                Quit = obj$quit(),
                Unadd = gitUnadd(h$action$file, dir),
                stop("Unknown action type: ", type))
  ## exit if no loading animation needed
  if (type %in% c("About", "Quit", "LastGitOutput", "Rdiff",
                  "Log", "Info", "Open", "OpenRepo", "Make", "Man")) return()
  if (!is.null(val) && ((is.logical(val) && !val) || is.na(val))) {
    obj$status(status)
    return()
  }
  ## other types need a loading animation
  obj$hide()
  on.exit({ obj$refresh(); obj$show() })
  while(gtkEventsPending()) gtkMainIteration()
  ## now do the work
  ret <- switch(type,
                Add = {
                  if (val == 0) obj$status("Added file", h$action$file, "sucessfully in", dir)
                  else obj$status("Error adding file", h$action$file, "in", dir)
                },
                AddSubmodule = {
                  obj$status("Adding submodule", val["path"], "in", rpath, "...")
                  gitSubmoduleAdd(val["url"], path, val["path"])
                },
                Clean = {
                  obj$status("Running git clean...")
                  gitSystemLong("clean -d -f -f", path)
                },
                Commit = {
                  obj$status("Running git rcommit...")
                  gitCommit(val$message, path, all=val$all, recursive=val$recursive)
                },
                Delete = {
                  obj$status("Removing", rpath, "...")
                  switch(as.character(h$action$mode),
                         `40000`= { ## remove only untracked dir by unlink()
                           if (grepl(gitStatus2Str("??"), h$action$status))
                             try(unlink(path, recursive=TRUE))
                           else gitRm(h$action$filename, dir, recursive=TRUE, force=force,
                                      stopOnError=FALSE)
                           },
                         `NA`= try(unlink(path)),
                         `160000`= { ## submodule
                           gitSubmoduleRm(h$action$filename, dir)
                         }, ## else
                         gitRm(h$action$filename, dir, force=force, stopOnError=FALSE)
                         )
                },
                Ignore = gitIgnore(path),
                LongTest = systemWithSleep("sleep", "10"),
                Move = {
                  if (val == h$action$file) {
                    obj$status("Aborted")
                    return()
                  } else if (file.exists(paste(dir, val, sep="/"))) {
                    try(stop("Error, file '", val, "' exists already"))
                  } else {
                    obj$status(sprintf("Moving '%s' to '%s'...", h$action$filename, val))
                    switch(as.character(h$action$mode),
                           `40000`= { ## move only untracked dir by file.rename()
                             if (grepl(gitStatus2Str("??"), h$action$status))
                               try(file.rename(path,paste(dir, val, sep="/")))
                             else gitMv(h$action$filename, val, dir)
                           },
                           `NA` = try(file.rename(path,paste(dir, val, sep="/"))),
                           `160000`= { ## submodule
                             gitSubmoduleMv(h$action$filename, val, dir)
                           }, ## else
                           gitMv(h$action$filename, val, dir))
                  }
                },
                Reset = {
                  if (!is.na(h$action$mode) && h$action$mode %in% c(160000, 0)) {
                    obj$status("Resetting", rpath, "to", val["commit"], "...")
                    gitReset(val["commit"], val["mode"], path)
                  } else {
                    gitSystem(c("reset HEAD", shQuote(h$action$file)), dir, statusOnly=TRUE)
                    gitSystemLong(c("checkout", shQuote(h$action$file)), dir)
                  }
                },
                Rfetch = gitSystemLong("rfetch", path),
                Rpull = gitSystemLong("rpull", path),
                Rpush = gitSystemLong("rpush", path),
                Rcheckout = {
                  obj$status("Checking out", val, "...")
                  gitSystemLong(paste("rcheckout", val), path)
                },
                Unadd = {
                  if (val == 0) obj$status("Reset file", h$action$file, "successfully in", dir)
                  else obj$status("Error resetting file", h$action$file, "in", dir)
                  })
  if (!is.null(ret) && is.character(ret)) obj$lastout <- ret[]
  ## catch errors
  if (!is.null(attr(ret, "exitcode")) && attr(ret, "exitcode") != 0) {
    err <- attr(ret, "stderr")
    if (is.null(err)) err <- ret
    showMessage("<b>Git Error</b>\n", escape(err), type="error", obj=obj)
    obj$status("Error")
    return()
  } else if (is(ret, "try-error")) {
    showMessage("<b>Error</b>\n",
                escape(ret), type="error",
                obj=obj)
    obj$status("Error")
    return()
  }
  ## update status
  switch(type,
         AddSubmodule = obj$status("Submodule", val["path"], "successfully added."),
         Clean = obj$status("Cleaned repository successfully."),
         Delete = obj$status("Removed", rpath, "successfully."),
         Commit = obj$status("Commit successful."),
         Ignore = obj$status("Added", rpath, "to .gitignore"),
         LongTest = obj$status("Test successful."),
         Move = obj$status(sprintf("Moved '%s' to '%s' successfully.", h$action$filename, val)),
         Refresh = obj$status("Refreshed."),
         Reset = obj$status("Reset successful."),
         Rfetch = obj$status("Rfetch in", rpath, "successfully finished."),
         Rpull = obj$status("Rpull in", rpath, "successfully finished."),
         Rpush = obj$status("Rpush in", rpath, "successfully finished."),
         Rcheckout = obj$status("Rcheckout successful."))
  return()
}

