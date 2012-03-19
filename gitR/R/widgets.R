#######################################################
## Some commonly used widgets and helper functions   ##
#######################################################

##' Show a message
##'
##' Shows a message, with an icon and an ok button. 
##' @param ... message to display
##' @param title of window
##' @param icon to show
##' @param handler (optional) handler to attach to the ok button
##' @param obj gitR object
showMessageNewWindow <- function(..., title = "Message", icon="info",
                                 use.scrollwindow = FALSE, resizable = use.scrollwindow,
                                 handler=function(h,...) dispose(window),
                                 obj) {
  window <- gwindow(title, visible=FALSE)
  group <- ggroup(container = window, spacing=10)
  imggroup <- ggroup(horizontal=FALSE, container = group)
  gimage(icon, dirname="stock", size="dialog", container=imggroup)
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group, expand=TRUE)
  inner.group2 <-
    if (use.scrollwindow) ggroup(horizontal=FALSE, container = inner.group,
                                 use.scrollwindow = use.scrollwindow,
                                 expand = TRUE)
    else gframe(horizontal = FALSE, container = inner.group, expand=TRUE)
  inner.group2$modifyBg("normal", "white") ## FIXME: this does not work
  label <- glabel(paste(unlist(list(...)), collapse="\n"), container=inner.group2,
                  markup=TRUE, expand=TRUE) 
  ## A group to organize the buttons
  button.group <- ggroup(container = inner.group)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("close", handler=handler,
          container=button.group)
  ## if the window is not resizable, so make it just a little bit larger
  if (!resizable) {
    req <- window$sizeRequest()$requisition
    window$setSizeRequest(req$width+10, req$height+10)
  } else window$resize(650, 600)
  window$setResizable(resizable)
  visible(window) <- TRUE
  label$setSelectable(TRUE)
  return()
}
## alternative using gmessage (markup does not work):
## showMessageNewWindow <- function(..., title = "Message", icon="info",
##                                  obj) {
##   msg <- gmessage(paste(unlist(list(...)), collapse="\n"),
##                   title = title, icon=icon, parent = obj$w, markup=TRUE)
##   return()
## }

##' Show Dialog
##'
##' Shows a dbraialog while blocking the input to the parent window.
##' @param ... marked up message to display
##' @param type of message, e.g., "info", "error", ...
##' @param obj gitR object
showMessage <- function(..., type = "info", obj = obj) {
  dialog <- gtkMessageDialogNewWithMarkup(obj$getWindow(),
                                          "destroy-with-parent",
                                          type, "ok", NULL,
                                          show=FALSE)
  markup <- paste(unlist(list(...)), collapse="\n")
  dialog$setMarkup(markup)
  dialog$run()
  dialog$destroy()
}

##' Choice
##'
##' Simple reference class to retrieve input in a dialog box
##' more easily.
##' @name choice-class
##' @rdname choice-class
##' @slot choice character vector of input
setRefClass("choice",
            fields = list(choice = "character"))

##' Escape a string
##'
##' Escapes a string for display within message that is
##' in Pango text markup language format.
##' @param string to escape
##' @return escaped string
escape <- function(string) {
  gsub(">", "&gt;", gsub("<", "&lt;", gsub("&", "&amp;", string)))
}

#######################################################
## Widgets used to get user input / show output      ##
#######################################################

##' Add submodule dialog
##'
##' Displays the dialog for adding submodules and
##' returns the url and the path of the submodules.
##' @param obj gitR object
##' @param where to be put into title.
##' @return vector with url and path (or NULL if cancelled)
showAddSubmodule <- function(obj, where) {
  grp <- ggroup(horizontal = FALSE)
  glabel("Please enter the submodule url and the path where it should be added.",
         container = grp)
  grp1 <- ggroup(container = grp)
  glabel("Url:", container = grp1)
  addSpring(grp1)
  url <- gedit(container = grp1, width=50)
  grp2 <- ggroup(container = grp)
  glabel("Path:", container = grp2)
  path <- gedit(container = grp2, width=50)
  addHandlerFocus(path, handler = function(h, ...) if(nchar(svalue(h$obj)) == 0) {
    svalue(h$obj) <- sub("\\.git$", "", sub(".*(:|/)", "", svalue(url)))
  })
  t <- new("choice")
  dialog <- gbasicdialog(paste("Add submodule in", where), parent=obj$w,
                         handler = function(...) t$choice <- c(url=svalue(url), path=svalue(path)))
  add(dialog, grp)
  test <- visible(dialog, set=TRUE)
  if (test) return(t$choice) else return(FALSE)
}

##' Widget to select branch or tag
##'
##' Displays a tree view with two branches:
##' branches and tags.
##' @param dir repository directory
##' @param obj gitR obj
##' @param where to be put into title.
##' @return selected branch or NULL (on cancel)
selectBranchTag <- function(dir, obj, where) {
  branches <- gitListBranches(dir, remote=TRUE)
  active = attr(branches, "active")
  if (length(branches) > 1) branches <- mixedsort(branches)
  tags <- gitListTags(dir)
  if (length(tags) > 1) tags <- mixedsort(tags)
  gp <- ggroup(horizontal=FALSE, expand=TRUE)
  glabel("Select branch or tag to checkout:", container=gp, anchor=c(0,1))
  scrollwindow <- ggroup(horizontal=FALSE, container=gp,
                         use.scrollwindow = TRUE, expand=TRUE)
  rb <- gradio(c(branches, tags),
               selected = which(branches == active),
               container=scrollwindow, expand=TRUE)
  sel <- new("choice")
  ret <- gbasicdialog(title=paste("Rcheckout in", where), widget=gp, parent=obj$w,
                      handler = function(h, ...) sel$choice <- svalue(rb))
  if (ret) return(sel$choice) else return(FALSE)
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

##' Display git log
##'
##' Displays a nicely formatted git log in a separate window
##' @param dir repository directory
##' @param obj gitR object
showGitLog <- function(dir, obj) {
  message <- escape(gitLog(dir))
  ## format log
  message <- sub("^(commit [a-z0-9]+)$", '<span foreground="brown">\\1</span>', message)
  showMessageNewWindow(message, title=paste("Git log of", dir),
                       use.scrollwindow=TRUE, obj=obj)
}

##' Display last git output
##'
##' Displays the output of the last git command in
##' a separate window.
##' @param obj gitR object
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

##' Display reset dialog
##'
##' Displays dialog to choose how to call git reset.
##' @param obj gitR object
##' @param where to be put into title.
showGitReset <- function(obj, where) {
  grp <- ggroup(horizontal=FALSE)
  glabel("Please enter commit to reset HEAD to.", container=grp)
  lay <- glayout(container=grp)
  lay[1,1] <- "Commit:"
  lay[1,2] <- gedit("HEAD~1", container=lay)
  lay[2,1] <- "Mode:"
  lay[2,2] <- gdroplist(c("soft", "mixed", "hard", "merge", "keep"), selected=1,
                        container=lay)
  t <- new("choice")
  dialog <- gbasicdialog(paste("Reset HEAD in", where), parent=obj$w,
                         handler = function(...)
                         t$choice <- c(commit=svalue(lay[1,2]), mode=svalue(lay[2,2])))
  add(dialog, grp)
  test <- visible(dialog, set=TRUE)
  if (test) return(t$choice) else return(FALSE) 
}

##' Display commit dialog
##'
##' Enter a commit message and commit options.
##' @param obj gitR object
##' @param where to be put into title.
showGitCommit <- function(obj, where) {
  grp <- ggroup(horizontal=FALSE)
  glabel("Please enter a commit message.", container=grp)
  msg <- gtext(height=100, expand=TRUE, container=grp)
  all <- gcheckbox("Automatically stage modified and delete files.",
                   checked=TRUE, container=grp)
  rec <- gcheckbox("Commit recursively in any submodules.",
                   checked=TRUE, container=grp)
  t <- new("choice")
  dialog <- gbasicdialog(paste("Commit in", where), parent=obj$w,
                         handler = function(...)
                         t$choice <- c(message=svalue(msg), all=svalue(all),
                                       rrecursive=svalue(rec)))
  add(dialog, grp)
  test <- visible(dialog, set=TRUE)
  out <- list()
  out$message <- unname(t$choice["message"])
  out$all <- as.logical(t$choice["all"])
  out$recursive <- as.logical(t$choice["rrecursive"])
  if (test) return(out) else return(FALSE)
}

##' About gitR
##'
##' Show about gitR window.
##' @param obj gitR object
showAbout <- function(obj) {
  showMessageNewWindow("<b>About gitR</b>

gitR is an open-source graphical git client with focus on active development in submodules.

Licence: GPL v2
Copyright (c) 2012 Manuel Koller", title="About gitR")
}

##' Show Rdiff output
##'
##' Show Rdiff output for the current repository in a separate window.
##' @param obj gitR object.
showRdiff <- function(obj)
  showMessageNewWindow(gitSystem("rdiff", obj$absPath()),
                       title=sprintf("Rdiff in %s", obj$repo))

##' Show man page
##'
##' Shows a man page in a separate window.
##' @param man what man page
showMan <- function(man) {
  cmd <- paste(c("man", shQuote(man), "| col -b"), collapse=" ")
  showMessageNewWindow(escape(system(cmd, intern=TRUE)),
                       title=sprintf("man %s", man),
                       use.scrollwindow = TRUE)
}
