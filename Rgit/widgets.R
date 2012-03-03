## Some commonly used widgets

##' Show a message
##'
##' Shows a message, with an icon and an ok button. 
##' @param ... message to display
##' @param title of window
##' @param icon to show
##' @param handler (optional) handler to attach to the ok button
##' @param obj gitManager object
showMessageNewWindow <- function(..., title = "Message", icon="info",
                        handler=function(h,...) dispose(window), obj) {
  window <- gwindow(title)
  window$setResizable(FALSE)
  group <- ggroup(container = window, spacing=10)
  imggroup <- ggroup(horizontal=FALSE, container = group)
  gimage(icon, dirname="stock", size="dialog", container=imggroup)
  ## A group for the message and buttons
  inner.group <- ggroup(horizontal=FALSE, container = group)
  glabel(paste(unlist(list(...)), collapse="\n"), container=inner.group,
         markup=TRUE, expand=TRUE)
  ## A group to organize the buttons
  button.group <- ggroup(container = inner.group)
  ## Push buttons to right
  addSpring(button.group)
  gbutton("ok", handler=handler,
          container=button.group)
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
##' Shows a dialog while blocking the input to the parent window.
##' @param ... marked up message to display
##' @param type of message, e.g., "info", "error", ...
##' @param obj gitManager object
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
