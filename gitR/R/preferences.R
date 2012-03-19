##' Generate Preferences Menulist
##'
##' Defines the label text to show for the current setting.
.genPrefMenu <-
  list(hiddenFiles=
       c(`show`="Do not show hidden files",
         `hide`="Show hidden files",
         `default`="hide"),
       untrackedFiles=
       c(`show`="Do not show untracked files",
         `hide`="Show untracked files",
         `default`="show"),
       ignoredFiles=
       c(`show`="Do not show files ignored by git",
         `hide`="Show files ignored by git",
         `default`="show"))

##' Generate Preferences Menulist
##'
##' Uses information in obj and, if empty
##' from "~/.gitR", if this does not exist
##' uses the defaults.
##' @param obj
genPrefMenu <- function(obj) {
  prefs <- if (length(obj$preferences) > 0) {
    obj$preferences
  } else if (file.exists("~/.gitR")) {
    readRDS("~/.gitR")
  } else sapply(.genPrefMenu, `[[`, "default")
  obj$preferences <- prefs
  ret <- lapply(names(prefs), function(x)
                gaction(.genPrefMenu[[x]][prefs[x]],
                        handler=function(...) menu("Prefs", ...),
                        action = list(obj=obj, pref=x)))
  names(ret) <- sapply(names(prefs), function(x)
                       .genPrefMenu[[x]][prefs[x]])
  ret
}

##' Toggle preferences entry
##'
##' Sets the preferences in gitR object and updates
##' the menu label accordingly.
##' @param action list supplied to menu() function.
togglePref <- function(action) {
  obj <- action$obj
  pref <- action$pref
  toggle <- c(show="hide", hide="show")
  value <- toggle[obj$preferences[pref]]
  ## set preference
  obj$preferences[pref] <- value
  ## update menu label
  item <- which(names(.genPrefMenu) == pref)[1]
  svalue(obj$prefMenu[[item]]) <- .genPrefMenu[[pref]][value]
  return(0)
}
