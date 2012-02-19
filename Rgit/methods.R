##' Execute a git command
##'
##' Executes a git command using \code{system2()},
##' e. g., calls \code{git <args> 2>&1} in the supplied directory.
##' @param args argument to \code{git}.
##' @param dir directory to execute command in
##' @return vector of output
gitSystem <- function(args, dir) {
  ## preserve working directory
  if (!missing(dir)) {
    ldir = getwd()
    setwd(dir)
  }
  ## run command
  #print(args)
  res <- system2("git", args=args, stdout=TRUE, stderr=TRUE)
  ## restore working dir
  if (!missing(dir)) setwd(ldir)
  return(res)
}

##' git status
##'
##' Calls git status.
##' @param dir directory of repository
##' @param untracked (see git status documentation)
##' @param ignoreSubmodules (see git status documentation)
##' @param ignored show ignored files (see git status documentation)
##' @return vector of output
gitStatus <- function(dir=getwd(),
                      untracked = c("all", "no", "normal"),
                      ignoreSubmodules = c("none", "untracked", "dirty", "all"),
                      ignored = TRUE) {
  ## match arguments
  untracked <- match.arg(untracked)
  ignoreSubmodules <- match.arg(ignoreSubmodules)
  ## build command
  args = sprintf("status --porcelain --untracked=%s --ignore-submodules=%s%s",
    untracked, ignoreSubmodules, if (!ignored) " --ignored" else "")
  ## get status
  res <- gitSystem(args, dir)
  res
}

##' git ls-files
##'
##' Calls git ls-files. Converts the output into a nice
##' data.frame if required.
##' @param dir directory to list
##' @param what (see git ls-files documentation)
##' @param exclude (see git ls-files documentation)
##' @param excludeStandard (see git ls-files documentation)
##' @param errorUnmatch (see git ls-files documentation)
##' @param withTree (see git ls-files documentation)
##' @param fullName (see git ls-files documentation)
##' @param abbrev (see git ls-files documentation)
##' @param file (see git ls-files documentation)
##' @return vector of filenames,
##'   if "stage" is requested, then a data.frame
##'   with additional information is returned.
gitLsFiles <- function(dir=getwd(),
                       what=c("cached", "deleted", "modified", "others",
                         "ignored","stage", "unmerged", "killed"),
                       exclude,
                       excludeStandard = FALSE,
                       errorUnmatch = FALSE,
                       withTree,
                       fullName = FALSE,
                       abbrev,
                       file) {
  ## match arguments
  what <- match.arg(what, several.ok=!missing(what))
  whatStr = paste(" --", what, collapse="", sep="")
  ## build command
  args = sprintf("ls-files%s%s%s%s%s%s%s%s", whatStr,
    if (!missing(exclude)) sprintf(" --exclude=%s", exclude) else "",
    if (excludeStandard) " --exclude-standard" else "",
    if (errorUnmatch) " --error-unmatch" else "",
    if (!missing(withTree)) sprintf(" --with-tree=%s", withTree) else "",
    if (fullName) " --fullName" else "",
    if (!missing(abbrev)) {
      if (is.numeric(abbrev)) sprintf(" --abbrev=%i", abbrev) else " --abbrev"
    } else "",
    if (!missing(file)) sprintf(" -- %s", paste(file, collapse=" ")) else "")
  res <- gitSystem(args, dir)
  ## build data.frame if --stage was used
  if ("stage" %in% what) {
    ## split into [<tag> ]<mode> <object> <stage> <file>
    ## remove tag, mode and file
    tmp = sub("^[^ ]*? ?\\d{6} (.*?)\\t.*$", "\\1", res)
    res <- data.frame(tag = sub("^([^ ]*?) ?\\d{6}.*", "\\1", res),
                      mode = sub(".*?(\\d{6}).*", "\\1", res),
                      object = sub(" \\d*", "", tmp),
                      stage = sub(".* (\\d*)", "\\1", tmp),
                      file = sub(".*\\t", "", res),
                      stringsAsFactors = FALSE)
    ## clean up: "other" files do not have <mode> etc
    res[res[,5] == tmp , 1:4] <- ""
    res$mode <- as.numeric(res$mode)
    res$stage <- as.numeric(res$stage)
  }
  res
}

##' gitMode2Str
##'
##' Convert file mode to a string.
##' @param mode vector of mode to convert.
##' @return vector of strings.
gitMode2Str <- function(mode) {
  sapply(mode, function(x) switch(sprintf("%06i", as.integer(x)),
                                  `040000` = "Directory",
                                  `100644` = "Regular non-executable file",
                                  `100664` = "Regular non-executable group-writeable file",
                                  `100755` = "Regular executable file",
                                  `120000` = "Symbolic link",
                                  `160000` = "Submodule (Gitlink)",
                                  "unknown"))
}
