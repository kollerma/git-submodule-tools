##' Execute a git command
##'
##' Executes a git command using \code{system2()},
##' e. g., calls \code{git <args> 2>&1} in the supplied directory.
##' @param args argument to \code{git}.
##' @param dir directory to execute command in
##' @param statusOnly only check the status of the command
##' @param stopOnError whether to stop on error
##' @return vector of output
##' @export
gitSystem <- function(args, dir, statusOnly=FALSE, stopOnError=!statusOnly) {
  ## preserve working directory
  if (!missing(dir)) {
    ldir = getwd()
    setwd(dir)
  }
  ## run command
  #print(args)
  res <- tryCatch(system2("git", args=args, stdout=TRUE, stderr=TRUE),
                  warning=function(w) w)
  #print(res)
  ## restore working dir
  if (!missing(dir)) setwd(ldir)
  ## if args contain --exit-code, then return exit code
  if (statusOnly) {
    if (is(res, "simpleWarning")) {
      return(as.numeric(sub(".*?(\\d+)$", "\\1", res$message)))
    } else return(0)
  } else {
    if (is(res, "simpleWarning")) {
      if (stopOnError) {
        stop("tried git ", args, "\nBut got the following error:\n",
             res$message)
      } else {
        msg <- res$message
        res <- ""
        attr(res, "stderr") <- msg
        attr(res, "exitcode") <- as.numeric(sub(".*?(\\d+)$", "\\1", msg))
      }
    }
    attr(res, "dir") <- dir
    attr(res, "cmd") <- paste(c(shQuote("git"), args), collapse=" ")
    return(res)
  }
}

##' Non blocking system()
##'
##' Exectute an external command without blocking the gui.
##' @param cmd command to run
##' @param args string of arguments
##' @param env environment variables
##' @param separateStderr whether to separate stderr from stdout
##' @return vector of strings (stdout) with attribute exitcode that contains
##'   the exit code of the cmd and stderr attribute (if not separated).
##' @export
systemWithSleep <- function(cmd, args = c(), env = c(), separateStderr = TRUE) {
    outfile <- tempfile()
    errfile <- if (separateStderr) tempfile() else c()
    codefile <- tempfile()
    on.exit(unlink(c(errfile, outfile, codefile)))
    ## open pipe
    con <- pipe(paste(c(env, shQuote(cmd), args, ">", outfile,
                      if (separateStderr) "2>" else "2>&1", errfile,
                      "&& wait $!; echo $? >", codefile), collapse=" "), "r")
    ## test whether it is still running
    while(!file.exists(codefile)) Sys.sleep(0.01)
    ## get exit code
    exitcode <- as.numeric(readLines(codefile))
    ## close pipe
    close(con)

    ret <- readLines(outfile)
    if (separateStderr) attr(ret, "stderr") <- readLines(errfile)
    attr(ret, "exitcode") <- exitcode
    attr(ret, "cmd") <- paste(c(env, shQuote(cmd), args), collapse=" ")
    ret
}

##' Execute a (long) git command
##'
##' Executes a git command using \code{systemWithSleep()},
##' e. g., calls \code{git <args> 2>&1} in the supplied directory.
##' @param args argument to \code{git}.
##' @param dir directory to execute command in
##' @param statusOnly only check the status of the command
##' @param stopOnError whether to stop on error
##' @return vector of output
##' @export
gitSystemLong <- function(args, dir, statusOnly=FALSE, stopOnError=FALSE) {
  ## preserve working directory
  if (!missing(dir)) {
    ldir = getwd()
    setwd(dir)
  }
  ## run command
  res <- systemWithSleep("git", args=args, separateStderr=TRUE)
  ## restore working dir
  if (!missing(dir)) {
    setwd(ldir)
    attr(res, "dir") <- dir
  }
  if (stopOnError && attr(res, "exitcode") != 0) {
    stop("tried git ", args, "\nBut got the status", attr(res, "exitcode"),
         "and the following error message:\n",
         attr(res, "stderr"))
  }
  if (statusOnly) {
    return(attr(res, "exitcode"))
  } else return(res)
}

##' git status
##'
##' Calls git status.
##' @param dir directory of repository
##' @param untracked (see git status documentation)
##' @param ignoreSubmodules (see git status documentation)
##' @param ignored show ignored files (see git status documentation)
##' @return data.frame with three columns:
##'   XY (status code), file, was (original name in case of rename)
##' @export
gitStatus <- function(dir=getwd(),
                      untracked = c("all", "no", "normal"),
                      ignoreSubmodules = c("none", "untracked", "dirty", "all"),
                      ignored = FALSE) {
  if (dir == "./") dir <- getwd()
  ## get toplevel directory (show only subdirectory if dir is not tl)
  tl <- gitToplevel(dir)
  ## convert dir into an absolute path
  if (!grepl("^/", dir)) dir <- paste(getwd(), dir, sep="/")
  ## and the relative path (to filter for files)
  rdir <- sub("/$", "", sub("^/", "", sub(tl, "", dir))) 
  ## match arguments
  untracked <- match.arg(untracked)
  ignoreSubmodules <- match.arg(ignoreSubmodules)
  ## build command
  args = sprintf("status --porcelain --untracked=%s --ignore-submodules=%s%s",
    untracked, ignoreSubmodules, if (ignored) " --ignored" else "")
  ## get status
  status <- gitSystem(args, dir)
  ## filter
  if (nchar(rdir) > 0) {
    filter <- sprintf("( |\")%s/.+", rdir)
    status <- grep(filter, status, value=TRUE)
    status <- gsub(filter, "\\1", status)
  }
  if (length(status) == 0) return(data.frame())
  ## convert into data.frame
  lclean <- function(lst) {
    ret <- matrix("", length(lst), 2)
    colnames(ret) <- c("file", "was")
    ret[,1] <- sapply(lst, function(x) if (length(x) > 1) x[2] else x[1])
    ret[,2] <- sapply(lst, function(x) if (length(x) > 1) x[1] else "")
    gsub("\\\"", "", ret)
  }
  res <- data.frame(XY=substring(status, 1, 2),
                    lclean(strsplit(substring(status, 4), " -> ")),
                    stringsAsFactors=FALSE)
  res
}

##' dirty repository?
##'
##' Checks if a repository is dirty.
##' @param dir directory to check
##' @return TRUE or FALSE
##' @export
gitIsDirty <- function(dir) {
  gitSystem("diff --no-ext-diff --quiet --exit-code", dir, statusOnly = TRUE) > 0
}

##' Get Branch
##'
##' Get current branch name, some descriptive string
##' or nothing if head is detached and describe is FALSE
##' @param dir directory of respository
##' @param describe try to describe (like master~3)?
##' @return string
##' @export
gitBranch <- function(dir, describe=FALSE) {
  if (describe)
    return(gitSystem("describe --contains --always --all HEAD", dir))
  ## find out if HEAD is attached
  if (gitSystem("symbolic-ref -q HEAD", dir, statusOnly=TRUE) > 0)
    return("")
  ## HEAD is attached, return symbolic ref
  gitSystem("describe --contains --all HEAD", dir)
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
##' @export
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
  if (length(res) == 0) return(data.frame())
  ## build data.frame if --stage was used
  if ("stage" %in% what) {
    ## split into [<tag> ]<mode> <object> <stage> <file>
    ## remove tag, mode and file
    tmp = sub("^[^ ]*? ?\\d{6} (.*?)\\t.*$", "\\1", res)
    res <- data.frame(tag = sub("^([^ ]*?) ?\\d{6}.*", "\\1", res),
                      mode = sub(".*?(\\d{6}) .*", "\\1", res),
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
##' @export
gitMode2Str <- function(mode) {
  sapply(mode, function(x) switch(sprintf("%06i", as.integer(x)),
                                  `040000` = "Directory",
                                  `100644` = "Regular non-executable file",
                                  `100664` = "Regular non-executable group-writeable file",
                                  `100755` = "Regular executable file",
                                  `120000` = "Symbolic link",
                                  `160000` = "Submodule (gitlink)",
                                  `000000` = "Git repository",
                                  "unknown"))
}

##' gitStatus2Str
##'
##' Convert status code (XZ) to a human readable string.
##' @param status vector of status codes to convert.
##' @return vector of strings.
##' @export
gitStatus2Str <- function(status) {
  ## from the git documentation (man git-status)
  ##   X          Y     Meaning
  ## -------------------------------------------------
  ##           [MD]   not updated
  ## M        [ MD]   updated in index
  ## A        [ MD]   added to index
  ## D         [ M]   deleted from index
  ## R        [ MD]   renamed in index
  ## C        [ MD]   copied in index
  ## [MARC]           index and work tree matches
  ## [ MARC]     M    work tree changed since index
  ## [ MARC]     D    deleted in work tree
  ## -------------------------------------------------
  ## D           D    unmerged, both deleted
  ## A           U    unmerged, added by us
  ## U           D    unmerged, deleted by them
  ## U           A    unmerged, added by them
  ## D           U    unmerged, deleted by us
  ## A           A    unmerged, both added
  ## U           U    unmerged, both modified
  ## -------------------------------------------------
  ## ?           ?    untracked
  ## !           !    ignored
  ## -------------------------------------------------
  ret <- sapply(status, function(xy)
                switch(xy,
                       `DD` = "unmerged, both deleted",
                       `AU` = "unmerged, added by us",
                       `UD` = "unmerged, deleted by them",
                       `UA` = "unmerged, added by them",
                       `DU` = "unmerged, deleted by us",
                       `AA` = "unmerged, both added",
                       `UU` = "unmerged, both modified",
                       `??` = "untracked",
                       `!!` = "ignored",
                       ## otherwise
                       {
                         if (xy %in% c("", " ")) xy <- "  "
                         paste(switch(substring(xy, 1, 1),
                                      M="modified in index",
                                      A="added to index",
                                      D="deleted from index",
                                      R="renamed in index",
                                      C="copied in index",
                                      ` `="", "unknown code"),
                               switch(substring(xy, 2, 2),
                                      M="modified in work tree",
                                      D="deleted in work tree",
                                      ` `="", "unknown code"), sep=", ")
                       }))
 ret <- sub("^, ", "", ret)
 sub(", $", "", ret)
}

##' Get Submodule Status String
##'
##' Get informative submodule status string from git status
##' @param directory repository directory
##' @param submodules requested submodules
##' @export
gitSubmoduleStatus <- function(directory, submodules) {
  str <- gitSystem(paste("status",shQuote(submodules),collapse=" "), directory)
  ret <- character(0)
  ## get all lines in question
  for (submodule in submodules) {
    val <- grep(sprintf("\\#.*?:\\s+%s\\s+\\(", submodule), str, value=TRUE)
    val <- sub(".*\\(([^)]+)\\)\\s*$", "\\1", val[length(val)])
    ret <- c(ret, if (length(val) > 0) val else "")
  }
  ret
}

##' Show upstream status
##'
##' Return number of unpushed and unpulled commits
##' @param dir repository directory
##' @return string like in git completion bash
##' @export
gitUpstream <- function(dir) {
  count <- gitSystem("rev-list --count --left-right \\@{upstream}...HEAD",
                     dir, stopOnError=FALSE)
  if (length(count) == 0) return("")
  ## equal to upstream
  if (count == "0\t0") return("u=")
  ## ahead of upstream
  if (grepl("^0\t", count)) return(sub("^0\t", "u+", count))
  ## behind upstream
  if (grepl("\t0", count)) return(sub("(\\d+)\t0", "u-\\1", count))
  ## diverged from upstream
  return(sub("(\\d+)\t(\\d+)","u+\\2-\\1", count))
}

##' List branches
##'
##' Calls git branch, optionally also returns the
##' remote branches (without the remote/ prefix).
##' The currently active branch is given in the
##' attribute "active".
##' @param dir repository directory
##' @param remote whether to show remote-tracking branches as well
##' @return vector of branch names
##' @export
gitListBranches <- function(dir, remote=FALSE) {
  branches <- gitSystem(paste("branch", if (remote) "-a" else c()), dir)
  if (remote) {
    ## remove HEAD pointer
    branches <- grep("/HEAD ->", branches, invert=TRUE, value=TRUE)
    ## remove remotes/.../
    branches <- sub("remotes/.*?/", "", branches)
  }
  ## find active branch
  activebranch <- grepl("^\\*", branches)
  ## remove leading spaces
  branches <- sub("^\\*? +", "", branches)
  ## find unique branches
  ubranches <- unique(branches)
  attr(ubranches, "active") <- branches[activebranch]
  ubranches
}

##' List tags
##'
##' Calls git tag
##' @param dir repository directory
##' @return vector of tag names
##' @export
gitListTags <- function(dir) {
  gitSystem("tag", dir)
}

##' Git log
##'
##' Retrieve git log
##' @param dir repository directory
##' @param n number of commits to show
##' @param stat whether to add --stat argument
##' @return vector of lines
##' @export
gitLog <- function(dir, n=10, stat=TRUE) {
  cmd <- paste("log -",n,if (stat) " --stat" else c(), sep="")
  gitSystem(cmd, dir)
}

##' Git add
##'
##' Add a file to the staging area.
##' @param file to add
##' @param dir repository directory
##' @return exit code
##' @export
gitAdd <- function(file, dir) {
  gitSystem(c("add", shQuote(file)), dir, statusOnly=TRUE)
}

##' Git unadd
##'
##' Remove a file from the staging area.
##' @param file file to unadd
##' @param dir repository directory
##' @return exit code
##' @export
gitUnadd <- function(file, dir) {
  gitSystem(c("reset -q HEAD", shQuote(file)), dir, statusOnly=TRUE)
}

##' Git reset
##'
##' Reset current branch HEAD to \code{commit} and possibly
##' updates the index and work-tree depending on \code{mode}.
##' @param commit to reset HEAD to
##' @param mode of reset
##' @param dir repository directory
##' @param git output
##' @export
gitReset <- function(commit, mode = c("soft", "mixed", "hard", "merge", "keep"),
                     dir) {
  mode <- match.arg(mode)
  ## using gitSystemLong to get better error handling
  gitSystemLong(c("reset", paste("--", mode, sep=""), commit), dir)
}

##' Git rm
##'
##' Delete a file from the work tree.
##' @param file file to delete
##' @param dir repository directory
##' @param recursive remove recursively
##' @param force removal
##' @return exit code
##' @export
gitRm <- function(file, dir, recursive=FALSE, force=FALSE,
                  statusOnly=FALSE, stopOnError=FALSE) {
  gitSystem(c("rm", shQuote(file), if (recursive) "-r" else c(),
              if (force) "-f" else c()), dir,
            statusOnly=statusOnly, stopOnError=stopOnError)
}

##' Git add submodule
##'
##' Add a submodule.
##' @param url submodule url
##' @param dir repository directory
##' @param path submodule path (optional)
##' @return git output
##' @export
gitSubmoduleAdd <- function(url, dir, path = c()) {
  ## need to create submodule in toplevel directory
  tl <- gitToplevel(dir)
  subdir <- sub("/$", "", sub("^/", "", sub(tl, "", dir)))
  if (nchar(subdir) > 0)
    path <- paste(subdir, path, sep=.Platform$file.sep)
  gitSystemLong(c("submodule add", shQuote(c(url, path))), tl)
}

##' Git rm submodule
##'
##' Remove a submodule.
##' @param path submodule path
##' @param dir repository directory
##' @return git output
##' @export
gitSubmoduleRm <- function(path, dir) {
  gitSystemLong(c("rm-submodule", shQuote(path)), dir)
}

##' Git mv submodule
##'
##' Move a submodule.
##' @param source from
##' @param destination to
##' @param repository directory
##' @return git output
##' @export
gitSubmoduleMv <- function(source, dest, dir) {
  ## need to move submodule in toplevel directory
  tl <- gitToplevel(dir)
  subdir <- sub("/$", "", sub("^/", "", sub(tl, "", dir)))
  if (nchar(subdir) > 0) {
    source <- paste(subdir, source, sep=.Platform$file.sep)
    dest <- paste(subdir, dest, sep=.Platform$file.sep)
  }
  gitSystemLong(c("mv-submodule", shQuote(source), shQuote(dest)), tl)
}

##' Get repository root
##'
##' Returns the toplevel directory of the given repository.
##' @param dir repository directory
##' @export
gitToplevel <- function(dir) {
  gitSystem("rev-parse --show-toplevel", dir)
}

##' Ignore a .gitignore
##'
##' Adds the given file to .gitignore in the repository's
##' toplevel directory. Give an absolute path.
##' @param path to add to .gitignore
##' @export
gitIgnore <- function(path) {
  dir <- if (file.info(path)$isdir) path else sub("[^/]*$", "", path)
  tl <- gitToplevel(dir)
  try(cat("\n",sub(dir, "", path), file=paste(tl, ".gitignore", sep="/"), append=TRUE,sep=""))
}

##' Git mv
##'
##' Move a file or a directory
##' @param source from
##' @param destination to
##' @param dir repository directory
##' @export
gitMv <- function(source, dest, dir) {
  gitSystem(c("mv", shQuote(source), shQuote(dest)), dir)
}

##' Git commit
##'
##' Commit (recursively) in directory.
##' @param msg commit message
##' @param dir repository directory
##' @param all whether to commit all modified and deleted files.
##' @param recursive whether to commit in all submodules as well.
##' @export
gitCommit <- function(msg, dir, all=FALSE, recursive=FALSE) {
  gitSystemLong(c(if (recursive) "rcommit" else "commit",
                  if (all) "--all" else c(),
                  "-m", shQuote(msg)), dir)
}

##' Get a list of targets from a Makefile
##'
##' Reads the makefile and returns a vector of targets.
##' @param file path to Makefile
##' @return vector of targets
makeGetTargets <- function(file) {
  makefile <- readLines(file, -1)
  targets <- grep("^[^#[:space:]].*:", makefile, value=TRUE)
  ## drop some
  targets <- grep("(^\\.|:=|%|^\\$)", targets, value=TRUE, invert=TRUE)
  sub(" *:.*$", "", targets)
}
