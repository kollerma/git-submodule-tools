\name{gitStatus}
\alias{gitStatus}
\title{git status}
\usage{
  gitStatus(dir = getwd(),
    untracked = c("all", "no", "normal"),
    ignoreSubmodules = c("none", "untracked", "dirty", "all"),
    ignored = FALSE)
}
\arguments{
  \item{dir}{directory of repository}

  \item{untracked}{(see git status documentation)}

  \item{ignoreSubmodules}{(see git status documentation)}

  \item{ignored}{show ignored files (see git status
  documentation)}
}
\value{
  data.frame with three columns: XY (status code), file,
  was (original name in case of rename)
}
\description{
  Calls git status.
}

