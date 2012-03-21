\name{gitCommit}
\alias{gitCommit}
\title{Git commit}
\usage{
  gitCommit(msg, dir, all = FALSE, recursive = FALSE)
}
\arguments{
  \item{msg}{commit message}

  \item{dir}{repository directory}

  \item{all}{whether to commit all modified and deleted
  files.}

  \item{recursive}{whether to commit in all submodules as
  well.}
}
\description{
  Commit (recursively) in directory.
}

