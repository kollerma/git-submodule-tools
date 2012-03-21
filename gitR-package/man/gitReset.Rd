\name{gitReset}
\alias{gitReset}
\title{Git reset}
\usage{
  gitReset(commit,
    mode = c("soft", "mixed", "hard", "merge", "keep"),
    dir)
}
\arguments{
  \item{commit}{to reset HEAD to}

  \item{mode}{of reset}

  \item{dir}{repository directory}

  \item{git}{output}
}
\description{
  Reset current branch HEAD to \code{commit} and possibly
  updates the index and work-tree depending on \code{mode}.
}

