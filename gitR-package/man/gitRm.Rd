\name{gitRm}
\alias{gitRm}
\title{Git rm}
\usage{
  gitRm(file, dir, recursive = FALSE, force = FALSE,
    statusOnly = FALSE, stopOnError = FALSE)
}
\arguments{
  \item{file}{file to delete}

  \item{dir}{repository directory}

  \item{recursive}{remove recursively}

  \item{force}{removal}
}
\value{
  exit code
}
\description{
  Delete a file from the work tree.
}

