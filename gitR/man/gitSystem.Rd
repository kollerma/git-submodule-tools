\name{gitSystem}
\alias{gitSystem}
\title{Execute a git command}
\usage{
  gitSystem(args, dir, statusOnly = FALSE,
    stopOnError = !statusOnly)
}
\arguments{
  \item{args}{argument to \code{git}.}

  \item{dir}{directory to execute command in}

  \item{statusOnly}{only check the status of the command}

  \item{stopOnError}{whether to stop on error}
}
\value{
  vector of output
}
\description{
  Executes a git command using \code{system2()}, e. g.,
  calls \code{git <args> 2>&1} in the supplied directory.
}

