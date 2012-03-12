\name{gitSystemLong}
\alias{gitSystemLong}
\title{Execute a (long) git command}
\usage{
  gitSystemLong(args, dir, statusOnly = FALSE,
    stopOnError = FALSE)
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
  Executes a git command using \code{systemWithSleep()}, e.
  g., calls \code{git <args> 2>&1} in the supplied
  directory.
}

