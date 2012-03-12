\name{systemWithSleep}
\alias{systemWithSleep}
\title{Non blocking system()}
\usage{
  systemWithSleep(cmd, args = c(), env = c(),
    separateStderr = TRUE)
}
\arguments{
  \item{cmd}{command to run}

  \item{args}{string of arguments}

  \item{env}{environment variables}

  \item{separateStderr}{whether to separate stderr from
  stdout}
}
\value{
  vector of strings (stdout) with attribute exitcode that
  contains the exit code of the cmd and stderr attribute
  (if not separated).
}
\description{
  Exectute an external command without blocking the gui.
}

