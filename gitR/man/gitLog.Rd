\name{gitLog}
\alias{gitLog}
\title{Git log}
\usage{
  gitLog(dir, n = 10, stat = TRUE)
}
\arguments{
  \item{dir}{repository directory}

  \item{n}{number of commits to show}

  \item{stat}{whether to add --stat argument}
}
\value{
  vector of lines
}
\description{
  Retrieve git log
}

