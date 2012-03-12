\name{gitListBranches}
\alias{gitListBranches}
\title{List branches}
\usage{
  gitListBranches(dir, remote = FALSE)
}
\arguments{
  \item{dir}{repository directory}

  \item{remote}{whether to show remote-tracking branches as
  well}
}
\value{
  vector of branch names
}
\description{
  Calls git branch, optionally also returns the remote
  branches (without the remote/ prefix). The currently
  active branch is given in the attribute "active".
}

