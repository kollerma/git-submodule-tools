\name{gitBranch}
\alias{gitBranch}
\title{Get Branch}
\usage{
  gitBranch(dir, describe = FALSE)
}
\arguments{
  \item{dir}{directory of respository}

  \item{describe}{try to describe (like master~3)?}
}
\value{
  string
}
\description{
  Get current branch name, some descriptive string or
  nothing if head is detached and describe is FALSE
}

