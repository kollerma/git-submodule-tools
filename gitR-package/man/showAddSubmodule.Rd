\name{showAddSubmodule}
\alias{showAddSubmodule}
\title{Add submodule dialog}
\usage{
  showAddSubmodule(obj, where, ...)
}
\arguments{
  \item{obj}{gitR object}

  \item{where}{to be put into title.}

  \item{dir}{absolute path to directory where the submodule
  should be added (ignored).}
}
\value{
  vector with url and path (or NULL if cancelled)
}
\description{
  Displays the dialog for adding submodules and returns the
  url and the path of the submodules.
}

