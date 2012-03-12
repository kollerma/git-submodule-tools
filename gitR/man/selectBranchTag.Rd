\name{selectBranchTag}
\alias{selectBranchTag}
\title{Widget to select branch or tag}
\usage{
  selectBranchTag(dir, obj, where)
}
\arguments{
  \item{dir}{repository directory}

  \item{obj}{gitR obj}

  \item{where}{to be put into title.}
}
\value{
  selected branch or NULL (on cancel)
}
\description{
  Displays a tree view with two branches: branches and
  tags.
}

