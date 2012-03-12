\name{gitLsFiles}
\alias{gitLsFiles}
\title{git ls-files}
\usage{
  gitLsFiles(dir = getwd(),
    what = c("cached", "deleted", "modified", "others", "ignored", "stage", "unmerged", "killed"),
    exclude, excludeStandard = FALSE, errorUnmatch = FALSE,
    withTree, fullName = FALSE, abbrev, file)
}
\arguments{
  \item{dir}{directory to list}

  \item{what}{(see git ls-files documentation)}

  \item{exclude}{(see git ls-files documentation)}

  \item{excludeStandard}{(see git ls-files documentation)}

  \item{errorUnmatch}{(see git ls-files documentation)}

  \item{withTree}{(see git ls-files documentation)}

  \item{fullName}{(see git ls-files documentation)}

  \item{abbrev}{(see git ls-files documentation)}

  \item{file}{(see git ls-files documentation)}
}
\value{
  vector of filenames, if "stage" is requested, then a
  data.frame with additional information is returned.
}
\description{
  Calls git ls-files. Converts the output into a nice
  data.frame if required.
}

