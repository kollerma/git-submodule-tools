\name{gitR-package}
\alias{gitR-package}
\docType{package}
\title{
  gitR
}
\description{
  gitR is a graphical user interface for git with an emphasis on
  active development in submodules. It makes heavy use of the git
  submodule tools, which are designed with the same use case in mind but
  for use in the terminal.
}
\details{
 The graphical interface can be started using the function
 \code{createGUI(path)} or from the shell, by invoking \code{gitR} in
 the respective repository.

 The files are shown using a standard tree, directories and submodules
 can be expanded and collapsed to the users wishes using the little
 triangles shown on the left.

 Actions on specific files are available via context menus (right
 click). The context menus are dynamically generated and only show the
 appropriate actions for the currently selected file. Actions available
 in the toolbar act always on the whole repository. The git output of a
 certain action is not always shown. It is available on request via the
 menu \code{Help > Last git output}.

 To have a good overview on the state of the repository, several columns
 of data are shown. The most important are described below.

 \itemize{
   \item \emph{Staged}: An active/selected radio button indicates that the file
   has been staged (using `git add`).
   \item \emph{Modified}: An active radio button indicates that there are
   unstaged modifications to the file.
   \item \emph{Upstream}: This is an indicator on the upstream status of
   the repository (or submodule). \code{u=} indicates that the local and
   upstream repository are equal. \code{u+1} indicates that there is one
   local commit that has not been pushed to upstream. \code{u-1} shows
   that there is one commit in the upstream repository that has not been
   pulled to the local repository yet (using \code{fetch} one can update
   this count).
   \item \emph{Branch}: shows the branch that is currently checked out,
   detached HEADs are indicated by \code{(detached HEAD)}.
   \item \emph{Status}: Shows the status of a file in
   words. \emph{Index} is a synonym for \emph{staging area} while
   \emph{work tree} describes the set of files currently checked out.   
 }

 The status bar at the very bottom of the \code{gitR} window displays
 simple status messages. The full output of the last command run can be
 accessed using the menu \code{Help > Last git output}.

 The preferences menu can be used to show and hide certain file types
 (hidden files starting with a ., untracked and ignored files). Note that
 hidden files are are not shown also if they contain local
 modifications. The preferences are stored in the file \code{~/.gitR}. 
}
\author{
  Manuel Koller <koller.manuel@gmail.com>
}
\keyword{ git }
\keyword{ gui }
\keyword{ submodule }
\keyword{ gitR }
\seealso{
  \emph{Git submodule tools} at \url{https://github.com/kollerma/git-submodule-tools}.
}
\examples{
\dontrun{createGUI()}
}
