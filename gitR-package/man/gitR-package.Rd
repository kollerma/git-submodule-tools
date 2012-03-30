\name{rlmer-Package}
\alias{rlmer-Package}
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
 
}