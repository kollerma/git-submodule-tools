\name{menu}
\alias{menu}
\title{Menu handler}
\usage{
  menu(type, h, ...)
}
\arguments{
  \item{type}{type of menu button pressed}

  \item{h}{user info that contains the action list, etc.}
}
\value{
  NULL
}
\description{
  This function handles all the buttons pressed in a menu.
  It first sets the status (indicating that the action is
  performed) or executes a short action immediately. In the
  former case the TreeView is then replaced by a spinner
  and the action is performed. In the end, the status is
  updated with a sucess message. It also captures errors
  and shows them in a dialog box.
}

