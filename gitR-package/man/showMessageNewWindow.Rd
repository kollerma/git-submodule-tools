\name{showMessageNewWindow}
\alias{showMessageNewWindow}
\title{Show a message}
\usage{
  showMessageNewWindow(..., title = "Message",
    icon = "info", use.scrollwindow = FALSE,
    resizable = use.scrollwindow,
    handler = function(h, ...) dispose(window), obj)
}
\arguments{
  \item{...}{message to display}

  \item{title}{of window}

  \item{icon}{to show}

  \item{handler}{(optional) handler to attach to the ok
  button}

  \item{obj}{gitR object}
}
\description{
  Shows a message, with an icon and an ok button.
}

