% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/handle_arg.R
\name{handle_arg}
\alias{handle_arg}
\title{Deliver arguments of command}
\usage{
handle_arg(tag, value, format = "\%s", sep = " ")
}
\arguments{
\item{tag}{A string specifying argument tag, like "-i", "-o".}

\item{value}{Value passed to the argument.}

\item{format}{The format of the value, details see \link{sprintf}.}

\item{sep}{A character string used to separate "tag" and "value", usually " "
or "=".}
}
\value{
A string or NULL which can be used directly by \link{run_command}.
}
\description{
Deliver arguments of command
}