% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exec-kraken2-tools.R
\name{kraken_tools}
\alias{kraken_tools}
\title{KrakenTools is a suite of scripts to be used alongside the Kraken,
KrakenUniq, Kraken 2, or Bracken programs.}
\usage{
kraken_tools(script, ..., python = NULL)
}
\arguments{
\item{script}{Name of the kraken2 script. One of
\code{combine_kreports}, \code{combine_mpa}, \code{extract_kraken_reads}, \code{filter_bracken_out}, \code{fix_unmapped}, \code{kreport2krona}, \code{kreport2mpa}, \code{make_kreport}, and \code{make_ktaxonomy}}

\item{...}{<\link[rlang:dyn-dots]{dynamic dots}> Additional arguments passed to \code{script} command, empty arguments are automatically trimed. Details see: \code{kraken_tools(script, help = TRUE)}.}

\item{python}{A string of path to \code{python} command.}
}
\value{
A \link{Execute} object.
}
\description{
These scripts are designed to help Kraken users with downstream
analysis of Kraken results.
}
\seealso{
\url{https://github.com/jenniferlu717/KrakenTools}
}
