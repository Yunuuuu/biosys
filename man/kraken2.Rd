% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exec-kraken2.R
\name{kraken2}
\alias{kraken2}
\title{Running Kraken2}
\usage{
kraken2(
  fq1,
  ...,
  fq2 = NULL,
  ofile = "kraken_output.txt",
  report = "kraken_report.txt",
  classified_out = NULL,
  unclassified_out = NULL,
  odir = getwd(),
  kraken2 = NULL
)
}
\arguments{
\item{fq1, fq2}{A string of fastq file path.}

\item{...}{<\link[rlang:dyn-dots]{dynamic dots}> Additional arguments passed to \code{kraken2} command, empty arguments are automatically trimed. Details see: \code{kraken2(help = TRUE)}}

\item{ofile}{A string of path to save kraken2 output.}

\item{report}{A string of path to save kraken2 report.}

\item{classified_out}{A string of path to save classified sequences, which
should be a fastq file.}

\item{unclassified_out}{A string of path to save unclassified sequences,
which should be a fastq file.}

\item{odir}{A string of path to the output directory.}

\item{kraken2}{A string of path to \code{kraken2} command.}
}
\value{
A \link{Execute} object.
}
\description{
Kraken is a taxonomic sequence classifier that assigns taxonomic labels to
DNA sequences. Kraken examines the k-mers within a query sequence and uses
the information within those k-mers to query a database. That database maps
k-mers to the lowest common ancestor (LCA) of all genomes known to contain a
given k-mer.
}
\seealso{
\itemize{
\item \url{https://github.com/DerrickWood/kraken2/wiki/Manual}
\item \url{https://benlangmead.github.io/aws-indexes/k2}
}
}
