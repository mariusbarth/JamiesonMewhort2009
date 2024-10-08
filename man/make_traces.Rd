% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/traces.R
\name{construct_traces}
\alias{construct_traces}
\alias{make_traces}
\title{Construct Memory Traces}
\usage{
construct_traces(vectors, L = 0.7, S, pR, R)

make_traces(vectors, L = 0.7, ...)
}
\description{
When participants made a response, we assume that they updated memory with a
record of the current stimulus, the response required by that stimulus, and
the context provided by their own response on the previous trial.
Hence, each trial was represented by a vector of dimensionality 60 constructed
by concatenating the vectors for the current stimulus, the response associated
with that stimulus, and the response on the previous trial.
}
