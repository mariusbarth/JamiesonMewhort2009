#' Construct Memory Traces
#'
#' When participants made a response, we assume that they updated memory with a
#' record of the current stimulus, the response required by that stimulus, and
#' the context provided by their own response on the previous trial.
#' Hence, each trial was represented by a vector of dimensionality 60 constructed
#' by concatenating the vectors for the current stimulus, the response associated
#' with that stimulus, and the response on the previous trial.
#'
#' @rdname make_traces
#' @export

construct_traces <- function(vectors, L = .7, S, pR, R){
  # obtain number of traces to generate
  n_traces <- length(S)
  # Concatenate memory vectors (perfect memory)
  traces <-cbind(vectors$stimuli[S, ], vectors$responses[pR, ], vectors$responses[R, ])
  # Sample learning rate
  learning <- matrix(sample(0:1, size = n_traces * ncol(traces), replace = TRUE, prob = c(1 - L, L)), nrow = n_traces)
  # Degrade memory traces by (non-)learning rate
  traces <- traces * learning
  dimnames(traces) <- list("trace" = NULL, "feature" = NULL)
  return(traces)
}

#' @rdname make_traces
#' @export

make_traces <-function(vectors, L = .7, ...) {

  features <- list(...)
  y <- list()

  for (i in names(features)) {
    feature_i <- list()
    for (j in seq_along(features[[i]])) {
      feature_i[[j]] <- vectors[[i]][features[[i]][[j]], , drop = FALSE]
    }
    y[[i]] <- do.call("cbind", feature_i)
  }
  y <- do.call("cbind", y)
  y[is.na(y)] <- 0
  y
}
