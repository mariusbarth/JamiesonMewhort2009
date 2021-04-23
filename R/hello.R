
# rbenchmark::benchmark(construct_probe(vectors, stimuli = sample(1:6, replace = TRUE, size = 2e5), responses = sample(1:6, replace = TRUE, size = 2e5)))



#' Calculate similarities
#'
#' function for one paricipant

# make_similarities <- function(probes, traces, n_features = 40, preexperimental_traces = 0){
#   similarities <- (probes %*% t(traces[, 1:n_features]))/n_features
#   similarities[(row(similarities)+preexperimental_traces)<=col(similarities)] <-0
#   dimnames(similarities) <-list("probe" = NULL, "trace" = NULL)
#   return(similarities)
# }

#' @export
make_similarities <- function(probes, traces, index_probes, index_traces, preexperimental_traces, n_features = 40) {

  if(missing(index_probes)) {
    index_probes <- seq_len(ncol(probes))
  }
  if(missing(index_traces)) {
    index_traces <- seq_len(n_features)
  }

  n_features <- length(index_probes)

  out <- (probes[, index_probes] %*% t(traces[, index_traces])) / n_features
  out[(row(out) + preexperimental_traces) <= col(out)] <- 0
  dimnames(out) <-list("probe" = NULL, "trace" = NULL)
  return(out)
}


#' Iterative Resonator
#'
#' blablabla
#'
#'
#'@export

iterative_resonator <- function(similarities, traces, probes, vectors, k = .99){

  exponent <-1
  iteration_nr <- rep(NA, length(nrow(similarities)))

  repeat{

    activations <- abs(similarities^exponent) * sign(similarities)
    # Echo content, a matrix,
    # each row repesents the echo to probe i
    # each column represents feature j of the echo(es)
    echo_content <- t(t(traces) %*% t(activations))

    # Echo intensity, a vector
    # the i-th element represents echo intensty to probe i
    echo_intensity <- as.double(rowMeans(probes * echo_content[, 1:40]))
    # response_intensity <- echo_content[, 41:60] %*% t(vectors$responses)/20
    # response_echo_similarity <- response_intensity

    #response-echo-similarity
    # response_echo_similarity <- (echo_content[, 41:60] %*% t(vectors$responses))/20
    response_echo_similarity_M <- (echo_content[, 41:60] %*% t(vectors$responses) / outer(sqrt(rowSums(echo_content[, 41:60]^2)),sqrt(rowSums(vectors$response^2))))
    # response_echo_similarity <- matrix(NA, nrow = nrow(echo_content), ncol = 6)
    # for(i in 1:nrow(echo_content)){
    #   response_echo_similarity[i, ] <- lsa::cosine(echo_content[i, 41:60], t(vectors$response))
    # }

    iteration_nr[rowSums(response_echo_similarity_M>k)>=1 & is.na(iteration_nr)] <-exponent

    if(all(rowSums(response_echo_similarity_M>k)>0)|exponent==40){
      break
    }

    exponent <- exponent + 1
  }

  response <- as.integer(apply(X = response_echo_similarity_M>k, MARGIN = 1, which))
  return(list(
    "iteration_nr" = iteration_nr
    , "response" = response
    # , "lsa" = response_echo_similarity
    , "matrix" = response_echo_similarity_M))
}


