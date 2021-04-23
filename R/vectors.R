#' Generate vectors
#'
#' To simulate the SRT task, we started by constructing 12 vectors, 1 to stand for each of the six stimulus positions and for each of the six possible responses.
#' Each vector was of dimensionality 20 with values of +1 and -1 selected at random with $p(+1)=p(-1)=.5$.
#' @export
construct_vectors <- function(dimensionality = 20, features = c("stimuli", "responses"), max_cor = .4){

  n_features <- length(features)
  repeat{
    vectors <- t(sapply(X = rep(NA, 6 * n_features), FUN = function(x){sample(c(-1, 1), size = dimensionality, replace = TRUE)}))
    cosines <- lsa::cosine(t(vectors))
    diag(cosines) <- 0
    if(all(abs(cosines)<max_cor)) break
  }

  results <- vector(mode = "list", length = n_features)
  for (i in seq_along(results)) {
    results[[i]] <- vectors[1:6 + (i - 1) * 6, ]
  }
  names(results) <- features
  results
}
