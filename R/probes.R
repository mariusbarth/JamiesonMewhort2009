#' Concatenate Probe Content
#'
#' When a stimulus is presented, we assume that the participant attempts to retrieve
#' the correct response given a probe composed of the current stimulus and the
#' participant's own response from the previous trial.
#' To do so, we applied Minerva 2's mechanism for cued recall; that is, we used
#' S_i R_{i-1} as a prompt to recover R_i.
#'
#' @param vectors Matrix containing the vectors to store in memory
#' @param S Stimulus presented on trial i
#' @param pR Response made on trial i-1
#'
#' @export

construct_probes <- function(vectors, S = 1:2, pR = 1:2){


  cbind(vectors$stimuli[S, ], vectors$responses[pR, ])

}
