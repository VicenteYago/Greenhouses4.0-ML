##' converts relative humidity to specific humidity
##' @title RH to SH
##' @param rh relative humidity (proportion, not \%)
##' @param Tc absolute temperature (Centigrad)
##' @param press air pressure (Pascals)
##' @export
##' @author Mike Dietze, Ankur Desai
##' @aliases rh2rv
rh2qair <- function(rh, Tc, press = 102300) {
  stopifnot(T[!is.na(T)] >= 0)
  es <- 6.112 * exp((17.67 * Tc) / (Tc + 243.5))
  e <- rh * es
  p_mb <- press / 100
  qair <- (0.622 * e) / (p_mb - (0.378 * e))
  ## qair <- rh * 2.541e6 * exp(-5415.0 / T) * 18/29
  return(qair)
} # rh2qair
