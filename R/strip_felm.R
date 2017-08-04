#' Strip off data-intensive felm objects.
#' @param felm.est Object from lfe::felm call
#' @return Returns (much smaller) lfe::felm object. 
#' @export
strip_felm <- function(felm.est) {
  # Strip out all of the fatty bits from felm to save it.
  # Stargazer can still use the remainder.
  felm.est$residuals <- NULL
  felm.est$weights <- NULL
  felm.est$response <- NULL
  felm.est$fitted.values <- NULL
  felm.est$r.residuals <- NULL
  felm.est$na.action <- NULL
  felm.est$fe <- NULL
  felm.est$cfactor <- NULL
  felm.est$X <- NULL
  felm.est$cX <- NULL
  felm.est$cY <- NULL
  felm.est$clustervar <- sapply(felm.est$clustervar, function(x) x <- NULL)
  # IV only
  felm.est$c.response <- NULL
  felm.est$iv.residuals <- NULL
  felm.est$r.iv.residuals <- NULL
  felm.est$r.iv.response <- NULL
  felm.est$stage1$residuals <- NULL
  felm.est$stage1$response <- NULL
  felm.est$stage1$fitted.values <- NULL
  felm.est$stage1$fe <- NULL
  felm.est$stage1$na.action <- NULL
  felm.est$stage1$cfactor <- NULL
  felm.est$stage1$X <- NULL
  felm.est$stage1$cX <- NULL
  felm.est$stage1$cY <- NULL
  felm.est$stage1$ivx <- NULL
  felm.est$stage1$ivy <- NULL
  felm.est$stage1$r.residuals <- NULL
  felm.est$stage1$clustervar <- sapply(felm.est$clustervar, function(x) x <- NULL)
  felm.est$stage1$centred.exo <- NULL
  return(felm.est)
}
