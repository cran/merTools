#' merTools: Provides methods for extracting and exploring results from merMod
#' objects in the lme4 package.
#'
#' The merTools package contains convenience tools for extracting useful
#' information from and exploring the implications of merMod objects created by
#' the lme4 package.  These convenience functions are especially useful for
#' merMod objects that take a long time to estimate due to their complexity or
#' because they are estimated on very large samples.
#'
#' See the vignettes for usage examples
#'
#' @section merMod extraction/utility functions:
#'
#' \itemize{
#'   \item \code{\link{fastdisp}}
#'   \item \code{\link{superFactor}}
#'   \item \code{\link{REextract}}
#'   \item \code{\link{REsim}}
#'   \item \code{\link{FEsim}}
#'   \item \code{\link{RMSE.merMod}}
#'   \item \code{\link{thetaExtract}}
#'   \item \code{\link{REquantile}}
#' }
#'
#' @section merMod exploration functions:
#'
#' \itemize{
#'   \item \code{\link{plotREsim}}
#'   \item \code{\link{plotFEsim}}
#'   \item \code{\link{draw}}
#'   \item \code{\link{wiggle}}
#'   \item \code{\link{subBoot}}
#'   \item \code{\link{predictInterval}}
#'   \item \code{\link{expectedRank}}
#'   \item \code{\link{REimpact}}
#'   \item \code{\link{shinyMer}}
#' }
#'
#' @name merTools
#' @docType package
NULL
