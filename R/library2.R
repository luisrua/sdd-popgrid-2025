

#' Convenience function for loading a package and installing if necessaryt
#'
#' @param package package to be installed, must be a character string
#' @param quietly passed through to \code{require}
#' @param github a github user and repo, passed through to install_github. If
#'   blank, package is installed from CRAN
#' @param ... passed through to \code{require}    
#' @details if package cannot be loaded, this function will try to install it.
#'   If github is not provided then it will try to install from CRAN, otherwise
#'   it will install from the GitHub repo provided
library2 <- function(package, quietly = TRUE, github = NULL, ...){
  if(!require(package, character.only = TRUE, quietly = quietly, ...)){
    if(is.null(github)){
      install.packages(package)
    } else {
      remotes::install_github(github)
    }
    require(package, character.only = TRUE, quietly = quietly, ...)
  }
  
}
