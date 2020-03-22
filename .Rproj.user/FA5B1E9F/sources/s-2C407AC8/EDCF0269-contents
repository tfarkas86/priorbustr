# find alpha given beta and mode
get_alpha <- function(beta, mode) mode * beta + 1

# get parameter value for given percentile
get_percentile <- function(beta, mode, pct, ndraw=10000) {

  alpha <- get_alpha(beta, mode)
  gamma_draws <- rgamma(ndraw, rate=beta, shape=alpha)
  percentile <- quantile(gamma_draws, pct)
  return(percentile)

}

# get absolute value of difference from target value
diff_pct <- function(beta, mode, pct, target, ndraw=10000) {

  abs(get_percentile(beta, mode, pct, ndraw) - target)

}


#' Gamma Prior Elicitation
#'
#' Returns alpha and beta values for a gamma distribution given statements of the most likley value and a confidence statement for a boundary value, either less than or greater than.
#'
#' This function mathematically formalizes expert knowledge about a gamma distributed variable. Experts provide a most likely value, which is hard coded as the mode of the distribution, as well as a confidence statement about whether the value is greater than or less than some boundary values. For example, an expert might say "I believe the average height of women in Albuquerque, New Mexico is 1.6 meters, and I'm 95 percent sure it's less than 1.8 meters." The function then performs a parameter search via Brent's method to determine the shape (alpha) and scale (beta) parameters of a gamma distribution with the location and scale specified by the expert.
#'
#' @param mode The most likely value
#' @param pct Percent confidence in boundary statement
#' @param bound Boundary value
#' @param dir Direction from boundary value. Valid values are "lt" and "gt", indicating "less than" and "greater than".
#' @param ndraw Number of random draws from distribution
#' @param beta_upper Max beta value to search
#' @param plot Logical whether to plot final distribution
#'
#' @return A named numeric vector with alpha and beta values for a gamma distribution. If plot == TRUE, also plots a gamma distribution with those parameters and indicates the (analytically calculated) mode by a vertical dotted line. Discrepancy between the location of the dotted line and the peak of the curve is an indication of poor convergence.
#'
#' @examples
#' gammabustr(mode = 0.5, pct = 0.95, bound = 2, dir = "lt")
#'
#' @export
#'
gammabustr <- function(mode, pct, bound, dir = "lt",
                       ndraw = 10000, beta_upper = 100, plot = FALSE) {

  new_pct <- switch(dir, lt = pct, gt = 1 - pct)


  beta_hat <- optim(par=1, fn=diff_pct, method="Brent", mode=mode,
               pct=new_pct, target=bound, ndraw=ndraw,
               lower=0, upper=beta_upper)$par

  parms <- c(alpha=get_alpha(beta=beta_hat, mode=mode), beta=beta_hat)

  if(plot) {

    sims <- dplyr::tibble(sims=rgamma(ndraw,
                                      parms["alpha"],
                                      parms["beta"]))

    p <- ggplot2::ggplot(data=sims, ggplot2::aes(x=sims))

    print(p +
          ggplot2::geom_density() +
          ggplot2::geom_vline(xintercept=mode, linetype="dotted") +
          ggplot2::xlab("theta"))

  }

  return(parms)

}



