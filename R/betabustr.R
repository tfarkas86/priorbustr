# find alpha given beta and mode
get_beta <- function(alpha, mode) (alpha - mode * alpha + 2 * mode - 1) / mode

# get parameter value for given percentile
get_beta_pct <- function(alpha, mode, pct) {

  qbeta(pct, alpha, get_beta(alpha, mode))

}



# get absolute value of difference from target value
diff_beta_pct <- function(alpha, mode, pct, target) {

  abs(get_beta_pct(alpha, mode, pct) - target)

}


#' Beta Prior Elicitation
#'
#' Returns alpha and beta values for a beta distribution given statements of the most likley value and a confidence statement for a boundary value, either less than or greater than.
#'
#' This function mathematically formalizes expert knowledge about a beta distributed variable. Experts provide a most likely value, which is hard coded as the mode of the distribution, as well as a confidence statement about whether the value is greater than or less than some boundary values. For example, an expert might say "I believe the average height of women in Albuquerque, New Mexico is 1.6 meters, and I'm 95 percent sure it's less than 1.8 meters." The function then performs a parameter search via Brent's method to determine the shape (alpha) and scale (beta) parameters of a gamma distribution with the location and scale specified by the expert.
#'
#' @param mode The most likely value
#' @param pct Percent confidence in boundary statement
#' @param bound Boundary value
#' @param dir Direction from boundary value. Valid values are "lt" and "gt", indicating "less than" and "greater than".
#' @param beta_upper Max alpha value to search
#' @param plot Logical whether to plot final distribution
#'
#' @return A named numeric vector with alpha and beta values for a gamma distribution. If plot == TRUE, also plots a gamma distribution with those parameters and indicates the (analytically calculated) mode by a vertical dotted line. Discrepancy between the location of the dotted line and the peak of the curve is an indication of poor convergence.
#'
#' @examples
#' beta(mode = 0.2, pct = 0.95, bound = .8, dir = "lt")
#'
#' @export
#'
betabustr <- function(mode, pct, bound, dir = "lt", plot = FALSE) {

  if(!(mode > 0 & mode < 1)) return("error")

  new_pct <- switch(dir, lt = pct, gt = 1 - pct)


  alpha_hat <- optim(par=2, fn=diff_pct, method="Brent", mode=mode,
               pct=new_pct, target=bound,
               lower=1.000, upper=100)$par

  parms <- c(alpha=alpha_hat, beta=get_beta(alpha=alpha_hat, mode=mode))

  if(plot) {

    # sims <- dplyr::tibble(sims=rbeta(ndraw,
    #                                   parms["alpha"],
    #                                   parms["beta"]))

    sims <- dplyr::tibble(sims=dbeta(seq(0, 1, length.out=1000),
                                     parms["alpha"],
                                     parms["beta"]))

    p <- ggplot2::ggplot(data=sims, ggplot2::aes(x=seq(0, 1, length.out=1000),
                                                 y=sims))

    print(p +
          ggplot2::geom_line() +
          ggplot2::geom_vline(xintercept=mode, linetype="dotted") +
          ggplot2::xlab("theta"))

  }

  return(parms)

}



