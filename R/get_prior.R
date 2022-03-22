# find alpha given beta and mode
get_beta <- function(alpha, mode, family) {

  if(family == "gamma") {
    return((alpha - 1) / mode)
  } else if(family=="beta") {
    return((alpha - mode * alpha + 2 * mode - 1) / mode)
  }
}

# get parameter value for given percentile
get_pct <- function(alpha, mode, pct, family) {

  if(family == "beta") {
    qbeta(pct, alpha, get_beta(alpha, mode, family))
  } else if(family == "gamma") {
    qgamma(pct, alpha, get_beta(alpha, mode, family))
  }
}

# get absolute value of difference from target value
diff_pct <- function(alpha, mode, pct, target, family) {

  abs(get_pct(alpha, mode, pct, family) - target)

}

diff_beta_01 <- function(parms, pct, target) {

  abs(qbeta(pct, parms[1], parms[2]) - target)

}

#' Prior Elicitation
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
build_prior <- function(mode, bound, family,
                      pct = 0.95, dir = "lt", plot = FALSE) {

  new_pct <- switch(dir, lt = pct, gt = 1 - pct)

  # if gamma
  alpha_hat <- optim(par=1, fn=diff_pct, method="Brent",
                     lower = 0.001, upper=100,
                     mode=mode, pct=new_pct,
                     target=bound, family=family)$par
  # if beta, 0 < mode < 1
  alpha_hat <- optim(par=2, fn=diff_pct, method="Brent",
                   lower = 1.001, upper=100,
                   mode=mode, pct=new_pct,
                   target=bound, family=family)$par
  # if beta, mode == 0
  alpha_hat <- optim(par=c(.5, 2), fn=diff_beta_01,
                     mode=mode, pct=new_pct,
                     target=bound, family=family)$par
  # if beta, mode == 1
  alpha_hat <- optim(par=c(2, .5), fn=diff_beta_01,
                     mode=mode, pct=new_pct,
                     target=bound, family=family)$par

    }
  } & mode %in% c(0, 1)) {

  }
                      optim(par=c(1, 1), fn=diff_beta_01, method="Brent",
                            lower = 0.001, upper=100,
                            mode=mode, pct=new_pct,
                            target=bound, family=family)$par,
                      optim(par=1, fn=diff_pct, method="Brent",
                            lower = 0.001, upper=100,
                            mode=mode, pct=new_pct,
                            target=bound, family=family)$par)

  parms <- c(alpha=alpha_hat,
             beta=get_beta(alpha=alpha_hat, mode=mode, family=family))

  if(plot) {

    if(family == "beta") {
      sims <- dplyr::tibble(sims=dbeta(seq(0, 1, length.out=1000),
                                       parms["alpha"],
                                       parms["beta"]))
      p <- ggplot2::ggplot(data=sims, ggplot2::aes(x=seq(0, 1, length.out=1000),
                                                   y=sims))
    } else if (family == "gamma") {
      bounds <- qgamma(c(.001, .999), parms["alpha"], parms["beta"])
      sims <- dplyr::tibble(sims=dgamma(seq(bounds[1], bounds[2],
                                            length.out=1000),
                                       parms["alpha"],
                                       parms["beta"]))
      p <- ggplot2::ggplot(data=sims, ggplot2::aes(x=seq(bounds[1], bounds[2],
                                                         length.out=1000),
                                                   y=sims))
    }

    print(p +
          ggplot2::geom_line() +
          ggplot2::geom_vline(xintercept=mode, linetype="dotted") +
          ggplot2::xlab("theta"))

  }

  return(parms)

}



