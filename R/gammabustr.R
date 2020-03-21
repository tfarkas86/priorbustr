
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

# main gammabustr function
#' @export
gammabustr <- function(mode, pct, target, ndraw=10000,
                      beta_upper=100, plot=FALSE) {


  beta_hat <- optim(par=1, fn=diff_pct, method="Brent", mode=mode,
               pct=pct, target=target, ndraw=ndraw,
               lower=0, upper=beta_upper)$par

  parms <- c(alpha=get_alpha(beta=beta_hat, mode=mode), beta=beta_hat)

  if(plot) {

    sims <- dplyr::tibble(sims=rgamma(ndraw, parms["alpha"], parms["beta"]))

    p <- ggplot2::ggplot(data=sims, ggplot2::aes(x=sims))

    print(p + ggplot2::geom_density() +
            ggplot2::xlab("theta"))

  }

  return(parms)

}


