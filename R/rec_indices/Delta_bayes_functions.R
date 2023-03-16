
## Function to calculate the AIC for the full gamma and lognormal models to
## Select which distribution to use for the positive model for model selection

calculate_aic <- function(logn.full, gamma.full) {
  # lognormal
  logn.coefs <- as.numeric(coef(logn.full))
  logn.X <- model.matrix(logn.full)
  logn.y.obs <- exp(logn.full$model[[1]])
  logn.N <- length(logn.y.obs)
  # number of regression coefficients, including the intercept term
  logn.K <- length(logn.coefs)
  logn.sigma.mle <- sqrt(summary(logn.full)$dispersion * ((logn.N - logn.K) / logn.N))

  lnorm.nll <- -sum(dlnorm(logn.y.obs,
    fitted(logn.full),
    sdlog = logn.sigma.mle,
    log = TRUE
  ))
  # use K+1 parameters for AIC, to account for dispersion parameter
  AIC.lognormal <- 2 * lnorm.nll + 2 * (logn.K + 1)
  AIC.lognormal.results <- rbind(AIC.lognormal, logn.sigma.mle)



  # gamma
  gamma.coefs <- as.numeric(coef(gamma.full))
  gamma.X <- model.matrix(gamma.full)
  gamma.coefs <- as.numeric(coef(gamma.full))
  gamma.y.obs <- gamma.full$model[[1]]
  gamma.shape.mle <- MASS::gamma.shape(gamma.full)[[1]]
  gamma.par <- c(gamma.shape.mle, gamma.coefs)
  gamma.fitted <- as.numeric(exp(gamma.X %*% gamma.par[2:length(gamma.par)]))
  gamma.nll <- -sum(gamma.par[1] * (-gamma.y.obs / gamma.fitted - log(gamma.fitted)) +
    gamma.par[1] * log(gamma.y.obs) +
    gamma.par[1] * log(gamma.par[1]) -
    log(gamma.y.obs) - lgamma(gamma.par[1]))

  AIC.gamma <- 2 * gamma.nll + 2 * length(gamma.par)
  AIC.gamma.results <- rbind(AIC.gamma, gamma.shape.mle)


  # AIC_compare <-rbind(AIC(logn.full), AIC.lognormal, AIC(gamma.full), AIC.gamma)
  # rownames(AIC_compare)[c(1,3)] <-c("Canned.lognAIC", "Canned.GammaAIC")
  # AIC_compare
  aic.df<- as.data.frame(rbind(AIC.lognormal.results, AIC.gamma.results))
 colnames(aic.df) <- "Values"
  return(aic.df)
}







# Index generation and ploting functions ###
# Copied and modified from Tanya Roger's code
ilogit <- function(x) {
  exp(x) / (1 + exp(x))
}
plotindex <- function(model, years, backtrans, rand = F, standardize = T, title = model$formula) {
  if (rand) {
    coefs <- as.numeric(coef(model)[[1]][1, 1:(length(years))])
  } else {
    coefs <- coef(model)[1:length(years)]
  }
  coefssd <- sqrt(diag(vcov(model)))[1:length(years)]
  names(coefs)[1] <- paste0(yearvar, years[1])
  names(coefssd)[1] <- paste0(yearvar, years[1])
  ind.log <- c(coefs[1], coefs[1] + coefs[2:length(years)])
  if (backtrans == "ilogit") {
    ind <- ilogit(ind.log)
    indmean <- ifelse(standardize, mean(ind), 1)
    ind <- ind / indmean
    indl <- ilogit(ind.log - coefssd) / indmean
    indu <- ilogit(ind.log + coefssd) / indmean
  }
  if (backtrans == "exp") {
    ind <- exp(ind.log)
    indmean <- ifelse(standardize, mean(ind), 1)
    ind <- ind / indmean
    indl <- exp(ind.log - coefssd) / indmean
    indu <- exp(ind.log + coefssd) / indmean
  }
  plot(yrvec, ind, type = "o", ylim = range(indl, indu), main = title, ylab = "index", xlab = "year")
  lines(yrvec, indl, lty = 2)
  lines(yrvec, indu, lty = 2)
  return(data.frame(year = yrvec, coefs = coefs, coefssd = coefssd, ind = ind, indl = indl, indu = indu))
}
plotindex_bayes <- function(model, years, backtrans, rand = F, standardize = T, title, ylim = NULL) {
  c.draws <- as.data.frame(model)
  if (rand) {
    n <- nrow(c.draws)
    c.draws[, 1] <- c.draws[, 1] + rnorm(rep(1, n), rep(0, n), c.draws[, ncol(c.draws)])
  }
  index.draws <- cbind.data.frame(c.draws[, 1], c.draws[, 1] + c.draws[, 2:length(years)])
  colnames(index.draws)[1] <- paste0(yearvar, years[1])
  ind.log <- apply(index.draws, 2, mean)
  ind.se <- apply(index.draws, 2, sd)
  if (backtrans == "ilogit") {
    ind <- ilogit(ind.log)
    indmean <- ifelse(standardize, mean(ind), 1)
    ind <- ind / indmean
    indl <- ilogit(ind.log - ind.se) / indmean
    indu <- ilogit(ind.log + ind.se) / indmean
  }
  if (backtrans == "exp") {
    ind <- exp(ind.log)
    indmean <- ifelse(standardize, mean(ind), 1)
    ind <- ind / indmean
    indl <- exp(ind.log - ind.se) / indmean
    indu <- exp(ind.log + ind.se) / indmean
  }
  if (is.null(ylim)) ylim <- range(indl, indu)
  plot(years, ind, type = "o", ylim = ylim, xlim = range(yrvec), main = title, ylab = "index", xlab = "year")
  lines(years, indl, lty = 2)
  lines(years, indu, lty = 2)
  return(data.frame(year = years, coefs = ind.log, coefssd = ind.se, ind = ind, indl = indl, indu = indu))
}
plotindex_delta <- function(outdf, years, standardize = T, title, ylim = NULL) {
  ind <- exp(outdf$logmean)
  indmean <- ifelse(standardize, mean(ind), 1)
  ind <- ind / indmean
  indu <- exp(outdf$logmean + outdf$logSD) / indmean
  indl <- exp(outdf$logmean - outdf$logSD) / indmean
  if (is.null(ylim)) ylim <- range(indl, indu)
  plot(years, ind, type = "o", ylim = ylim, xlim = range(yrvec), main = title, ylab = "index", xlab = "year")
  lines(years, indu, lty = 2)
  lines(years, indl, lty = 2)
  return(data.frame(year = outdf$Year, ind = ind, indl = indl, indu = indu))
}
