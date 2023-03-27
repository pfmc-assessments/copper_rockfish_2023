plotindex_bayes <- function(model, years, backtrans, rand = F, standardize = T, title, ylim = NULL) {
  c.draws <- as.data.frame(model)
  if (rand) {
    n <- nrow(c.draws)
    c.draws[, 1] <- c.draws[, 1] + rnorm(rep(1, n), rep(0, n), c.draws[, ncol(c.draws)])
  }
  index.draws <- data.frame(c.draws[, 1:length(years)])
  #colnames(index.draws)[1] <- paste0(yearvar, years[1])
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