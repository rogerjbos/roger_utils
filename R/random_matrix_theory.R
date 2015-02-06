#' A routine to install essential packages
#'
#' @param none
#' @keywords random matrix theory
#' @export
#' @examples
#' filter.RMT()
"filter.RMT" <- function (x, trace = TRUE, doplot = TRUE) {
  mp.hist <- mp.density.kernel(x, adjust = 0.2, kernel = "e", doplot = doplot)
  mp.result <- DEoptim(fn = mp.fit.kernel, lower = c(Q = 0,
      sigma = 0.2), upper = c(10, 10), control = list(itermax = 200, trace=FALSE),
      hist = mp.hist)
  mp.Q <- mp.result$optim$bestmem[1]
  mp.sigma <- mp.result$optim$bestmem[2]
  if (trace) print(c(mp.Q, mp.sigma))
  if (doplot) rho <- mp.theory(mp.Q, mp.sigma)
  lambda <- mp.hist$values[1]
  sigma <- sqrt(1 - lambda/length(mp.hist$values))
  lambda.plus <- sigma*sigma * (1 + 1/mp.Q + 2*sqrt(1/mp.Q))

  ans = denoise(mp.hist, lambda.plus, x)
  if (trace) {
    cat("Upper cutoff (lambda.max) is", round(lambda.plus, 2), "\n")
    cat("Variance is", round(sigma, 2), "\n")
    cat("Greatest eigenvalue is", round(lambda, 2), "\n")
  }
  ans
}

#' A routine to install essential packages
#'
#' @param none
#' @keywords packages
#' @export
#' @examples
#' colSdColMeans()
"colSdColMeans" <- function(x, na.rm=TRUE) {
  if (na.rm) {
    n <- colSums(!is.na(x))
  } else {
    n <- nrow(x)
  }
  colVar <- colMeans(x*x, na.rm=na.rm) - (colMeans(x, na.rm=na.rm))^2
  return(sqrt(colVar * n/(n-1)))
}

#' A routine to install essential packages
#'
#' @param none
#' @keywords random matrix theory
#' @export
#' @examples
#' mp.density.kernel()
"mp.density.kernel" <- function (x, adjust = 0.2, kernel = "e", doplot = TRUE, ...) {
  e <- cov2cor(cov(x/colSdColMeans(x)))
  lambda <- eigen(e, symmetric = TRUE, only.values = FALSE)
  ds <- density(lambda$values, adjust = adjust, kernel = kernel, ...)
  ds$adjust <- adjust
  ds$kernel <- kernel
  ds$values <- lambda$values
  ds$vectors <- lambda$vectors
  if (doplot) plot(ds, xlim = c(0, max(ds$values) * 1.2), main = "Eigenvalue Distribution")
  return(ds)
}

#' A routine to install essential packages
#'
#' @param none
#' @keywords random matrix theory
#' @export
#' @examples
#' mp.rho()
"mp.rho" <- function (mp.Q, sigma, e.values) {
  l.min <- sigma*sigma * (1 + 1/mp.Q - 2*sqrt(1/mp.Q))
  l.max <- sigma*sigma * (1 + 1/mp.Q + 2*sqrt(1/mp.Q))
  k <- (mp.Q/2 * pi * sigma^2)
  rho <- k * sqrt(pmax(0, (l.max - e.values) * (e.values - l.min)))/e.values
  rho[is.na(rho)] <- 0
  attr(rho, "e.values") <- e.values
  rho
}

#' A routine to install essential packages
#'
#' @param none
#' @keywords random matrix theory
#' @export
#' @examples
#' mp.lambdas()
"mp.lambdas" <- function (Q, sigma, steps, trace = FALSE) {
  l.min <- sigma*sigma * (1 + 1/mp.Q - 2*sqrt(1/mp.Q))
  l.max <- sigma*sigma * (1 + 1/mp.Q + 2*sqrt(1/mp.Q))
  if (trace) {
    cat("Min eigenvalue:", l.min, "\n")
    cat("Max eigenvalue:", l.max, "\n")
  }
  evs <- seq(round(l.min - 1), round(l.max + 1), (l.max - l.min)/steps)
  evs[evs < l.min] <- l.min
  evs[evs > l.max] <- l.max
  if (trace) {
    cat("eigenvalues: ", evs, "\n")
  }
  evs
}

#' A routine to install essential packages
#'
#' @param none
#' @keywords random matrix theory
#' @export
#' @examples
#' imp.fit.kernel()
"mp.fit.kernel" <- function (ps, hist) {
  BIG <- 100000000000000
  zeros <- which(hist$y == 0)
  wholes <- which(hist$y > 0)
  after <- head(zeros[zeros > wholes[1]], 1)
  l.plus <- hist$x[after]
  Q <- ps[1]
  sigma <- ps[2]
  rhos <- mp.rho(Q, sigma, hist$x)
  if (max(rhos) == 0) return(BIG)
  scale <- max(rhos)/max(hist$y) + 0.25
  whole.idx <- head(rhos[rhos > 0], 1)
  hist$y <- c(rep(0, whole.idx - 1), tail(hist$y, length(hist$y) - whole.idx + 1))
  norm.factor <- sum(rhos[hist$x <= l.plus])
  hist$y <- hist$y[1:length(rhos)]
  dy <- (rhos - (hist$y * scale))/norm.factor
  dist <- as.numeric(dy %*% dy)
  if (is.na(dist)) dist = BIG
  dist
}

#' A routine to install essential packages
#'
#' @param none
#' @keywords random matrix theory
#' @export
#' @examples
#' mp.theory()
"mp.theory" <- function (Q, sigma, e.values = NULL, steps = 200) {
  if (is.null(e.values)) e.values <- mp.lambdas(Q, sigma, steps)
  rho <- mp.rho(Q, sigma, e.values)
  if (length(e.values) > 1) {
    l.min <- sigma*sigma * (1 + 1/mp.Q - 2*sqrt(1/mp.Q))
    l.max <- sigma*sigma * (1 + 1/mp.Q + 2*sqrt(1/mp.Q))
    xs <- seq(round(l.min - 1), round(l.max + 1), (l.max - l.min)/steps)
    main <- paste("Marcenko-Pastur Distribution for Q", round(Q, 1), "and sigma", round(sigma,1))
    plot(xs, rho, xlim = c(0, 6), type = "l", main = main)
  }
  rho
}

#' A routine to remove the 'noisey' observation in a correlation matrix using Random Matrix Theory
#'
#' @param none
#' @keywords random matrix theory
#' @export
#' @examples
#' denoise()
"denoise" <- function (hist, lambda.plus = 1.6, x = NULL) {
  e.values <- hist$values
  avg <- mean(e.values[e.values < lambda.plus])
  e.values[e.values < lambda.plus] <- avg
  e.vectors <- hist$vectors
  clean <- e.vectors %*% diag(e.values) %*% t(e.vectors)
  diags <- diag(clean) %o% rep(1, nrow(clean))
  clean <- clean/sqrt(diags * t(diags))
  if (!is.null(x)) {
    rownames(clean) <- colnames(x)
    colnames(clean) <- colnames(x)
  }
  clean
}
