#' Generates variables correlated with the input
#'
#' Given a (numeric) input variable, this function generates a set of variables
#' correlated to the input, with provided correlations. Currently it only
#' supports specification of correlations with the original input, not a full
#' covariance matrix among the generated variables. Depending on the provided
#' correlations and the random numbers generated, this will occasionally fail
#' for now due to an internal matrix not being positive definite. In that case,
#' you should be able to specify one variable at a time and bind the outputs.
#' @param x the input variable to be correlated against
#' @param cors a vector of correlations to generate variables for
#' @param mean a vector of means for the correlated variables, defaulting to the
#' mean of the input
#' @param sd a vector of standard deviations for the correlated variables,
#' defaulting to the standard deviation of the input
#' @return a data frame, whose first column is the input, and whose remaining
#' are correlated variables for each provided correlation
#' @source Adapted from \url{https://stat.ethz.ch/pipermail/r-help/2007-April/128925.html}
#' @export
correlate_vars <- function(x, cors, mean = mean(x), sd = sd(x)) {
  if (length(mean) %notin% c(1, length(cors))) {
    stop("`mean` must have length 1 or the same length as `cors`")
  }
  if (length(sd) %notin% c(1, length(cors))) {
    stop("`sd` must have length 1 or the same length as `cors`")
  }

  y <- scale(matrix(stats::rnorm(length(x) * length(cors)),
                    ncol = length(cors)))

  xy <- cbind(scale(x), y)
  c1 <- stats::var(xy)
  chol1 <- solve(chol(c1))
  newxy <-  xy %*% chol1

  newc <- diag(length(cors) + 1)
  newc[1, 2:(length(cors) + 1)] <- cors
  newc[2:(length(cors) + 1), 1] <- cors

  chol2 <- chol(newc)

  finaly <- (newxy %*% chol2)[ ,-1] %*% diag(sd) + rep(mean, each = length(x))

  finalx <- as.data.frame(cbind(x, finaly))
  colnames(finalx) <- c("x", paste("y", cors, sep = "_"))

  finalx
}
