
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
  xm <- model.matrix(formula,data)
  
  # Get the name of the y to extract y from the dataset
  yname <- paste(as.list(formula)[[2]])
  ym <- data[,yname]
  
  # Calculate beta
  beta <- qr.solve(xm, ym)
  
  # Beta = 0 indicates colinearity; changed to NA
  beta[beta == 0] <- NA
  
  fit <- list()
  fit$coefficients <- beta
  fit$residuals <- ym - xm %*% beta
  fit$fitted.values <- xm %*% beta
  fit$rank <- qr(xm)$rank
  fit$df.residuals <- nrow(xm) - qr(xm)$rank
  fit$weights <- NULL
  fit$qr <- qr(xm)
  fit$call <- call("linear_model", formula)
  fit$terms <- terms(x = formula, data = data)
  fit$contrasts <- NULL
  fit$xlevels <- NULL
  fit$offset <- NULL
  fit$y <- ym
  fit$x <- xm
  fit$model <- model.frame(data)
  class(fit) <- "lm"
  return(fit)
}


