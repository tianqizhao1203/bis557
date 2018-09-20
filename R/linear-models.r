
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
  yname <- paste(as.list(formula)[[2]])
  ym <- as.matrix(data[,yname])
  beta <- qr.solve(xm, ym)
  beta[beta == 0] <- NA
  
  fit <- list()
  fit$coefficients <- beta
  fit$residuals <- ym - xm %*% beta
  fit$fitted.values <- xm %*% beta
  fit$rank <- qr(xm)$rank
  fit$df.residuals <- nrow(xm) - qr(xm)$rank
  fit$qr <- qr(xm)
  fit$call <- match.call()
  fit$terms <- terms(x = formula, data = data)
  fit$contrasts <- attr(qr(xm), "contrasts")
  fit$effects <- qr.qty(qr(xm), ym)
  fit$assign <- attr(qr(xm), "assign")
  fit$xlevels <- .getXlevels(terms(x = formula, data = data), m = model.frame(data))
  fit$y <- ym
  fit$x <- xm
  fit$model <- model.frame(data)
  class(fit) <- "lm"
  return(fit)
}


