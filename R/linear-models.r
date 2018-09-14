
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
  if (det(t(xm)%*%xm) == 0) {
    beta <- "singular"
    class(beta) <- c("lm")
    return(beta[])
  } else {
    beta <- solve(t(xm)%*%xm)%*%t(xm)%*%ym
    class(beta) <- c("lm")
    colnames(beta) <- "coefficient"
    return(beta[])
  }
}





