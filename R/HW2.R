# Function for ridge regression
# Use SVD for estimation

ridge_reg <- function(formula, lambda,data){
  m <- model.matrix(formula, data)
  yname <- formula[[2]]
  y <- as.matrix(data[paste0(yname)])
  svd_obj <- svd(m)
  beta <- svd_obj$v %*% diag(svd_obj$d/((svd_obj$d)^2 + lambda)) %*% t(svd_obj$u) %*% y
  rownames(beta) <- colnames(m)
  ret <- list(coefficients = beta, lambda = lambda)
  class(ret) <- "ridge_reg"
  ret
}

