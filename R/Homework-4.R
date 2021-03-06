# Create a new class `sparse.matrix` that has add `+`, multiply `%*%`, and transpose `t()` methods.


#' Create a new class sparse.matrix
#'
#' @description This function is used for creating a new class called 'sparse.matrix'. 
#' @param i A vector of row indices for non-zero entries
#' @param j A vector of column indices for non-zero entries
#' @param x A vector of the values for non-zero entries at the corresponding (i,j) positions
#' @param dims The dimension of sparse matrix, and default is the largest coordinates of i and j.
#' @return A 'sparse.matrix' object.
#' @examples
#' sm3 <- sparse.matrix(i = rep(1, 3), j = 1:3, x = 1:3, dims = c(2, 3))
#' @export

sparse.matrix <- function(i, j, x, dims = c(max(i), max(j))) {
  s <- data.frame(i, j, x)
  class(s) <- c("sparse.matrix", "data.frame")
  # Add dims to attributes
  attributes(s)$dims <- dims
  return(s)
}

#' Define add method for 'sparse.matrix' class
#'
#' @description This function is used for defining `+` operation in class "sparse.matrix". 
#' @param x A 'sparse.matrix' object.
#' @param y A 'sparse.matirx' object.
#' @return A 'sparse.matrix' object, which is the result of x + y.
#' @examples
#' sm2 <- sparse.matrix(i = c(1, 2, 2), j = c(1, 1, 2), x = c(4.4, 1.2, 3), dims = c(2, 3))
#' sm3 <- sparse.matrix(i = rep(1, 3), j = 1:3, x = 1:3, dims = c(2, 3))
#' sm2 + sm3
#' @export

`+.sparse.matrix` <- function(x, y) {
  # Report error
  if (!inherits(y, "sparse.matrix")) {
    stop("The class of y is not sparse.matrix.")
  }
  
  if ((attributes(x)$dims[1] != attributes(y)$dims[1]) | (attributes(x)$dims[2] != attributes(y)$dims[2])) {
    stop("Invalid matrix addition!")
  }
  
  c <- merge(x, y, by.x = c('i', 'j'), by.y = c('i', 'j'), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- rowSums(c[,3:4])
  add <- c[, c("i", "j", "x")]
  add <- add[order(add$j), c(1,2,3)]
  row.names(add) <-1:nrow(add)
  sparse.matrix(add$i, add$j, add$x, dims = c(attributes(x)$dims[1], attributes(x)$dims[2]))
}

#' Define transpose method for 'sparse.matrix' class
#'
#' @description This function is used for defining `t` operation in class "sparse.matrix". 
#' @param x A 'sparse.matrix' object.
#' @return A 'sparse.matrix' object, which is the result of t(x).
#' @example
#' sm1 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1), dims = c(3, 2))
#' t(sm1)
#' @export

`t.sparse.matrix` <- function(x) {
  inter <- cbind(x, NA)
  # Add an intermediate column to exchange the values between i and j
  inter[,4] <- inter[,1]
  inter[,1] <- inter[,2]
  inter[,2] <- inter[,4]
  trans <- inter[,-4]
  sparse.matrix(trans$i, trans$j, trans$x, dims = c(attributes(x)$dims[2], attributes(x)$dims[1]))
}

`%*%.default` <- .Primitive("%*%")  

`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}

#' Define multiply method for 'sparse.matrix' class
#'
#' @description This function is used for defining `%*%` operation in class "sparse.matrix". 
#' @param x A 'sparse.matrix' object.
#' @param y A 'sparse.matrix' object.
#' @return A 'sparse.matrix' object, which is the result of x %*% y.
#' @example
#' sm1 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1), dims = c(3, 2))
#' sm3 <- sparse.matrix(i = rep(1, 3), j = 1:3, x = 1:3, dims = c(2, 3))
#' sm1 %*% sm3
#' @export

`%*%.sparse.matrix` <- function(x, y) {
  # Report error
  if (!inherits(y, "sparse.matrix")) {
    stop("The class of y is not sparse.matrix.")
  }
  if (attributes(x)$dims[2] != attributes(y)$dims[1]) {
    stop("Invalid matrix multiplication!")
  }
  
  sparse <- data.frame()
  for (g in 1:attributes(x)$dims[1]) {
    for (h in 1:attributes(y)$dims[2]) {
      su <- 0
      for (z in 1:attributes(y)$dims[1]) {
        a <- sum(x[x$i == g & x$j == z, ]$x * y[y$i == z & y$j == h, ]$x)
        su <- sum(a, su)
      }
      row <- cbind(g, h, su)
      sparse <- rbind(sparse, row)
    }
  }
  
  colnames(sparse) <- c("i", "j", "x")
  # Remove rows with x = 0
  row_sub <- apply(sparse, 1, function(row) all(row != 0))
  mul <- sparse[row_sub, ]
  mul <- mul[order(mul$j), c(1,2,3)]
  row.names(mul) <-1:nrow(mul)
  sparse.matrix(mul$i, mul$j, mul$x, dims = c(attributes(x)$dims[1], attributes(y)$dims[2]))
}