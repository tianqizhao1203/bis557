#lm_patho <- read.csv("df.csv")
ridge_train <- read.csv("ridge_train.csv")
ridge_test <- read.csv("ridge_test.csv")
#dir.create("../data")
save(ridge_train, file = "../data/ridge_train.rda")
save(ridge_test, file = "../data/ridge_test.rda")

save(ridge_train, file = "../vignettes/ridge_train.rda")
save(ridge_test, file = "../vignettes/ridge_test.rda")
