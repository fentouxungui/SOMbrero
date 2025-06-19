context("Check that plots are produced properly for data with missing entries.")

set.seed(1404)
removed <- cbind(sample(1:150, 20, replace = TRUE), 
                 sample(1:4, 20, replace = TRUE))
x.data <- iris[, 1:4]
x.data[removed] <- NA
nsom <- trainSOM(iris[, 1:4], maxit = 10)

test_that("All 'obs' plots are produced for inputs with missing entries", {
  expect_true(is_ggplot({
    plot(nsom)
  }))
  expect_true(is_ggplot({
    plot(nsom, what = "obs", type = "color", variable = 2)
  }))
  expect_true(is_ggplot({
    plot(nsom, what = "obs", type = "lines")
  }))
  expect_true(is_ggplot({
    plot(nsom, what = "obs", type = "meanline")
  }))
  expect_true(is_ggplot({
    plot(nsom, what = "obs", type = "barplot")
  }))
  expect_true(is_ggplot({
    plot(nsom, what = "obs", type = "boxplot")
  }))
})

test_that("Energy plot is produced for inputs with missing entries", {
  nsome <- trainSOM(iris[, 1:4], maxit = 10, nb.save = 10)
  expect_true(is_ggplot({
    plot(nsome, what = "energy")
  }))
})

test_that("All 'add' plots are produced for inputs with missing entries", {
  set.seed(201)
  removed <- sample(1:150, 10, replace = TRUE)
  add <- iris$Species
  add[removed] <- NA
  
  expect_true(is_ggplot({
    plot(nsom, what = "add", type = "pie", variable = add)
  }))
  expect_true(is_ggplot({
    plot(nsom, what = "add", type = "names", variable = add)
  }))
  
  add2 <- matrix(0, ncol = 3, nrow = 150)
  add2[iris$Species == "versicolor", 1] <- 1
  add2[iris$Species == "virginica", 2] <- 1
  add2[iris$Species == "setosa", 3] <- 1
  add2[removed, ] <- NA
  colnames(add2) <- c("versicolor", "virginica", "setosa")
  expect_true(is_ggplot({
    plot(nsom, what = "add", type = "words", variable = add2)
  }))
  
  add3 <- x.data[, 1]
  expect_true(is_ggplot({
    plot(nsom, what = "add", type = "color", variable = add3)
  }))
  
  add4 <- x.data
  expect_error(plot(nsom, what = "add", type = "lines", variable = add4))
  expect_true(is_ggplot({
    plot(nsom, what = "add", type = "meanline", variable = add4)
  }))
  expect_true(is_ggplot({
    plot(nsom, what = "add", type = "barplot", variable = add4)
  }))
  expect_true(is_ggplot({
    plot(nsom, what = "add", type = "boxplot", variable = add4)
  }))
})

test_that("Quality can be computed for inputs with missing entries", {
  out_quality <- quality(nsom)
  expect_true(out_quality$topographic >= 0 && out_quality$topographic <= 1)
  expect_true(is.numeric(out_quality$quantization))
})
