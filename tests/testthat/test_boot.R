context("Test bootstrapping functions")

test_that("bootstrap_test works correctly", {
  set.seed(1)
  expect_equal(bootstrap_test(g1_grouped, indexed_mean, reps=100), 0.96)
})

test_that("boot_compare_all works correctly", {
  set.seed(1)
  result <- boot_compare_all(g2_grouped, indexed_mean, reps=100)
  expect_equivalent(
    result$result,
    c(0.4, 0.93, 0.16, 0.3, 0.87, 0.14),
    tolerance=0
  )
})

test_that("boot_compare_all within_group parameter", {
  set.seed(1)
  result <- boot_compare_all(
    g2_grouped,
    indexed_mean,
    within_group = "g1",
    reps = 100
  )
  expect_equivalent(unlist(result[,1]), c("c", "e"))
  expect_equivalent(unlist(result[,2]), c("d", "f"))
  expect_equivalent(unlist(result[,3]), c(0.4, 0.15), tolerance=0)
})

test_that("boot_quantiles works correctly", {
  set.seed(1)
  result <- boot_quantiles(g1_grouped, indexed_mean, reps=1000)
  expect_equal(
    names(result),
    c("g1", "value", "ci1", "q25", "q75", "ci2")
  )
  expect_equal(na.omit(result), result)
})

test_that("boot_centered_null produces a centered null distribution", {
  result <- boot_centered_null(
    g1_grouped$data[[1]], indexed_mean, center=10, reps=100
  )
  expect_equal(mean(result), 10, tolerance=0)
})
