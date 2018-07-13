## Bootstrapping functions

#' Bootstrap difference in statistic across two groups
#'
#' See:
#'   - https://stats.stackexchange.com/questions/136661
#'   - Efron's and Tibshirani's - intro to the bootstrap, page 223
#' @param mutation_table tibble; A mutation table nested by a group
#' @param statistic function; A valid bootstrap statistic
#' @param reps integer; Number of bootstrap replicates
#' @importFrom magrittr "%>%"
#' @importFrom dplyr summarize mutate
#' @importFrom purrr map map_dbl
#' @importFrom parallel detectCores
#' @import dplyr
#' @return A p-value
#' @export
bootstrap_test <- function(dat, statistic, reps = 1e4, ncpus = 1) {
  combined_statistic <- dat %>%
    unnest %>%
    summarize(statistic(., indices = 1:nrow(.))[1]) %>%
    as.numeric
  if (ncpus > 1) {
    parallel = "multicore"
  } else {
    parallel = "no"
  }
  observed_statistic <- map_dbl(dat$data, ~statistic(., indices = 1:nrow(.))[1])
  # Bootstrap a pair of distributions with variance equal to observed, but
  # having the same mutation rate. This serves as the null distribution.
  boot_null <- map(
    dat$data,
    boot_centered_null,
    statistic = statistic,
    center = combined_statistic,
    reps = reps,
    parallel = parallel,
    ncpus = ncpus
  )
  observed_diff <- abs(observed_statistic[1] - observed_statistic[2])
  boot_diff <- abs(boot_null[[1]] - boot_null[[2]])
  p <- (length(which(boot_diff >= observed_diff)) + 1) / length(boot_diff)
  if (p > 1) {
    p <- 1
  }
  p
}


#' Apply a two group statistical test to each group in a nested tibble
#' @importFrom dplyr group_vars
#' @export
boot_compare_all <- function(
  dat,
  statistic,
  within_group = NULL,
  reps = 1e4,
  ...
) {
  tibble_combn(
    dat,
    set_size = 2,
    func = bootstrap_test,
    statistic = statistic,
    within = within_group,
    reps = reps
  )
}

#' Bootstrap a statistic across a table and produce bca ci and quantiles
#'
#' @param tbl A tibble
#' @param statistic function; A bootstrap statistic
#' @param reps integer; Number of bootstrap replicates
#' @importFrom dplyr mutate
#' @importFrom tidyr unnest
#' @importFrom purrr map map_dbl
#' @importFrom magrittr "%>%"
#' @importFrom boot boot
#' @export
boot_quantiles <- function(tbl, statistic, reps=1e4) {
  # Calculate the statistic on each group in the input table
  tbl %>%
    mutate(value = data %>% map_dbl(~statistic(., 1:nrow(.))[1])) %>%
  # Bootstrap the statistic across the table
    mutate(boot_stat = data %>% map(boot, statistic=statistic, R=reps)) %>%
  # Calculate quantiles from the bootstrapped sample
    mutate(quantiles = boot_stat %>% map(calc_bca_quantiles)) %>%
    select(-data, -boot_stat) %>%
    unnest
}

#' Bootstrap the null distribution for a statistic centered at a given value
#'
#' This function generates a distribution for the null hypothesis that two
#' groups do not differ in some statistic. For example if we were interested in
#' whether two samples had a different mean; this function would produce a
#' bootstrapped sample for one group centered at the actual mean across both
#' groups. This ensures that the bootstrapped distribution corresponds to the
#' actual null hypothesis. Explained better here:
#' https://stats.stackexchange.com/questions/136661
#' @importFrom boot boot
#' @importFrom broom tidy
#' @importFrom magrittr "%>%" set_colnames
#' @importFrom tibble as_tibble
boot_centered_null <- function(
  dat,
  statistic,
  center,
  ...,
  reps = 1e4
) {
  boot_sample <- boot(dat, statistic = statistic, R = reps)$t
  colnames(boot_sample) <- c("value", "variance")
  centered_boot <- center_distribution(boot_sample[, "value"], target = center)
  centered_boot
}


#' Calculate quantiles and bias corrected confidence intervals for a boot object
#'
#' @importFrom boot boot.ci
#' @importFrom tibble tibble
calc_bca_quantiles <- function(boot_object) {
  if (is.na(boot_object$t0[2])) {
    output <- tibble(ci1 = NA, q25 = NA, q75 = NA, ci2 = NA)
  } else {
    ci <- boot.ci(boot_object)$bca[4:5]
    quantiles <- quantile(boot_object$t[, 1], probs = c(0.25, 0.75))
    output <- tibble(
      ci1 = ci[1],
      q25 = quantiles[1],
      q75 = quantiles[2],
      ci2 = ci[2])
  }
  output
}
