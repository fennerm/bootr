#' Center a distribution at a given mean value
#'
#' @param d numeric; the distribution
#' @param target numeric; the new mean
#' @return The centered distribution
center_distribution <- function(d, target) {
  d <- unlist(d)
  d - mean(d) + target
}


#' Apply a function across all combinations of groups in a nested tibble
#'
#' All extra parameters are passed to the input function
#' @param tbl A nested tibble
#' @param set_size Number of rows in dat to compare in each iteration
#' @param func function Function to apply to each set of rows
#' @param within character A column name from the inner nested tibbles in `tbl`.
#'                         If given, comparisons will only be made between rows
#'                         which share the same group in the within column.
#' @return A list of results from 'func'
#' @importFrom tibble as.tibble
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by filter mutate bind_rows
#' @importFrom purrr map
tibble_combn <- function(tbl, set_size, func, within = NULL, ...) {
  grouping <- colnames(tbl)[1]
  results <- tbl %>%
    split_nested_tibble(by = within) %>%
    # Apply the function to each within group separately
    map(function(x) {
      if (nrow(x) > 1) {
        combinations <- combn(unlist(x[, 1]), set_size, simplify = FALSE)
        func_output <- combinations %>%
          # Filter the table
          map(~filter(x, !!as.name(grouping) %in% .)) %>%
          # Apply the function
          map(func, ...)
        # Bind the group names with the function results
        output_table <- as.tibble(do.call("rbind", combinations)) %>%
          mutate(result = unlist(func_output))
      } else {
        output_table <- NULL
      }
      output_table
    })
  results <- bind_rows(filter_null(results))
  results
}

#' Split a nested tibble by a column
#'
#' @param tbl A nested tibble
#' @param by A column name within the nested tibble to split by
#' @return A list of nested tibbles
#' @importFrom dplyr group_by
#' @importFrom purrr map
#' @importFrom tidyr nest unnest
split_nested_tibble <- function(tbl, by) {
  grouping <- colnames(tbl)[1]
  if (!is.null(by)) {
    tbls <- tbl %>%
      unnest %>%
      split_table(by = by) %>%
      map(~group_by(., !!as.name(grouping))) %>%
      map(nest)
  } else {
    tbls <- list(tbl)
  }
  tbls
}

#' Remove all NULL values from a list
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_lgl
filter_null <- function(x) {
  x[map_lgl(x, is.null)] <- NULL
  x
}

#' Split a data.frame by a column
#'
#' @param df A data.frame
#' @param by A column name
#' @param levels A set of possible values in by. If NULL, the table is
#'               split on all unique values.
#' @return A list of data.frames
#' @export
split_table <- function(df, by = NULL, levels = NULL) {
  if (is.null(by)) {
    dfs <- list(df)
  } else {
    if (is.null(levels)) {
      dfs <- split(df, df[, by])
    } else {
      per_level <- lapply(levels, function(x) which(df[, by] == x))
      dfs <- lapply(per_level, function(x) df[x, ])
    }
  }
  dfs
}
