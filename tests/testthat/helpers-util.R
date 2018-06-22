indexed_mean <- function(dat, indices) {
  sampling <- unlist(dat$val)[indices]
  c(mean(sampling), var(sampling))
}
