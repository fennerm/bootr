library(tidyverse)

set.seed(1)
dat <- tibble(
  g1 = c(rep("a", 10), rep("b", 10)),
  g2 = c(rep("c", 5), rep("d", 5), rep("e", 5), rep("f", 5)),
  sample = as.character(1:20),
  val = runif(20, 0, 20)
)

g1_grouped <- dat %>% group_by(g1) %>% nest
g2_grouped <- dat %>% group_by(g2) %>% nest
dual_grouped <- dat %>% group_by(g1, g2) %>% nest
