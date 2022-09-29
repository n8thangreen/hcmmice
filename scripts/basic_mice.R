#


library(dplyr)
library(mice)


load("~/R/hcmmice/data/data_set1.RData")


dat <-
  data_set1 |>
  select(id, age, nsvt, syncope, male) |>
  mutate(age = floor(age))

dat_mi <-
  dat |>
  slice(rep(1:n(), 3)) |>
  group_by(id) |>
  mutate(extra_yr = row_number() - 1,
         age0 = age,
         age = age + extra_yr,
         nsvt = ifelse(age != age0, NA, nsvt),
         syncope = ifelse(age != age0, NA, syncope)) |>
  ungroup() |>
  select(-extra_yr, -age0)


md.pattern(dat_mi)

imputed_dat <- mice(dat_mi, m=5, maxit = 50, method = 'pmm', seed = 500)

complete_data <- complete(imputed_dat, 2)
