# https://ignacioriveros1.github.io/r/2020/03/22/r_and_stata.html

library(RStata)
library(dplyr)
library(reshape2)


options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
options("RStata.StataVersion" = 17)

load("~/R/hcmmice/data/data_set1.RData")

# # rearrange data to wide format
# dat1 <-
#   data_set1 |>
#   filter(!is.na(male)) |>
#   select(id, age, nsvt, syncope, male) |>
#   mutate(age1 = floor(age),
#          age2 = age1 + 1,
#          age3 = age1 + 2,
#          firstyear = 1,
#          lastyear = 3,
#          nsvt1 = nsvt,
#          nsvt2 = NA,
#          nsvt3 = NA) |>
#   select(-age, -nsvt)
#
# # export data frame to Stata binary format
# foreign::write.dta(dat1, "data/widedata.dta")
#
#
# #############
# # imputation
#
# twofold_out <-
#   stata("scripts/twofold.do",
#         data.in = dat1, data.out = TRUE) |>
#   as_tibble()
#
# twofold_out



# -------------------------------------------------------------------------

FIRSTYEAR <- 41
LASTYEAR <- 50

range_years <- FIRSTYEAR:LASTYEAR
n_years <- length(range_years)

dat45 <-
  data_set1 |>
  select(id, age, nsvt, syncope, male, mwt, fhxscd, lvedd, fs) |>
  mutate(age = floor(age)) |>
  filter(age >= FIRSTYEAR, age <= LASTYEAR)

dat2 <-
  cbind.data.frame(
    id = rep(dat45$id, each = n_years),
    age = rep(range_years, nrow(dat45))) |>
  full_join(dat45, by = c("id", "age")) |>
  dcast(id ~ age, value.var = "nsvt") |>
  rename_with(~ paste0("nsvt", .x), !starts_with("id")) |>
  mutate(firstyear = FIRSTYEAR,
         lastyear = LASTYEAR) |>
  full_join(dat45, by = "id") |>
  select(-nsvt, -syncope) |>
  mutate(firstyear = age)  # left-censoring


twofold_out2 <-
  stata("scripts/twofold2.do",
        data.in = dat2, data.out = TRUE) |>
  as_tibble()

twofold_out2

twofold_out2 <-
  twofold_out2 |>
  mutate(across(starts_with("nsvt"), ~ as.numeric(.x >= 0.5)))



# deterministically fill in some nsvt ------------------------------

dat45_obs <-
  dat45 |>
  mutate(age_obs = age,
         nsvt_obs = nsvt) |>
  select(id, age_obs, nsvt_obs)

dat3 <-
  cbind.data.frame(
    id = rep(dat45$id, each = n_years),
    age = rep(range_years, nrow(dat45))) |>
  full_join(dat45, by = c("id", "age")) |>
  full_join(dat45_obs, by = "id") |>
  group_by(id) |>
  # left fill nsvt == 0
  # right fill nsvt == 1
  mutate(nsvt = ifelse(age_obs > age & nsvt_obs == 0, 0, nsvt)#,
         # nsvt = ifelse(age_obs < age & nsvt_obs == 1, 1, nsvt)
         ) |>
  ungroup() |>
  select(-age_obs, -nsvt_obs) |>
  dcast(id ~ age, value.var = "nsvt") |>
  rename_with(~ paste0("nsvt", .x), !starts_with("id")) |>
  mutate(firstyear = FIRSTYEAR,
         lastyear = LASTYEAR) |>
  full_join(dat45, by = "id") |>
  select(-nsvt, -syncope) |>
  mutate(firstyear = age)  # left-censoring


twofold_out3 <-
  stata("scripts/twofold2.do",
        data.in = dat3, data.out = TRUE) |>
  as_tibble()

twofold_out3

twofold_out3 <-
  twofold_out3 |>
  mutate(across(starts_with("nsvt"), ~ as.numeric(.x >= 0.5)))




