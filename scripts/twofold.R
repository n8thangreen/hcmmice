# https://ignacioriveros1.github.io/r/2020/03/22/r_and_stata.html

library(RStata)

options("RStata.StataPath" = "\"C:\\Program Files\\Stata17\\StataMP-64\"")
options("RStata.StataVersion" = 17)

load("~/R/hcmmice/data/data_set1.RData")

# rearrange data to wide format
dat1 <-
  data_set1 |>
  filter(!is.na(male)) |>
  select(id, age, nsvt, syncope, male) |>
  mutate(age1 = floor(age),
         age2 = age1 + 1,
         age3 = age1 + 2,
         firstyear = 1,
         lastyear = 3,
         nsvt1 = nsvt,
         nsvt2 = NA,
         nsvt3 = NA) |>
  select(-age, -nsvt)

# export data frame to Stata binary format
foreign::write.dta(dat1, "data/widedata.dta")


#############
# imputation

twofold_out <-
  stata("scripts/twofold.do",
        data.in = dat1, data.out = TRUE) |>
  as_tibble()

twofold_out



# -------------------------------------------------------------------------

library(reshape2)

dat45 <-
  data_set1 |>
  select(id, age, nsvt, syncope, male, mwt, fhxscd, lvedd, fs) |>
  mutate(age = floor(age)) |>
  filter(age > 40, age <= 45)

dat2 <-
  cbind.data.frame(id = rep(dat45$id, each = 5),
        age = rep(41:45, nrow(dat45))) |>
  full_join(dat45, by = c("id", "age")) |>
  dcast(id~age, value.var = "nsvt") |>
  rename_with(~paste0("nsvt", .x), !starts_with("id")) |>
  mutate(firstyear = 41,
         lastyear = 45) |>
  full_join(dat45, by = "id") |>
  select(-nsvt, -syncope)


twofold_out2 <-
  stata("scripts/twofold2.do",
        data.in = dat2, data.out = TRUE) |>
  as_tibble()

twofold_out2

twofold_out2 <-
  twofold_out2 |>
  mutate(nsvt41 = as.numeric(nsvt41 >= 0.5),
         nsvt42 = as.numeric(nsvt42 >= 0.5),
         nsvt43 = as.numeric(nsvt43 >= 0.5),
         nsvt44 = as.numeric(nsvt44 >= 0.5),
         nsvt45 = as.numeric(nsvt45 >= 0.5))
