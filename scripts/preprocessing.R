# This code generates the data used to make the plots and
# summaries in the paper
library(learningtower)
library(tidyverse)

# Get the 2018 student data
if (!file.exists("data/student_2018.rda")) {
  student_2018 <- load_student("2018")
  save(student_2018, file = "data/student_2018.rda")
}
data(countrycode, package = "learningtower")

# Load the country names, and join
student_country <- left_join(student_2018,
                             countrycode, by = "country")

# Drop missing values: not used
# math_pisa_2018_data <- student_country |>
#   filter(!is.na(gender), !is.na(math), !is.na(stu_wgt))

pisa_2018_data_complete <- student_country |>
  filter(!is.na(gender), !is.na(math), !is.na(stu_wgt))

# Run this code to calculate confidence intervals from scratch
# Compute average math scores and gender diff
if (!file.exists("data/math_diff_conf_intervals.rda")) {
  math_diff_df <- pisa_2018_data_complete |>
    group_by(gender, country_name) |>
    summarise(avg = weighted.mean(math, stu_wgt),
              .groups = "drop") |>
    ungroup() |>
    pivot_wider(country_name,
                names_from = gender,
                values_from = avg) |>
    mutate(diff = female - male,
           country_name = fct_reorder(country_name, diff))

  # Compute bootstrap samples
  set.seed(2020)
  boot_ests_math <- map_dfr(1:100, ~{
    pisa_2018_data_complete |>
      group_by(country_name, gender) |>
      sample_n(size = n(), replace = TRUE) |>
      summarise(avg = weighted.mean(math, stu_wgt),
                .groups = "drop") |>
      pivot_wider(country_name,
                  names_from = gender,
                  values_from = avg) |>
      mutate(diff = female - male,
             country_name = fct_reorder(country_name, diff)) |>
      mutate(boot_id = .x)
  })

  # Compute bootstrap confidence intervals
  math_diff_conf_intervals <- boot_ests_math |>
    group_by(country_name) |>
    summarise(lower = sort(diff)[5],
              upper = sort(diff)[95],
              .groups = "drop") |>
    left_join(math_diff_df, by = "country_name") |>
    mutate(country_name = fct_reorder(country_name, diff)) |>
    mutate(score_class = factor(case_when(
      lower < 0 & upper <= 0 ~ "boys",
      lower < 0 & upper >= 0 ~ "nodiff",
      lower >= 0 & upper > 0 ~ "girls"),
      levels = c("boys", "nodiff", "girls")))

  save(math_diff_conf_intervals,
       file="data/math_diff_conf_intervals.rda")

}

# Compute average math scores and gender diff
if (!file.exists("data/read_diff_conf_intervals.rda")) {
  read_diff_df <- pisa_2018_data_complete |>
    group_by(gender, country_name) |>
    summarise(avg = weighted.mean(read, stu_wgt),
              .groups = "drop") |>
    ungroup() |>
    pivot_wider(country_name, names_from = gender,
                values_from = avg) |>
    mutate(diff = female - male,
           country_name = fct_reorder(country_name, diff))

  # Compute bootstrap samples
  boot_ests_read <- map_dfr(1:100, ~{
    pisa_2018_data_complete |>
      group_by(country_name, gender) |>
      sample_n(size = n(), replace = TRUE) |>
      summarise(avg = weighted.mean(read, stu_wgt),
                .groups = "drop") |>
      ungroup() |>
      pivot_wider(country_name, names_from = gender, values_from = avg) |>
      mutate(diff = female - male, country_name = fct_reorder(country_name, diff)) |>
      mutate(boot_id = .x)
  })

  # Compute bootstrap confidence intervals
  read_diff_conf_intervals <- boot_ests_read |>
    group_by(country_name) |>
    summarise(lower = sort(diff)[5],
              upper = sort(diff)[95],
              .groups = "drop")|>
    left_join(read_diff_df, by = "country_name") |>
    mutate(country_name = fct_reorder(country_name, diff)) |>
    mutate(score_class = factor(case_when(
      lower < 0 & upper <= 0 ~ "boys",
      lower < 0 & upper >= 0 ~ "nodiff",
      lower >= 0 & upper > 0 ~ "girls"),
      levels = c("boys", "nodiff", "girls")))

  save(read_diff_conf_intervals,
       file="data/read_diff_conf_intervals.rda")

}

# Compute average math scores and gender diff
if (!file.exists("data/sci_diff_conf_intervals.rda")) {
  sci_diff_df <- pisa_2018_data_complete |>
    group_by(gender, country_name) |>
    summarise(avg = weighted.mean(science, stu_wgt),
              .groups = "drop") |>
    ungroup() |>
    pivot_wider(country_name, names_from = gender,
                values_from = avg) |>
    mutate(diff = female - male,
           country_name = fct_reorder(country_name, diff))

  # Compute bootstrap samples
  boot_ests_sci <- map_dfr(1:100, ~{
    pisa_2018_data_complete |>
      group_by(country_name, gender) |>
      sample_n(size = n(), replace = TRUE) |>
      summarise(avg = weighted.mean(science, stu_wgt),
                .groups = "drop") |>
      ungroup() |>
      pivot_wider(country_name,
                  names_from = gender,
                  values_from = avg) |>
      mutate(diff = female - male, country_name = fct_reorder(country_name, diff)) |>
      mutate(boot_id = .x)
  })

  # Compute bootstrap confidence intervals
  sci_diff_conf_intervals <- boot_ests_sci |>
    group_by(country_name) |>
    summarise(
      lower = sort(diff)[5],
      upper = sort(diff)[95],
      .groups = "drop")|>
    left_join(sci_diff_df, by = "country_name") |>
    mutate(country_name = fct_reorder(country_name, diff)) |>
    mutate(score_class = factor(case_when(
      lower < 0 & upper <= 0 ~ "boys",
      lower < 0 & upper >= 0 ~ "nodiff",
      lower >= 0 & upper > 0 ~ "girls"),
      levels = c("boys", "nodiff", "girls")))

  save(sci_diff_conf_intervals,
       file="data/sci_diff_conf_intervals.rda")

}

# Run this code to create the data to examine
# the effect of parent's education
student_country_data <- left_join(student_2018,
                                  countrycode,
                                  by = "country")
father_qual_math_read_sci_data <- student_country_data |>
  group_by(country_name, father_educ) |>
  dplyr::summarise(math_avg = weighted.mean(math, w = stu_wgt, na.rm = TRUE),
                   read_avg = weighted.mean(read, w = stu_wgt, na.rm = TRUE),
                   sci_avg  =  weighted.mean(science, w = stu_wgt, na.rm = TRUE)) |>
  dplyr::mutate(father_educ = recode_factor(father_educ,
                                            "less than ISCED1" = "0",
                                            "ISCED 1" = "1",
                                            "ISCED 2" = "2",
                                            "ISCED 3A" = "3",
                                            "ISCED 3B, C" = "3",
                                            .ordered = TRUE)) |>
  na.omit() |>
  rename(`Father's Education` = father_educ)

mother_qual_math_read_sci_data <- student_country_data |>
  group_by(country_name, mother_educ) |>
  dplyr::summarise(math_avg = weighted.mean(math, w = stu_wgt, na.rm = TRUE),
                   read_avg = weighted.mean(read, w = stu_wgt, na.rm = TRUE),
                   sci_avg  =  weighted.mean(science, w = stu_wgt, na.rm = TRUE)) |>
  dplyr::mutate(mother_educ = recode_factor(mother_educ,
                                            "less than ISCED1" = "0",
                                            "ISCED 1" = "1",
                                            "ISCED 2" = "2",
                                            "ISCED 3A" = "3",
                                            "ISCED 3B, C" = "3",
                                            .ordered = TRUE)) |>
  na.omit() |>
  rename(`Mother's Education` = mother_educ)

save(father_qual_math_read_sci_data,
     file="data/father_qual_math_read_sci_data.rda")
save(mother_qual_math_read_sci_data,
     file="data/mother_qual_math_read_sci_data.rda")

# TVs summary
z_star_95 <- qnorm(0.975)

tv_math_data <- student_country_data |>
  group_by(country_name, television) |>
  dplyr::summarise(math_avg =
                     weighted.mean(math,
                                   w = stu_wgt,
                                   na.rm = TRUE),
                   lower = weighted.mean(math,
                                         w = stu_wgt, na.rm = TRUE) -
                     z_star_95 * (sd(math, na.rm = TRUE)) /
                     sqrt(length(math)),
                   upper = weighted.mean(math,
                                         w = stu_wgt, na.rm = TRUE) +
                     z_star_95 * (sd(math, na.rm = TRUE)) /
                     sqrt(length(math)),
                   .groups = "drop") |>
  dplyr::mutate(television = recode_factor(television,
                 "No TV" = "0",
                 "1 TVs" = "1",
                 "2 Tvs" = "2",
                 "3+ TVs" = "3+",
                .ordered = TRUE)) |>
  na.omit() |>
  dplyr::select(country_name,
                television,
                math_avg,
                lower,
                upper)
save(tv_math_data, file="data/tv_math_data.rda")

# Books summary
book_math_read_sci_data <- student_country_data |>
  group_by(country_name, book)  |>
  dplyr::summarise(math_avg =
                     weighted.mean(math,
                                   w = stu_wgt, na.rm = TRUE),
                   bk_lower = weighted.mean(math,
                                            w = stu_wgt, na.rm = TRUE) -
                     z_star_95 * (sd(math, na.rm = TRUE)) /
                     sqrt(length(math)),
                   bk_upper = weighted.mean(math,
                                            w = stu_wgt, na.rm = TRUE) +
                     z_star_95 * (sd(math, na.rm = TRUE)) /
                     sqrt(length(math)), .groups = "drop")  |>
  dplyr::mutate(book = recode_factor(book,
                                     "0-10" = "1",
                                     "11-25" = "11",
                                     "26-100" = "26",
                                     "101-200" = "101",
                                     "201-500" = "201",
                                     "more than 500" = "500",
                                     .ordered = TRUE)) |>
  na.omit()
save(book_math_read_sci_data, file="data/book_math_read_sci_data.rda")

# Load student data, and filter to country, cache a copy of the data
# to save downloading every time paper is rendered

if (!file.exists("data/student_all.rda")) {
  student_all <- load_student("all")
  save(student_all, file="data/student_all.rda")
} else {
  load("data/student_all.rda")
}

# Examine the temporal trend in scores
# Give countries their name, subset if one wants to compare a
# few countries only, and select only variables needed
student_country <- left_join(student_all,
                             countrycode,
                             by = "country") |>
  #dplyr::filter(country_name %in%
  #                c("Australia",
  #                  "New Zealand",
  #                  "Qatar",
  #                  "Indonesia",
  #                  "Singapore",
  #                  "Germany")) |>
  dplyr::select(year, country_name, math, read, science, stu_wgt) |>
  na.omit() |>
  pivot_longer(c(math, read, science), names_to = "subject", values_to = "score")
save(student_country, file="data/student_country.rda")

# Compute the bootstrap confidence intervals, and save result
all_bootstrap <- map_dfr(1:100, ~{
  student_country |>
    group_by(country_name, #year,
             subject) |>
    #sample_n(size = n(), replace = TRUE) |>
    mutate(year = sample(year, replace=FALSE)) |>
    group_by(country_name, year,
             subject) |>
    dplyr::summarise(
      avg = weighted.mean(score, w = stu_wgt, na.rm = TRUE), .groups = "drop") |>
    #ungroup() |>
    mutate(boot_id = .x)
})

all_bootstrap_ci <- all_bootstrap |>
  group_by(country_name, year,
           subject) |>
  summarise(
    lower = min(avg), # sort(avg)[5],
    upper = max(avg), #sort(avg)[95],
    .groups = "drop")

# compute original estimate of average and join
all_avg <- student_country |>
  group_by(country_name, year, subject) |>
  summarise(
    avg = weighted.mean(score,
                        w = stu_wgt, na.rm = TRUE),
    .groups = "drop")

all_bs_cf <- left_join(all_avg,
                       all_bootstrap_ci,
                       by = c("country_name",
                              "year",
                              "subject"))

save(all_bs_cf, file="data/all_bs_cf.rda")

# Run this code once to generate the smaller subset
student_country_anim <- left_join(student_all,
                                  countrycode,
                                  by = "country") |>
  group_by(year) |>
  ungroup() |>
  dplyr::select(year, country, country_name,
                math, read, science, stu_wgt) |>
  na.omit()

student_country_anim_avg <- student_country_anim |>
  group_by(country_name, year) |>
  dplyr::summarise(math_avg =
                     weighted.mean(math, w = stu_wgt,
                                   na.rm = TRUE),
                   read_avg =
                     weighted.mean(read, w = stu_wgt,
                                   na.rm = TRUE),
                   sci_avg  =
                     weighted.mean(science, w = stu_wgt,
                                   na.rm = TRUE),
                   countrycode = country[1],
                   .groups = "drop") |>
  select(country_name, countrycode, year, math_avg, read_avg, sci_avg)

# Need to fix some country names: may be different data
# collections per year, but need some consistency in names
student_country_anim_avg <-
  student_country_anim_avg |>
  mutate(country_name = fct_recode(country_name,
                                   "Argentina" = "Argentina (Ciudad Autónoma de Buenos)",
                                   "Hong Kong" = "Hong Kong SAR China",
                                   "China, Macau" = "Macau SAR China",
                                   "China, Shanghai" = "Shanghai-China",
                                   "China, B-S-J-G" = "B-S-J-G (China)",
                                   "China, B-S-J-Z" = "B-S-J-Z (China)",
                                   "Azerbaijan" = "Baku (Azerbaijan)",
                                   "India, Himachal Pradesh" = "Himachal Pradesh-India",
                                   "India, Tamil Nadu" = "Tamil Nadu-India",
                                   "USA, Massachusetts" = "Massachusettes (USA)",
                                   "USA, North Carolina" = "North Carolina (USA)",
                                   "USA, Puerto Rico" = "Puerto Rico (USA)",
                                   "USA" = "United States",
                                   "Venezuela" = "Miranda-Venezuela",
                                   "Russia, Moscow" = "Moscow Region (RUS)",
                                   "Russia, Perm" = "Perm(Russian Federation)",
                                   "Russia, Tatarstan" = "Tatarstan (RUS)",
                                   "Spain, regional" = "Spain (Regions)"))

# Add continent
country_continent <-
  read_csv("data/country_continent.csv") |>
  select(iso3, continent)

student_country_anim_avg <- left_join(student_country_anim_avg,
                                      select(country_continent, iso3, continent),
                                      by = c("countrycode"="iso3"))

student_anim_data <- student_country_anim_avg

student_anim_data$year <- as.numeric(as.character(student_anim_data$year))

# Only keep countries that have five or more measurements
keep <- student_anim_data |>
  count(country_name) |>
  filter(n > 4) |>
  pull(country_name)

student_anim_data <- student_anim_data |>
  filter(country_name %in% keep)

save(student_anim_data, file="data/student_anim_data.rda")
