#' Creates Table 5.1
#'
#' @param death_data name of the data frame for deaths
#' @param death_year_var year variable for death
#' @param death_est_data name of the data frame for estimated deaths
#' @param populations_data name of the data frame for populations
#' @param births_data name of the data frame for births
#' @param birth_year_var year variable for births
#'
#' @return Data frame with tabulated result
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t5.1 <- create_table_5.1(death_data = dth_data, death_year_var = dodyr, death_est_data = dth_est,
#' populations_data = pops, births_data = bth_data, birth_year_var = dobyr)
#'
#'
create_table_5.1 <- function(death_data = dth_data, death_year_var = dodyr, death_est_data = dth_est,
                             populations_data = pops, births_data = bth_data, birth_year_var = dobyr, tablename = "Table_5_1"){

  dth_by_var <- enquo(death_year_var)
  dth_by_var_name <- quo_name(dth_by_var)
  bth_by_var <- enquo(birth_year_var)
  bth_by_var_name <- quo_name(bth_by_var)

  #### Calculate registered deaths
  dth <- {{death_data}} |>
    filter({{death_year_var}} %in% c(2018:2022)) |>
    mutate(sex = case_when(
      sex == "female" ~ "female",
      TRUE ~ "male")) |>
    group_by({{death_year_var}}, sex) |>
    summarise(reg_dths = n())

  dtht <- dth |>
    group_by({{death_year_var}}) |>
    summarise(reg_dths = sum(reg_dths)) |>
    mutate(sex = "total")

  dth <- rbind(dth, dtht)

  reg_dths <- dth |>
    mutate(Indicator = paste0("Registered Deaths ", sex)) |>
    pivot_wider(names_from = {{death_year_var}}, values_from = reg_dths) |>
    select(-sex) |>
    arrange(desc(Indicator))


  #### Calculate estimated deaths
  dthest <- {{death_est_data}} |>
    select(year, male, female, total) |>
    filter(year %in% c(2018:2022)) |>
    group_by(year) |>
    summarise(male = sum(male), female = sum(female), total = sum(total)) |>
    pivot_longer(cols = c(male, female, total), names_to = "Indicator", values_to =  "counts")

  dth <- merge(dth, dthest, by.x = c(dth_by_var_name, "sex"), by.y = c("year", "Indicator"))



  #### calculate completeness
  complete <- dth |>
    mutate(completeness = round_excel((reg_dths/counts)*100, 2)) |>
    mutate(Indicator = paste0("Completeness % ", sex)) |>
    select(Indicator, {{death_year_var}}, completeness) |>
    pivot_wider(names_from = {{death_year_var}}, values_from = completeness) |>
    arrange(desc(Indicator))


  #### calculate populations

  pop_for_dth <- pops |>
    select(starts_with("popu"), sex) |>
    pivot_longer(cols = starts_with("popu"), names_to = "year", values_to = "count" ) |>
    mutate(year = gsub("population_", "", year)) |>
    group_by(year, sex) |>
    summarise(total_pop = sum(count)) |>
    arrange(sex) |>
    select(year, sex, total_pop) |>
    filter(sex == "total")

  cdr <- merge(dtht, pop_for_dth, by.x = c(dth_by_var_name, "sex"), by.y = c("year", "sex")) |>
    mutate(crude_death_rate = round_excel((reg_dths/total_pop)*1000, 2)) |>
    select({{death_year_var}}, sex, crude_death_rate) |>
    rename(year = {{death_year_var}}) |>
    pivot_wider(names_from = year, values_from = crude_death_rate) |>
    mutate(sex = "Crude Death Rate")

  dth <- merge(dth, pop_for_dth, by.x = c(dth_by_var_name, "sex"), by.y = c("year", "sex"))

  #### calculate Under 5 mortality rate
  dthu5 <- {{death_data}} |>
    filter({{death_year_var}} %in% c(2018:2022) & ageinyrs < 5) |>
    group_by({{death_year_var}}) |>
    summarise(reg_dths = n())

  bths <- bth_data |>
    filter({{birth_year_var}} %in% c(2018:2022) & is.na(sbind)) |>
    group_by({{birth_year_var}}) |>
    summarise(total = n())

  under5 <- merge(dthu5, bths, by.x = dth_by_var_name, by.y = bth_by_var_name) |>
    mutate(u5_rate = round_excel((reg_dths/total)*1000, 2),
           year = {{death_year_var}},
           Indicator = "Under-5 mortality") |>
    select(year, Indicator, u5_rate) |>
    pivot_wider(names_from = year, values_from = u5_rate)

  rm(bths, dtht, dthu5, dth)

  #### calculate Life Expectancy

  dthpops <- {{death_data}} |>
    filter({{death_year_var}} %in% c(2018:2022)) |>
    mutate(sex = case_when(
      sex == "female" ~ 2,
      TRUE ~ 1)) |>
    group_by({{death_year_var}}, sex, age_grp_lexp) |>
    summarise(deaths = n()) |>
    rename(year = {{death_year_var}}) |>
    arrange(year, sex, age_grp_lexp)


  le_pops <- pops |>
    select(sex, population_2018, population_2019, population_2020, population_2021, population_2022, age_grp_lexp_pop) |>
    pivot_longer(cols = c(population_2018, population_2019, population_2020, population_2021, population_2022), names_to = "year" ) |>
    mutate(year = as.numeric(gsub("population_", "", year)),
           sex = case_when(
             sex == "F" ~ 2,
             TRUE ~ 1
           )
    ) |>
    group_by(sex, year, age_grp_lexp_pop) |>
    summarise(pops = sum(value)) |>
    arrange(year, sex, age_grp_lexp_pop)

  dthpops <- merge(dthpops, le_pops, by.x = c("sex","age_grp_lexp", "year"), by.y = c("sex", "age_grp_lexp_pop", "year")) |>
    arrange(year, sex, age_grp_lexp) |>
    rename(ageband = age_grp_lexp)

  source("./R/life_expectancy.R")

  le <- sc |>
    filter(ageband == 1) |>
    mutate(sex = case_when(
      sex == 2 ~ "female",
      TRUE ~ "male"
    )) |>
    select(sex, year, ex) |>
    mutate(Indicator =  paste0("life ecpectancy ", sex),
           life_expectancy = round_excel(ex, 1)) |>
    select(Indicator, year, life_expectancy) |>
    pivot_wider(names_from = year, values_from = life_expectancy)


  output <- rbind(reg_dths, complete, cdr, le, under5)
  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  return(output)

}
