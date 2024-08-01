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

  curr_year <- data %>% pull(!!sym(date_var)) %>% max(na.rm = TRUE)
  years <- generate_year_sequence(curr_year)

  dth_by_var <- enquo(death_year_var)
  dth_by_var_name <- quo_name(dth_by_var)
  bth_by_var <- enquo(birth_year_var)
  bth_by_var_name <- quo_name(bth_by_var)

  #### Calculate registered deaths
  dth <- {{death_data}} |>
    filter({{death_year_var}} %in% years) |>
    mutate(death2c = case_when(
      death2c == "female" ~ "female",
      TRUE ~ "male")) |>
    group_by({{death_year_var}}, death2c) |>
    summarise(reg_dths = n())

  dtht <- dth |>
    group_by({{death_year_var}}) |>
    summarise(reg_dths = sum(reg_dths)) |>
    mutate(death2c = "total")

  dth <- rbind(dth, dtht)

  reg_dths <- dth |>
    mutate(Indicator = paste0("Registered Deaths ", death2c)) |>
    pivot_wider(names_from = {{death_year_var}}, values_from = reg_dths) |>
    select(-death2c) |>
    arrange(desc(Indicator))


  #### Calculate estimated deaths
  dthest <- {{death_est_data}} |>
    select(year, male, female, total) |>
    filter(year %in% years) |>
    group_by(year) |>
    summarise(male = sum(male), female = sum(female), total = sum(total)) |>
    pivot_longer(cols = c(male, female, total), names_to = "Indicator", values_to =  "counts")

  dth <- merge(dth, dthest, by.x = c(dth_by_var_name, "death2c"), by.y = c("year", "Indicator"))



  #### calculate completeness
  complete <- dth |>
    mutate(completeness = round_excel((reg_dths/counts)*100, 2)) |>
    mutate(Indicator = paste0("Completeness % ", death2c)) |>
    select(Indicator, {{death_year_var}}, completeness) |>
    pivot_wider(names_from = {{death_year_var}}, values_from = completeness) |>
    arrange(desc(Indicator))


  #### calculate populations

  pop_for_dth <- pops |>
    select(starts_with("popu"), death2c) |>
    pivot_longer(cols = starts_with("popu"), names_to = "year", values_to = "count" ) |>
    mutate(year = gsub("population_", "", year)) |>
    group_by(year, death2c) |>
    summarise(total_pop = sum(count)) |>
    arrange(death2c)

  pop_for_2 <- pop_for_dth |>
    mutate(death2c = "total") |>
    group_by(year, death2c) |>
    summarise(total_pop = sum(total_pop))


  cdr <- merge(dtht, pop_for_dth, by.x = c(dth_by_var_name, "death2c"), by.y = c("year", "death2c")) |>
    mutate(crude_death_rate = round_excel((reg_dths/total_pop)*1000, 2)) |>
    select({{death_year_var}}, death2c, crude_death_rate) |>
    rename(year = {{death_year_var}}) |>
    pivot_wider(names_from = year, values_from = crude_death_rate) |>
    mutate(death2c = "Crude Death Rate")

  dth <- merge(dth, pop_for_dth, by.x = c(dth_by_var_name, "death2c"), by.y = c("year", "death2c"))

  #### calculate Under 5 mortality rate
  dthu5 <- {{death_data}} |>
    filter({{death_year_var}} %in% years & ageinyrs < 5) |>
    group_by({{death_year_var}}) |>
    summarise(reg_dths = n())

  bths <- bth_data |>
    filter({{birth_year_var}} %in% years & is.na(sbind)) |>
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
    filter({{death_year_var}} %in% years) |>
    mutate(death2c = case_when(
      death2c == "female" ~ 2,
      TRUE ~ 1)) |>
    group_by({{death_year_var}}, death2c, age_grp_lexp) |>
    summarise(deaths = n()) |>
    rename(year = {{death_year_var}}) |>
    arrange(year, death2c, age_grp_lexp)


  le_pops <- pops |>
    select(death2c, population_2018, population_2019, population_2020, population_2021, population_2022, age_grp_lexp_pop) |>
    pivot_longer(cols = c(population_2018, population_2019, population_2020, population_2021, population_2022), names_to = "year" ) |>
    mutate(year = as.numeric(gsub("population_", "", year)),
           death2c = case_when(
             death2c == "F" ~ 2,
             TRUE ~ 1)) |>

    group_by(death2c, year, age_grp_lexp_pop) |>
    summarise(pops = sum(value)) |>
    arrange(year, death2c, age_grp_lexp_pop)

  dthpops <- merge(dthpops, le_pops, by.x = c("death2c","age_grp_lexp", "year"), by.y = c("death2c", "age_grp_lexp_pop", "year")) |>
    arrange(year, death2c, age_grp_lexp) |>
    rename(ageband = age_grp_lexp)

  source("./R/life_expectancy.R")

  le <- sc |>
    filter(ageband == 1) |>
    mutate(death2c = case_when(
      death2c == 2 ~ "female",
      TRUE ~ "male")) |>
    select(death2c, year, ex) |>
    mutate(Indicator =  paste0("life ecpectancy ", death2c),
           life_expectancy = round_excel(ex, 1)) |>
    select(Indicator, year, life_expectancy) |>
    pivot_wider(names_from = year, values_from = life_expectancy)


  output <- rbind(reg_dths, complete, cdr, le, under5)

  output_dir <- "./outputs"
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  write.csv(output, paste0(output_dir, "/", tablename, ".csv"), row.names = FALSE)

  return(output)

}

