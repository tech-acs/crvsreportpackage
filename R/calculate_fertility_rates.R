#' calcualtes age specific and total fertility rates per year
#'
#' @param data births data frame
#'
#' @return tablulated age specific and total fertility rates per year
#' @export
#'
#' @import stringr
#'
#' @examples fertility_rates <- calculate_fertility_rates(bth_data)
#'
calculate_fertility_rates <- function(data, pop_data, date_var){
  by_var2 <- enquo(date_var)
  by_var_name <- quo_name(by_var2)

  curr_year <- max(data[[by_var_name]], na.rm = TRUE)
  start_year <- max(curr_year, na.rm = TRUE) - 4

output <- data |>
  filter(is.na(sbind) & !is.na(fert_age_grp) & {{date_var}} %in% c(start_year:curr_year)) |>
  group_by(fert_age_grp, {{date_var}}) |>
  summarise(total = n(), .groups = "drop_last") |>
  pivot_wider(names_from = by_var_name, values_from = total)

# Aggregate population data dynamically for each year
gfr_pops <- pop_data |>
  filter(sex == "F") |>
  group_by(fert_age_grp) |>
  summarise(across(starts_with("population"), sum))

# Merge fertility rate and population data
output <- merge(output, gfr_pops, by = "fert_age_grp") |>
  mutate(across(starts_with("20"),
                ~round(. / get(paste0("population_", str_sub(cur_column(), -4))) * 1000, 1))) |>
  select(fert_age_grp, starts_with("20"))

total_fertility_rates <- output |>
  summarise(across(starts_with("20"), sum)) |>
  mutate(fert_age_grp = 'Total Fertility Rate') |>
  mutate(round_excel(across(starts_with("20"), ~ . * 5 / 1000),2)) |>
  select(fert_age_grp, starts_with("20"))

output <- rbind(output, total_fertility_rates)


return(output)
}


