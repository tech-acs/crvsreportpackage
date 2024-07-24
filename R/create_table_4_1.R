#' Calculates Table 4.1 Births summary table
#'
#' @param data births data frame
#' @param date_var variable for year
#' @param tablename name for csv output use _ instead of . for names
#'
#' @return data frame with tabulated results
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import janitor
#'
#' @examples t4.1 <- create_t4.1(bth_data, date_var = dobyr, tablename = "Table_4_1")
#'
create_t4.1 <- function(data, est_data, date_var, tablename = "Table_4_1"){
  by_var2 <- enquo(date_var)
  by_var_name <- quo_name(by_var2)

  curr_year <- max(bth_data[[by_var_name]], na.rm = TRUE)
  start_year <- max(curr_year, na.rm = TRUE) - 4


  output <- data |>
    filter(is.na(sbind) & {{date_var}} %in% c(start_year:curr_year) & sex != "not stated")|>
    group_by(sex, {{date_var}}) |>
    rename(Indicator = sex) |>
    summarise(total = n())

  outputt <- output |>
    group_by({{date_var}}) |>
    summarise(total = sum(total))


  outputt <- data.frame(Indicator = "total", outputt)
  output <- bind_rows(output, outputt)

  output_counts <- output |>
    pivot_wider(names_from = Indicator, values_from = total)

  output_comp <- est_data |>
    filter(year %in% c(start_year:curr_year)) |>
    group_by(year) |>
    summarise(ftotal = sum(female), mtotal = sum(male), ttotal = sum(male + female))


  output_comp <- cbind(output_counts, output_comp) |>
    mutate(male_comp = round_excel(male/mtotal*100, 1),
           female_comp = round_excel(female/ftotal*100, 1),
           all_comp = round_excel(total/ttotal*100, 1)) |>
    select(year, male_comp, female_comp, all_comp) |>
    pivot_longer(cols = c(male_comp, female_comp, all_comp), names_to = "Indicator", values_to =  "counts") |>
    pivot_wider(names_from = year, values_from = counts)

  output_counts <- output_counts |>
    pivot_longer(cols = c(male, female, total), names_to = "Indicator", values_to =  "counts") |>
    pivot_wider(names_from = by_var_name, values_from = counts)

  population <- pops |>
    select(starts_with("popu"), sex) |>
    pivot_longer(cols = starts_with("popu"), names_to = "year", values_to = "count" ) |>
    mutate(year = gsub("population_", "", year)) |>
    group_by(year, sex) |>
    summarise(total_pop = sum(count)) |>
    arrange(sex)

  popst <- population |>
    group_by(year) |>
    summarise(total_pop = sum(total_pop))


  popst <- data.frame(sex = "total", popst)
  population <- bind_rows(population, popst)

  output_cbr <- cbind(output, population) |>
    select(year, Indicator, total, total_pop ) |>
    group_by(year) |>
    summarise(total = sum(total), total_pop = sum(total_pop)) |>
    mutate(cbr = round_excel((total/total_pop)*1000,2)) |>
    select(year, cbr) |>
    mutate(Indicator = "Crude Birth Rate") |>
    pivot_wider(names_from = year, values_from = cbr)

  output_ratio <- output |>
    pivot_wider(names_from = Indicator, values_from = total) |>
    mutate(total = male + female,
           Ratio = round((male/female),2)) |>
    select({{date_var}}, Ratio) |>
    pivot_wider(names_from = by_var_name, values_from = Ratio) |>
    mutate(Indicator = "Sex Ratio at Birth") |>
    select(Indicator, starts_with("20"))
  output_ratio <- output_ratio[1,]


  fertility_rates <- calculate_fertility_rates(bth_data, pops, dobyr) |>
    rename(Indicator = fert_age_grp) |>
    filter(Indicator == "Total Fertility Rate")

  output <- rbind(output_counts, output_comp, output_ratio, output_cbr, fertility_rates)
  output <- output[match(t4.1_order, output$Indicator), ]

  write.csv(output, paste0("./outputs/", tablename, ".csv"), row.names = FALSE)
  write.csv(outputt, paste0("./outputs/figure_4_1.csv"), row.names = FALSE)
  return(output)

}
