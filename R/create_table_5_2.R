create_t5.2 <- function(data, date_var, data_year){
outputa <- dth_data |>
  #filter({{date_var}} == data_year & substr(birth1c,1,1) %in% c("E","W")) |>
  filter(dodyr == 2022 & substr(birth1c,1,1) %in% c("E","W")) |>
  group_by(birth2a, death2r) |>
  summarise(total = n()) |>
  pivot_wider(names_from = birth2a, values_from = total, values_fill = 0) |>
  adorn_totals("col") |>
  rename(area = death2r)

outputb <- dth_data |>
  filter(dodyr == 2022 & substr(birth1c,1,1) %in% c("E","W")) |>
  group_by(birth2a, birth1c) |>
  summarise(total = n()) |>
  pivot_wider(names_from = birth2a, values_from = total, values_fill = 0) |>
  adorn_totals("col")|>
  rename(area = birth1c)

output <- rbind(outputa, outputb)

return(output)
}
