#loads in libraries
library(tidyverse) #needed for ggplot and various other functions.
library(data.table) #needed for the 'shift' function.
#sets the working directory

#switches off scientific notation
options (scipen = 999)
#set the number of decimal places to display
options (digits = 10)
#reads in a csv

sc <- dthpops |>
  arrange(year, sex, ageband) |>
  mutate(mx = deaths/pops,
         ax = ifelse(ageband == 1, 0.1, 0.5),
         int = case_when(ageband == 1 ~ 1,
                         ageband == 2 ~ 4,
                         ageband == 20 ~ 2/mx,
                         TRUE ~ 5),
         qx = ifelse(ageband == 20, 1, (int * mx)/(1 + int * (1 - ax) * mx)),
         px = 1 - qx,
         lx = ifelse(ageband == 1, 100000,0))

#setting up the counter for the loop. nice and easy as ageband is 1 to 20.
i <- sc$ageband

for (i in 1:20){sc <- sc |>
  mutate(lx = ifelse(ageband == 1, lx, lag(px, 1) * lag(lx, 1)))}

sc <- sc |>
  arrange(year, sex, desc(ageband)) |>
  mutate(dx = ifelse(ageband ==20, lx, 0))

for (i in 1:20){sc <- sc |>
  mutate(dx = ifelse(ageband == 20, lx, lx - lag(lx, 1)))}

sc <- sc |>
  mutate(Lx = ifelse(ageband ==20, lx / mx, 0))

for (i in 1:20){sc <- sc |>
  mutate(Lx = ifelse(ageband == 20, Lx, int * ((lag(lx, 1)) + (ax * dx))))}

sc <- sc |>
  mutate(Tx = ifelse(ageband == 20, Lx , NA))

for (i in 1:20){sc <- sc |>
  mutate(Tx = ifelse(ageband == 20, Tx, Lx + lag(Tx, 1)))}

sc <- sc |>
  arrange(year, sex, ageband) |>
  mutate(ex = Tx/lx,
         varqx = ifelse(ageband == 20,
                        (4/(deaths * (mx^2))),
                        (int^2 * mx * (1 - ax * int * mx))/( pops * (1 +(1 - ax) * int * mx)^3)))
sc <- sc |>
  arrange(year, sex, desc(ageband)) |>
  mutate(P1 = ifelse(ageband == 20, (((lx / 2)^2)*varqx),(lx^2) * (((1 - ax) * int + lag(ex, 1))^2) * varqx))

for (i in 1:20) {sc$P2 <- ifelse(sc$ageband == 20, sc$P1, sc$P1 + shift(sc$P2, 1, type = "lag"))}

sc <- sc |>
  arrange(year, sex, ageband) |>
  mutate(varex = round(P2/(lx^2), digits = 5),
         SE = round(sqrt(varex), digits = 10),
         CIll = round(ex - SE * 1.96, digits = 5),
         CIul = round(ex + SE * 1.96, digits = 5))

sc <- sc |>
  arrange(year, sex, ageband)

