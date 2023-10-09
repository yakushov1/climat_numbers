# Расчет экстремальных периодов -------------------------------------------
library(tidyverse)
library(foreach)

# Импорт и подготовка -----------------------------------------------------
data <- read.csv2("initial_data/climate/cleaned/Bakhta_daily_attention1966-1976(problems_with_summer).csv") |>
  select(Year:Sn)

# Для весенних экстримов нужны периоды:
#   1976-1994, 2008-2023, март-май
# Для осенних экстримов нужны периоды:
#   1975-1993, 2007-2022, сентябрь-ноябрь

filtered_data_spring <- data |>
  filter(
    Year %in% c(1976:1994, 2008:2023),
    Month %in% c(3:5)
  )

filtered_data_autumn <- data |>
  filter(
    Year %in% c(1975:1993, 2007:2022),
    Month %in% c(9:11)
  )

# Функция -----------------------------------------------------------------
extreme_test <- function(df, min_temp, min_sn) {
  first_step_result <- function(df, temp, min_sn) {
    # Отберём строки, соответствующие условию (снег меньше min_sn, температура ниже min_temp), при условии,
    # если соседние строки отвечают этому же условию
    string_with_extreme <- foreach(i = 1:nrow(df), .combine = "rbind") %:%
      when(df[i, 4] < temp & df[i, 6] <= min_sn) %:%
      when(df[i - 1, 4] < temp & df[i - 1, 6] <= min_sn) %:%
      when(df[i + 1, 4] < temp & df[i + 1, 6] <= min_sn) %do%
      df[i, ]

    result <- string_with_extreme |>
      group_by(Year) |>
      summarise(N = n()) |>
      mutate(N = N + 2) |>
      mutate(Temperature = temp)
    # Только надо учесть 1 вещь
    # Допустим, был ряд  (- не удовлетв условию(снег меньше 5, температура меньше 0) + удовлетворял)
    # --++++--
    # Функция посчитает именно экстримами только 2 средних плюса
    # Потому что для крайних значений условие не удовлетворяется полностью (либо предыдущая, либо последующая строка не будет экстримом)
    # Поэтому, по идее, нужно будет прибавить к посчитанному количеству еще 2 дня с плохими условиями
    return(result)
  }
  # применим функцию first_step_result к диапазону пороговых температур от 0 до min_temp
  final_result <- foreach(temp = 0:min_temp, .combine = "rbind") %do%
    first_step_result(df, temp, min_sn)
  return(final_result)
}

# Результат ---------------------------------------------------------------

spring_extreme <- extreme_test(filtered_data_spring, -5, 5)
# Температура меньше -5 выдает ошибку, так как дней, когда снег меньше 5 см,
# а температура меньше, например, -6, в течение 3 дней подряд весной не было
autumn_extreme <- extreme_test(filtered_data_autumn, -5, 5)

rm(data, filtered_data_autumn, filtered_data_spring, extreme_test)

# Переформатируем в "широкий" вид
spring_extreme_wide <- spring_extreme |>
  pivot_wider(names_from = Temperature, values_from = N, names_prefix = "Spring_extreme_")

# В autumn_extreme_wide важная деталь!!!
# Осенние экстримы теоретически могут повлиять на численность не в текущих отловах, а
# в отловах следующего года. Поэтому Year+1, а в таблице для модели год, например, 1975(+1) = 1976
# то есть осень 1975 года будет влиять на численность в 1976 году.
autumn_extreme_wide <- autumn_extreme |>
  pivot_wider(names_from = Temperature, values_from = N, names_prefix = "Autumn_extreme_") |>
  mutate(Year = Year + 1)

total_extreme <- spring_extreme_wide |>
  left_join(autumn_extreme_wide, by = "Year")
total_extreme[is.na(total_extreme)] <- 0 # Все NA заменили на 0

write.csv2(total_extreme, "initial_data/climate/cleaned/total_extreme.csv")
