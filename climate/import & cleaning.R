# Импорт и очистка данных -------------------------------------------------
library(tidyverse)
library(readxl)
library(plotly)
library(VIM) # использовал для визуализации пропусков
library(imputeTS) # Для интерполяции пропусков
library(WaveletComp)

# Данные из Обнинска ------------------------------------------------------
obninsk_temperature <- read_excel("initial_data/climate/1961_2005/23776_TTTR.xlsx") |>
  select(
    Station = станция,
    Year = год,
    Month = месяц,
    Day = день,
    Tmin = Тмин,
    Tavg = Тср,
    Tmax = Тмакс,
    Pr = осадки
  )
obninsk_snow <- read.csv2("initial_data/climate/1961_2005/23776_snow.csv", fileEncoding = "windows-1251") |>
  select(
    Station = Станция,
    Year = Год,
    Month = Месяц,
    Day = День,
    Sn = Высота_снежного_покрова,
    Sn_description = Снежный_покров_.степень_покрытия
  )
obninsk <- obninsk_temperature |>
  left_join(obninsk_snow, by = c("Year", "Month", "Day")) |>
  select(c(2:8, 10))
# Проверка на повторы
obninsk_temperature |>
  group_by(Station, Year, Month, Day) |>
  summarise_all(mean) |>
  nrow() # осталось столько же строк (16436, значит, повторяющихся данных нет)
obninsk_snow |>
  group_by(Station, Year, Month, Day) |>
  summarise_all(mean) |>
  nrow() # осталось столько же строк (16102, значит, повторяющихся данных нет)


# Данные rp5.ru -----------------------------------------------------------
paths <- list.files("initial_data/climate/2005_2023", pattern = "[.]xls$", full.names = TRUE) # Просканировали все файлы в директории
rp5 <- paths |>
  map(readxl::read_excel) |>
  list_rbind()
rm(paths)

levels(as.factor(rp5$tR)) # видим, что накопленные осадки измерялись не всегда за одинаковое количество времени

rp5_select <- rp5 |>
  separate_wider_delim(Local_time, ".", names = c("Day", "Month", "Year")) |> # Разбиваем на отдельные столбцы
  separate_wider_delim(Year, " ", names = c("Year", "Time")) |>
  select(Year, Month, Day, Tmin = Tn, Tavg = T, Tmax = Tx, Pr = RRR, Sn = sss) |>
  mutate(across(Month:Day, ~ sub("^0+", "", .)), # Чтобы, например, месяцы были не 04, а 4 (удаляем 0 в начале строки)
    Pr = case_when(Pr == "Осадков нет" | Pr == "Следы осадков" ~ "0",
      .default = Pr
    )
  ) |>
  mutate_all(as.numeric) |>
  arrange(Year, Month, Day) |>
  group_by(Year, Month, Day) |>
  summarise(
    Tmin = mean(Tmin, na.rm = T),
    Tavg = mean(Tavg, na.rm = T),
    Tmax = mean(Tavg, na.rm = T),
    Pr = sum(Pr, na.rm = T), # Потому что это накопленные осадки, нужно суммировать
    Sn = mean(Sn, na.rm = T)
  )


# Объединенные данные -----------------------------------------------------
climate <- rbind(obninsk, rp5_select)
rm(obninsk, obninsk_temperature, obninsk_snow, rp5, rp5_select)

ggplot(climate, aes(Year)) +
  geom_line(aes(y = Tavg), col = "black") +
  geom_line(aes(y = Tmin), col = "blue") +
  geom_line(aes(y = Tmax), col = "red") +
  facet_wrap(~Month)
# Явно есть проблемы в месяцах 5-9 в 1960х-70х


with_date <- climate |>
  mutate(Date = make_date(Year, Month, Day)) # датафрейм с датами в формате дат,чтобы plot_ly смог построить интерактивный график

interactive_temperature_min_avg_max <- function(df) {
  result <- plot_ly(df, type = "scatter", mode = "lines") |>
    add_trace(x = ~Date, y = ~Tmin, name = "Tmin") |>
    add_trace(x = ~Date, y = ~Tmax, name = "Tmax") |>
    add_trace(x = ~Date, y = ~Tavg, name = "Tavg") |>
    layout(
      showlegend = T, title = "Temperature",
      xaxis = list(rangeslider = list(visible = T))
    )
  return(result)
}

interactive_temperature_min_avg_max(with_date)
# На интерактивной карте четко видны выбросы и проблемы с летними месяцами 1966-1976
rm(with_date)


# Очистка суточных измерений из Бахты ----------------------------------------------------

summary(climate) # 271 пропуск средней температуры, 3423 пропуса снега, 1458 пропуск осадков
aggr(climate, prop = F, numbers = T) # Визуализация пропусков из пакета VIM

# Если снег равен 9999.0, его надо заменить на NA
climate$Sn[climate$Sn == 9999] <- NA
climate$Sn[climate$Sn == "NaN"] <- NA
aggr(climate, prop = F, numbers = T)

# Снега в летние месяцы быть не может
climate$Sn[is.na(climate$Sn) == T & climate$Month == 6 |
  climate$Month == 7 | climate$Month == 8] <- 0

summary(climate) # Теперь NA(Sn)=1869

# Если осадки NA, а снег и температура не пропущены, то, скорее всего, осадков не было (=0)
climate$Pr[is.na(climate$Pr) == T & is.na(climate$Tavg) == F & is.na(climate$Sn) == F] <- 0
summary(climate) # Теперь NA(Pr)=293(-1165)


# Оставшиеся пропуски удалим методом линейной интерполяции
# (замена с помощью уравнения линейной регрессии y = kx+b)
# (na_interpolation из пакета imputeTS)

climate$Tavg <- na_interpolation(climate$Tavg)
climate$Pr <- na_interpolation(climate$Pr)
climate$Sn <- na_interpolation(climate$Sn)

summary(climate)
aggr(climate, prop = F, numbers = T)


# Проверка на выбросы суточных измерений из Бахты -------------------------
Bakhta <- climate |>
  select(Year, Month, Day, Tavg, Pr, Sn) |>
  mutate(Date = make_date(Year, Month, Day)) # датафрейм с датами в формате дат,чтобы plot_ly смог построить интерактивный график

# График среднесуточных температур в Бахте (без пропусков, но проблемы с летними периодами в 1966-1976)
plot_ly(Bakhta, type = "scatter", mode = "lines") |>
  add_trace(x = ~Date, y = ~Tavg, name = "Tavg") |>
  layout(
    showlegend = T, title = "Temperature",
    xaxis = list(rangeslider = list(visible = T))
  )

# График атмосферных осадков, видно выбросы
plot_ly(Bakhta, type = "scatter", mode = "lines") |>
  add_trace(x = ~Date, y = ~Pr, name = "Precipitation") |>
  layout(
    showlegend = T, title = "Precipitation",
    xaxis = list(rangeslider = list(visible = T))
  )
# Совсем нереальные пики заменил значениями, похожими на соседние
Bakhta$Pr[Bakhta$Pr == 813] <- 2
Bakhta$Pr[Bakhta$Pr == 204.3] <- 3
Bakhta$Pr[Bakhta$Pr == 202] <- 3


# График уровня снежного покрова
plot_ly(Bakhta, type = "scatter", mode = "lines") |>
  add_trace(x = ~Date, y = ~Sn, name = "Snow") |>
  layout(
    showlegend = T, title = "Snow",
    xaxis = list(rangeslider = list(visible = T))
  )

aggr(climate, prop = F, numbers = T) # Пропусков больше нет
write.csv2(Bakhta, "initial_data/climate/cleaned/Bakhta_daily_attention1966-1976(problems_with_summer).csv")


# Бахта расчет среднемесячных температур (1966-1976 исходные) -------------
Bakhta <- read.csv2("initial_data/climate/cleaned/Bakhta_daily_attention1966-1976(problems_with_summer).csv")

Bakhta_monthly <- Bakhta |>
  group_by(Year, Month) |>
  summarise(
    Tavg = mean(Tavg),
    Sn = mean(Sn),
    Pr = sum(Pr)
  ) |> # накопленное количество осадков
  mutate(Date = make_date(Year, Month))

plot_ly(Bakhta_monthly, type = "scatter", mode = "lines") |>
  add_trace(x = ~Date, y = ~Tavg, name = "Tavg") |>
  layout(
    showlegend = T, title = "Temperature",
    xaxis = list(rangeslider = list(visible = T))
  )



# А что с данными с Бора и Верхнеимбатска? --------------------------------
Bor_daily <- read_table("initial_data/climate/daily/Bor_daily.txt", col_names = F) |>
  select(
    Station = X1,
    Year = X2,
    Month = X3,
    Day = X4,
    Tmin = X6,
    Tavg = X7,
    Tmax = X8,
    Pr = X9
  ) |>
  mutate(Date = make_date(Year, Month, Day))
interactive_temperature_min_avg_max(Bor_daily)


Verkhneimbatsk_daily <- read_table("initial_data/climate/daily/Verkhneimbatsk_daily.txt", col_names = F) |>
  select(
    Station = X1,
    Year = X2,
    Month = X3,
    Day = X4,
    Tmin = X6,
    Tavg = X7,
    Tmax = X8,
    Pr = X9
  ) |>
  mutate(Date = make_date(Year, Month, Day))
interactive_temperature_min_avg_max(Verkhneimbatsk_daily)
rm(interactive_temperature_min_avg_max)
# По обеим метеостанциям большие проблемы с суточными измерениями
rm(Verkhneimbatsk_daily, Bor_daily) # Эти не нужны, там много пропусков

# Бор и Верхнеимбатск месячные данные -------------------------------------
Bor_monthly <- read_delim("initial_data/climate/monthly/Bor_monthly.txt",
  col_names = F,
  delim = ";", col_types = "n"
) |>
  mutate_all(as.numeric) |>
  select(
    Station = X1,
    Year = X2,
    "1" = X3,
    "2" = X4,
    "3" = X5,
    "4" = X6,
    "5" = X7,
    "6" = X8,
    "7" = X9,
    "8" = X10,
    "9" = X11,
    "10" = X12,
    "11" = X13,
    "12" = X14
  ) |>
  pivot_longer(cols = -c(Station, Year), names_to = "Month", values_to = "Tavg") |>
  mutate(Day = "1") |>
  mutate(Date = make_date(Year, Month, Day))

plot_ly(Bor_monthly, type = "scatter", mode = "lines") %>%
  add_trace(x = ~Date, y = ~Tavg) %>%
  layout(
    showlegend = F, title = "Месячные температуры в Бору",
    xaxis = list(rangeslider = list(visible = T))
  )

Verkhneimbatsk_monthly <- read_delim("initial_data/climate/monthly/Verkhneimbatsk_monthly.txt",
  col_names = F,
  delim = ";", col_types = "n"
) |>
  mutate_all(as.numeric) |>
  select(
    Station = X1,
    Year = X2,
    "1" = X3,
    "2" = X4,
    "3" = X5,
    "4" = X6,
    "5" = X7,
    "6" = X8,
    "7" = X9,
    "8" = X10,
    "9" = X11,
    "10" = X12,
    "11" = X13,
    "12" = X14
  ) |>
  pivot_longer(cols = -c(Station, Year), names_to = "Month", values_to = "Tavg") |>
  mutate(Day = "1") |>
  mutate(Date = make_date(Year, Month, Day))
plot_ly(Verkhneimbatsk_monthly, type = "scatter", mode = "lines") %>%
  add_trace(x = ~Date, y = ~Tavg) %>%
  layout(
    showlegend = F, title = "Месячные температуры в Верхнеимбатске",
    xaxis = list(rangeslider = list(visible = T))
  )

# Кросс-вейвлет Бахта-Бор-Верхнеимбатск -----------------------------------
Bor_Verkhn <- Bor_monthly |>
  left_join(Verkhneimbatsk_monthly, by = c("Year", "Month")) |>
  select(Year, Month, Bor_tavg = Tavg.x, Verkhn_tavg = Tavg.y) |>
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month)
  )

Bakhta_Bor_Verkhneimbatsk <- Bakhta_monthly |>
  left_join(Bor_Verkhn, by = c("Year", "Month")) |>
  mutate(Date = make_date(Year, Month)) |>
  ungroup() |>
  select(Date, Bakhta_tavg = Tavg, Bor_tavg, Verkhn_tavg) |>
  filter(!is.na(Date)) |>
  as.data.frame()
aggr(Bakhta_Bor_Verkhneimbatsk, prop = F, numbers = T) # В Бору и Верхнеимбатске есть пропуски
# Очистил методом простой линейной интерполяции
Bakhta_Bor_Verkhneimbatsk$Bor_tavg <- na_interpolation(Bakhta_Bor_Verkhneimbatsk$Bor_tavg)
Bakhta_Bor_Verkhneimbatsk$Verkhn_tavg <- na_interpolation(Bakhta_Bor_Verkhneimbatsk$Verkhn_tavg)
aggr(Bakhta_Bor_Verkhneimbatsk, prop = F, numbers = T) # Пропусков нет

Bakhta_Bor_cross <- analyze.coherency(Bakhta_Bor_Verkhneimbatsk, c("Bakhta_tavg", "Bor_tavg"))
wc.image(Bakhta_Bor_cross,
  which.image = "wp", timelab = "time (month)", periodlab = "period (Month)",
  main = "Кросс-вейвлет Бор-Бахта",
  plot.arrow = F,
  show.date = T,
  legend.params = list(lab = "wavelet power-spectrum levels", lab.line = 3.5, label.digits = 3)
)

Bakhta_Verkhn_cross <- analyze.coherency(Bakhta_Bor_Verkhneimbatsk, c("Bakhta_tavg", "Verkhn_tavg"))
wc.image(Bakhta_Verkhn_cross,
  which.image = "wp", timelab = "time (month)", periodlab = "period (Month)",
  main = "Кросс-вейвлет Бахта-Верхнеимбатск",
  plot.arrow = F,
  show.date = T,
  legend.params = list(lab = "wavelet power-spectrum levels", lab.line = 3.5, label.digits = 3)
)

Verkhn_Bor_cross <- analyze.coherency(Bakhta_Bor_Verkhneimbatsk, c("Verkhn_tavg", "Bor_tavg"))
wc.image(Bakhta_Verkhn_cross,
  which.image = "wp", timelab = "time (month)", periodlab = "period (Month)",
  main = "Кросс-вейвлет Бор-Верхнеимбатск",
  plot.arrow = F,
  show.date = T,
  legend.params = list(lab = "wavelet power-spectrum levels", lab.line = 3.5, label.digits = 3)
)

str(Bakhta_Bor_Verkhneimbatsk)



# 1966-1976 Бахта замена на оср Бор - Верхнеимбатск --------
Bor_Verkhn <- Bor_monthly |>
  left_join(Verkhneimbatsk_monthly, by = c("Year", "Month")) |>
  select(Year, Month, Bor_tavg = Tavg.x, Verkhn_tavg = Tavg.y) |>
  filter(Year %in% c(1966:1976), Month %in% c(6:9)) |>
  group_by(Year, Month) |>
  summarise(Tavg = mean(Bor_tavg, Verkhn_tavg)) |>
  mutate(
    Year = as.integer(Year),
    Month = as.integer(Month)
  )

Bakhta_monthly_new <- Bakhta_monthly |>
  left_join(Bor_Verkhn, by = c("Year", "Month")) |>
  mutate(Tavg = ifelse(is.na(Tavg.y) == F, Tavg.y, Tavg.x)) |>
  select(Year, Month, Tavg, Sn, Pr, Date)


# Среднемесячные температуры в Бахте (исходные данные)
plot_ly(Bakhta_monthly, type = "scatter", mode = "lines") |>
  add_trace(x = ~Date, y = ~Tavg, name = "Tavg") |>
  layout(
    showlegend = T, title = "Temperature",
    xaxis = list(rangeslider = list(visible = T))
  )
# График среднемесячных температур в Бахте (1966-1976гг 6-8 мес заменены на оср Бор-Верхнеимбатск)
plot_ly(Bakhta_monthly_new, type = "scatter", mode = "lines") |>
  add_trace(x = ~Date, y = ~Tavg, name = "Tavg") |>
  layout(
    showlegend = T, title = "Temperature",
    xaxis = list(rangeslider = list(visible = T))
  )


write.csv2(Bakhta_monthly_new, "initial_data/climate/cleaned/Bakhta_monthly_1966-1976_replaced_by_average_Bor_Verkhn.csv")
