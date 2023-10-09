library(tidyverse)
library(ggpmisc)
library(patchwork)

# Импорт ------------------------------------------------------------------
by_season <- read.csv2("initial_data/climate/cleaned/Bakhta_by_season.csv") |>
  select(-1)
by_month <- read.csv2("initial_data/climate/cleaned/Bakhta_monthly_1966-1976_replaced_by_average_Bor_Verkhn.csv") |>
  select(-1)
zero_crossing <- read.csv2("initial_data/climate/cleaned/zero_crossing.csv") |>
  select(-1)
extreme <- read.csv2("initial_data/climate/cleaned/total_extreme.csv") |>
  select(-1)
duration_of_season_by_snow <- read.csv2("initial_data/climate/cleaned/Duration_of_season(snow)_for_graph.csv") |>
  select(-1)

start_of_season_by_snow <- read.csv2("initial_data/climate/cleaned/start_end_of_spring_autumn_accroding_snow_cover.csv") |>
  select(-1) |> 
  pivot_longer(-Year, values_to = "Day", names_to = "Season") |> 
  mutate(Season = ifelse(Season == "First_day_above_zero_autumn", "Осень(начало)",
                         ifelse(Season == "First_day_above_zero_spring", "Весна(начало)",
                                ifelse(Season == "Last_day_above_zero_autumn", "Осень(окончание)",
                                       "Весна(окончание)"
                                )
                         )
  ))

# Расчет среднегодовых значений -------------------------------------------
average_by_year <- by_month |>
  group_by(Year) |>
  summarise(
    Tavg = mean(Tavg),
    Pr = sum(Pr)
  )

# Среднесезонные - замена на русские названия сезонов ---------------------
by_season_in_russian <- by_season |>
  mutate(Season = ifelse(Season == "Autumn", "Осень",
    ifelse(Season == "Spring", "Весна",
      ifelse(Season == "Summer", "Лето", "Зима")
    )
  ))

# Продолжительность и начало сезонов (по переходу температур через ноль) - замена на русские названия ---------
zero_crossing_in_russian <- zero_crossing |>
  pivot_longer(-Year, values_to = "Day", names_to = "Season") |>
  mutate(Season = ifelse(Season == "First_day_above_zero_autumn", "Осень(начало)",
    ifelse(Season == "First_day_above_zero_spring", "Весна(начало)",
      ifelse(Season == "Last_day_above_zero_autumn", "Осень(окончание)",
        "Весна(окончание)"
      )
    )
  ))


# на сколько сдвинулось по сравнению с базовым периодом?
# начало сезонов в базовый период
start_base <- zero_crossing_in_russian |> 
  group_by(Season) |> 
  filter(Year %in% c(1976:1990)) |> 
  summarise(Avg = round(mean(Day),0))

# начало сезонов в последнее десятилетие
start_of_season_last_ten_years <- zero_crossing_in_russian |> 
  group_by(Season) |> 
  filter(Year %in% c(2013:2023)) |> 
  summarise(Avg_last = round(mean(Day, na.rm = T),0)) |> 
  left_join(start_base, by = "Season") |> 
  mutate(diff = Avg_last - Avg)

# Продолжительность бесснежных/наоборот сезонов-замена на русские --------
duration_of_season_by_snow_in_russian <- duration_of_season_by_snow |>
  mutate(Season = ifelse(Season == "Autumn", "Осень",
    ifelse(Season == "Spring", "Весна",
      ifelse(Season == "Winter", "Зима",
        "Лето"
      )
    )
  ))

# на сколько сдвинулось по сравнению с базовым периодом?
# начало сезонов в базовый период
start_base_by_snow <- duration_of_season_by_snow_in_russian |> 
  group_by(Season) |> 
  filter(Year %in% c(1976:1990)) |> 
  summarise(Avg = round(mean(Day),0))

# На сколько сдвинулись сезоны по сравнению с 1976-1990 (по снегу) --------
# начало сезонов в базовый период
start_base_by_snow <- start_of_season_by_snow |> 
  group_by(Season) |> 
  filter(Year %in% c(1976:1990)) |> 
  summarise(Avg = round(mean(Day),0))

# начало сезонов в последнее десятилетие
start_of_season_by_snow_last_ten_years <- start_of_season_by_snow |> 
  group_by(Season) |> 
  filter(Year %in% c(2013:2023)) |> 
  summarise(Avg_last = round(mean(Day, na.rm = T),0)) |> 
  left_join(start_base_by_snow, by = "Season") |> 
  mutate(diff = Avg_last - Avg)

# начало сезонов в последнее десятилетие
start_of_season_last_ten_years <- zero_crossing_in_russian |> 
  group_by(Season) |> 
  filter(Year %in% c(2013:2023)) |> 
  summarise(Avg_last = round(mean(Day, na.rm = T),0)) |> 
  left_join(start_base, by = "Season") |> 
  mutate(diff = Avg_last - Avg)

# Расчёт аномалий ---------------------------------------------------------
# средние значения за опорный период
base_annual <- by_month |>
  filter(Year %in% c(1961:1990)) |>
  summarise(
    Tavg_base = mean(Tavg),
    Pr_base = mean(Pr)
  )
# среднегодовые аномалии
Bakhta_annual_anomaly <- by_month |>
  group_by(Year) |>
  summarise(
    Tavg = mean(Tavg),
    Pr = mean(Pr),
    Sn = mean(Sn)
  ) |>
  mutate(
    Tavg_base = base_annual$Tavg_base,
    Pr_base = base_annual$Pr_base
  ) |>
  mutate(
    Tavg_anomaly = Tavg - Tavg_base,
    Pr_anomaly = Pr - Pr_base
  )


# Разница между среднегодовым за 2013-2023 и средними значениями за 1961-1990
avg_last_ten_years_by_year <- by_month |> 
  filter(Year %in% c(2013:2023)) |>
  summarise(
    Tavg_last_ten = mean(Tavg),
    Pr_last_ten = mean(Pr)
  ) |> 
  cbind(base_annual) |> 
  mutate(tavg_diff = Tavg_last_ten - Tavg_base,
         pr_diff = Pr_last_ten - Pr_base)


# среднемесячные за опорный период
base_monthly <- by_month |>
  filter(Year %in% c(1961:1990)) |>
  group_by(Month) |>
  summarise(
    Tavg_base = mean(Tavg),
    Pr_base = mean(Pr)
  )
# аномалии по месяцам
Bakhta_monthly_anomaly <- by_month |>
  left_join(base_monthly, by = c("Month")) |>
  mutate(
    Tavg_anomaly = Tavg - Tavg_base,
    Pr_anomaly = Pr - Pr_base
  )

# Разница между температурой за 2013-2023 и базовым периодом помесячно
tavg_last_ten_years <- by_month |> 
  filter(Year %in% c(2013:2023)) |> 
  group_by(Month) |> 
  summarise(Tavg_last_ten = mean(Tavg),
            Pravg_last_ten = mean(Pr)) |> 
  left_join(base_monthly, by = "Month") |> 
  mutate(diff_tavg = Tavg_last_ten - Tavg_base,
         diff_pr = Pravg_last_ten - Pr_base)

# среднесезонные за опорный период - посчитать нельзя, так как не получилось выделить сезоны с 1961 года (нет данных в суточном разрешении)
# Функции -----------------------------------------------------------------
# добавляет на график линию тренда и уравнение
same_components_for_graph <- function() {
  list(
    stat_poly_line(color = "red", size = 0.5),
    stat_poly_eq(use_label(c("eq"))),
    stat_poly_eq(use_label(c("R2", "P")), label.y = 0.85),
    scale_x_continuous(breaks = c(1961, 1971, 1981, 1991, 2001, 2011, 2023)),
    theme_minimal(),
    theme(
      text = element_text(
        family = "sans", size = 14,
        colour = "black"
      ),
      plot.caption = element_text(
        hjust = 0.5,
        lineheight = 0.5
      ),
      plot.title = element_text(
        hjust = 0.5,
        lineheight = 0.5
      )
    )
  )
}

# общий шаблон графиков
graph <- function(geom = geom_line(),
                  facet = facet_wrap(. ~ Month, ncol = 3),
                  x_axis_angle = 0,
                  ylimit = c(NA, NA),
                  ylabel = "Температура, [\u00B0C]",
                  xlabel = "Год") {
  list(
    geom,
    facet,
    same_components_for_graph(),
    theme(axis.text.x = element_text(angle = x_axis_angle, vjust = .5)),
    coord_cartesian(ylim = ylimit),
    labs(
      title = "",
      x = xlabel,
      y = ylabel
    )
  )
}

coord_cartisian(ylim(0,NA))
# Графики по исходным значениям--------------------------------------------

# Температуры среднегодовые/среднемесячные --------------------------------
temp_by_month <- by_month |>
  ggplot(aes(Year, Tavg)) +
  graph(
    x_axis_angle = 90,
    ylimit = c(NA, 50)
  )
temp_by_year <- average_by_year |>
  ggplot(aes(Year, Tavg)) +
  graph(
    x_axis_angle = 0,
    facet = facet_null(),
    ylimit = c(NA, NA),
    xlabel = NULL
  )
temp_by_month_year <- temp_by_year / temp_by_month +
  plot_layout(heights = c(1, 4))

ggsave(
  device = png, filename = "images/climate/average_temperature_by_years_and_month.png",
  plot = temp_by_month_year, bg = "transparent", width = 2480, height = 3000, units = "px"
)


# Суммы осадков по месяцам/среднегодовые ----------------------------------
pr_by_month <- by_month |>
  ggplot(aes(Year, Pr)) +
  graph(
    geom = geom_col(fill = "lightblue"),
    ylabel = "Сумма осадков, мм",
    x_axis_angle = 90
  )

pr_by_year <- average_by_year |>
  ggplot(aes(Year, Pr)) +
  graph(
    geom = geom_col(fill = "lightblue"),
    facet = facet_null(),
    ylabel = "Сумма осадков, мм",
    xlabel = NULL,
    x_axis_angle = 0
  )
pr_by_month_year <- pr_by_year / pr_by_month +
  plot_layout(heights = c(1, 4))

ggsave(
  device = png, filename = "images/climate/average_precipitation_by_years_and_month.png",
  plot = pr_by_month_year, bg = "transparent", width = 2480, height = 3000, units = "px"
)

# Среднесезонные температуры и осадки -------------------------------------
temp_by_season <- by_season_in_russian |>
  ggplot(aes(Year, Tavg)) +
  graph(
    facet = facet_wrap(. ~ Season, ncol = 1),
    x_axis_angle = 0,
    ylimit = c(NA, 25)
  )

pr_by_season <- by_season_in_russian |>
  ggplot(aes(Year, Pr_sum)) +
  graph(
    geom = geom_col(fill = "lightblue"),
    facet = facet_wrap(. ~ Season, ncol = 1),
    ylabel = "Сумма осадков, мм",
    x_axis_angle = 0
  )

temp_pr_by_season <- temp_by_season + pr_by_season

ggsave(
  device = png, filename = "images/climate/temperature_and_precipitation_by_season.png",
  plot = temp_pr_by_season, bg = "transparent", width = 2480, height = 3000, units = "px"
)

# Продолжительность и начало "сезонов" (выделено по температурным переходам) ---------------------------------------------
duration_of_season <- by_season_in_russian |>
  ggplot(aes(Year, Duration)) +
  graph(
    geom = geom_col(fill = "mistyrose1"),
    facet = facet_wrap(. ~ Season, ncol = 4),
    x_axis_angle = 90,
    xlabel = NULL,
    ylabel = "Продолжительность, дней"
  )

start_of_season_spring <- zero_crossing_in_russian |>
  filter(Season %in% c("Весна(начало)", "Весна(окончание)")) |>
  ggplot(aes(Year, Day)) +
  graph(
    geom = geom_col(fill = "mistyrose1"),
    facet = facet_wrap(. ~ Season, ncol = 4),
    x_axis_angle = 90,
    xlabel = "Год",
    ylabel = "Количество дней от начала года",
    ylimit = c(0, 200)
  )

start_of_season_autumn <- zero_crossing_in_russian |>
  filter(Season %in% c("Осень(начало)", "Осень(окончание)")) |>
  ggplot(aes(Year, Day)) +
  graph(
    geom = geom_col(fill = "mistyrose1"),
    facet = facet_wrap(. ~ Season, ncol = 4),
    x_axis_angle = 90,
    xlabel = NULL,
    ylabel = NULL,
    ylimit = c(200, 400)
  )
duration_start_season <- duration_of_season / (start_of_season_spring + start_of_season_autumn)
ggsave(
  device = png, filename = "images/climate/duration_start_season.png",
  plot = duration_start_season, bg = "transparent", width = 2480, height = 3000, units = "px"
)

# Графики для аномалий ----------------------------------------------------
# Аномалии температур по годам + месяцам ----------------------------------
anomaly_temp_by_year <- Bakhta_annual_anomaly |>
  ggplot(aes(Year, Tavg_anomaly)) +
  graph(
    x_axis_angle = 0,
    facet = facet_null(),
    ylabel = "Аномалии, [\u00B0C]",
    xlabel = NULL
  )

anomaly_temp_by_month <- Bakhta_monthly_anomaly |>
  ggplot(aes(Year, Tavg_anomaly)) +
  graph(
    x_axis_angle = 90,
    facet = facet_wrap(. ~ Month, ncol = 3),
    ylabel = "Аномалии, [\u00B0C]",
    xlabel = "Год",
    ylimit = c(-10, 25)
  )

anomaly_temp_year_month <- anomaly_temp_by_year / anomaly_temp_by_month +
  plot_layout(heights = c(1, 4))
ggsave(
  device = png, filename = "images/climate/anomaly_temperature_by_years_and_month.png",
  plot = anomaly_temp_year_month, bg = "transparent", width = 2480, height = 3000, units = "px"
)

# Аномалии сумм осадков по годам и по месяцам -----------------------------

anomaly_pr_by_year <- Bakhta_annual_anomaly |>
  ggplot(aes(Year, Pr_anomaly)) +
  graph(
    geom = geom_col(fill = "lightblue"),
    x_axis_angle = 0,
    facet = facet_null(),
    ylabel = "Аномалии, мм",
    xlabel = NULL
  )

anomaly_pr_by_month <- Bakhta_monthly_anomaly |>
  ggplot(aes(Year, Pr_anomaly)) +
  graph(
    geom = geom_col(fill = "lightblue"),
    x_axis_angle = 90,
    facet = facet_wrap(. ~ Month, ncol = 3),
    ylabel = "Аномалии, мм",
    xlabel = "Год"
  )

anomaly_pr_month_year <- anomaly_pr_by_year / anomaly_pr_by_month +
  plot_layout(heights = c(1, 4))
ggsave(
  device = png, filename = "images/climate/anomaly_precipitation_by_years_and_month.png",
  plot = anomaly_pr_month_year, bg = "transparent", width = 2480, height = 3000, units = "px"
)


# Продолжительность снежного/бесснежного периодов -------------------------
# Особенно важны переходные периоды: Весна и Осень (когда снег то таял, то был >0)
# "Лето" наступало после последнего перехода снега в ноль
# Осень - после первого снегопада и до устойчивого перехода в >0 см

duration_of_snow_period <- duration_of_season_by_snow_in_russian |> 
  ggplot(aes(Year, Duration))+
  graph(
    geom = geom_col(fill = "mistyrose1"),
    x_axis_angle = 0,
    facet = facet_wrap(. ~ Season, ncol = 2, scales = "free_y"),
    ylabel = "Продолжительность, дней",
    xlabel = NULL
  )

duration_of_spring_by_snow <- duration_of_season_by_snow_in_russian |> 
  filter(Season == "Весна") |> 
  ggplot(aes(Year, Duration))+
  geom_col(fill = "lightblue")+
  theme_bw()+
  labs(x = NULL,
        y = "Продолжительность, дней")+
  theme(text = element_text(size = 14))


ggsave(
  device = png, filename = "images/climate/duration_of_snow_period.png",
  plot = duration_of_snow_period, bg = "transparent", width = 2480, height = 3000, units = "px"
)

# Начало/конец весны/осени (по снегу)

start_of_season_spring_by_snow <- start_of_season_by_snow |>
  filter(Season %in% c("Весна(начало)", "Весна(окончание)")) |>
  ggplot(aes(Year, Day)) +
  graph(
    geom = geom_col(fill = "mistyrose1"),
    facet = facet_wrap(. ~ Season, ncol = 2),
    x_axis_angle = 90,
    xlabel = "Год",
    ylabel = "Количество дней от начала года",
    ylimit = c(0, 200)
  )

start_of_season_autumn_by_snow <- start_of_season_by_snow |>
  filter(Season %in% c("Осень(начало)", "Осень(окончание)")) |>
  ggplot(aes(Year, Day)) +
  graph(
    geom = geom_col(fill = "mistyrose1"),
    facet = facet_wrap(. ~ Season, ncol = 2),
    x_axis_angle = 90,
    xlabel = NULL,
    ylabel = NULL,
    ylimit = c(200, 400)
  )
duration_start_season_by_snow <- duration_of_snow_period / (start_of_season_spring_by_snow + start_of_season_autumn_by_snow)
ggsave(
  device = png, filename = "images/climate/duration_start_season_by_snow.png",
  plot = duration_start_season_by_snow, bg = "transparent", width = 2480, height = 3000, units = "px"
)
# Количество экстремальных дней - Доработать -------------------------------------------
# spring_extreme_for_graph <- extreme |>
#   select(1:7) |>
#   pivot_longer(-Year, values_to = "Day", names_to = "Season")
#
# ggplot(spring_extreme_for_graph, aes(Year, Day, fill = Season))+
#   geom_col()
