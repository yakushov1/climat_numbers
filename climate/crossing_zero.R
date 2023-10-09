library(tidyverse)

Bakhta_daily <- read.csv2("initial_data/climate/cleaned/Bakhta_daily_attention1966-1976(problems_with_summer).csv") |>
  filter(Year > 1976)

# Было рассчитано количество дней в каждом году, после которого наступал устойчивый переход температур через 0 градусов.
# Этот показатель играет существенную роль в жизни мелких млекопитающих:
# после перехода температур к положительным значениям наступает вегетационный период.
# К сожалению, данные с 1966 по 1976 год в летне-осеннний период доступны только в месячном разрешении,
# поэтому для расчета даты устойчивого перехода температур через 0 C будут использованы данные только с 1977 года.
# Однако учеты численности мелких млекопитающих на стационаре велись с 1976 года по настоящее время с перерывом на рубеже веков
# поэтому, на наш взгляд, это допущение не повлияет на решение задач работы.

# Переход через 0 весной --------------------------------------------------
# Отфильтруем только март-май, чтобы в этот период искать переходы через 0
only_march_may <- Bakhta_daily |>
  filter(Month %in% c(3:5))

spring_data <- only_march_may |>
  select(-X) |>
  mutate(Year = lubridate::year(Date)) |>
  group_by(Year) |>
  mutate(
    more_then_zero = Tavg > 0, # логический столбец, температура больше 0? (TRUE | FALSE)
    chng1 = cumsum(more_then_zero != lag(more_then_zero, def = first(more_then_zero)))
  )
# lag(more_then_zero, def = first(more_then_zero))
# lag сдвигает вектор на +1 (в начале вектора будет добавлено значение)
# по умолчанию это значение NA, но def переопределяет его на первое в исходном столбце more_then_zero
# cumsum - кумулятивная сумма
# в итоговом столбце будет цифра, обозначающая, какой это переход через 0 (в любую сторону) в текущем году
# в столбце chng1 - порядковый номер перехода температур через 0 в конкретном году

# Отберем первый переход через 0 (chng1 = 1) и последний переход через 0 (max(chng1))
# функция
calculate_interval <- function(df, first_or_last) {
  # на вход датафрейм
  # Рассчитывает количество дней между началом года и
  # первым (first) или последним (last) днем перехода через ноль для каждого года
  df |>
    group_by(Year) |>
    filter(chng1 == ifelse(first_or_last == "first", 1, max(chng1))) |>
    filter(Month == min(Month)) |>
    filter(Day == min(Day)) |>
    mutate(
      Jan_first = make_date(Year), # первое января текущего года
      first_day_above_zero = make_date(Year, Month, Day), # дата первого перехода через ноль
      difference_week = interval(Jan_first, first_day_above_zero) %% years(1) %/% weeks(1), # Сколько полных недель в интервале между первым переходом через ноль и первым января этого года?
      difference_days = interval(Jan_first, first_day_above_zero) %% years(1) %% weeks(1) %/% days(1), # Сколько осталось дней в неполной неделе в остатке?
      result = difference_week * 7 + difference_week
    ) |> # в каждой неделе 7 дней + остатки от деления
    select(Year, Month, Day, result)
}


first_spring <- calculate_interval(spring_data, "first") |>
  rename("First_day_above_zero_spring" = result)

last_spring <- calculate_interval(spring_data, "last") |>
  rename("Last_day_above_zero_spring" = result)

# Результат
# Spr_fst - первый день перехода через 0
# Spr_lst - последний день перехода через 0 (дальше только положительные температуры в случае весны)
# First_day.... Количество дней с начала года до первого дня перехода через ноль
# Last_day.... то же самое, но до последнего дня перехода через ноль

zero_crossing_spring <- first_spring |>
  full_join(last_spring, by = "Year") |>
  rename(
    Month_first = Month.x,
    Day_first = Day.x,
    Month_last = Month.y,
    Day_last = Day.y
  ) |> # Чем меньше показатели, тем раньше наступила "весна"
  mutate(
    Spr_fst = make_date(Year, Month_first, Day_first),
    Spr_lst = make_date(Year, Month_last, Day_last)
  ) |>
  select(Year, Spr_fst, Spr_lst, First_day_above_zero_spring, Last_day_above_zero_spring)
rm(first_spring, last_spring, spring_data, only_march_may)


# Переход через 0 осенью --------------------------------------------------

only_sep_nov <- Bakhta_daily |>
  filter(Month %in% c(9:11))

autumn_data <- only_sep_nov |>
  select(-X) |>
  mutate(Year = lubridate::year(Date)) |>
  group_by(Year) |>
  mutate(
    more_then_zero = Tavg > 0, # логический столбец, температура больше 0? (TRUE | FALSE)
    chng1 = cumsum(more_then_zero != lag(more_then_zero, def = first(more_then_zero)))
  )

first_autumn <- calculate_interval(autumn_data, "first") |>
  rename("First_day_above_zero_autumn" = result)

last_autumn <- calculate_interval(autumn_data, "last") |>
  rename("Last_day_above_zero_autumn" = result)
zero_crossing_autumn <- first_autumn |>
  full_join(last_autumn, by = "Year") |>
  rename(
    Month_first = Month.x,
    Day_first = Day.x,
    Month_last = Month.y,
    Day_last = Day.y
  ) |>
  mutate(
    Aut_fst = make_date(Year, Month_first, Day_first),
    Aut_lst = make_date(Year, Month_last, Day_last)
  ) |>
  select(Year, Aut_fst, Aut_lst, First_day_above_zero_autumn, Last_day_above_zero_autumn)



# Создадим интервалы
intervals <- zero_crossing_autumn |>
  full_join(zero_crossing_spring, by = "Year") |>
  # select(Year, Aut_fst, Aut_lst, Spr_fst, Spr_lst) |>
  mutate(
    First_day_of_year = make_date(Year),
    Last_day_of_year = make_date(Year, month = 12, day = 31)
  )
crossing_zero <- intervals |>
  select(Year, First_day_above_zero_spring, Last_day_above_zero_spring, First_day_above_zero_autumn, Last_day_above_zero_autumn)

write.csv2(crossing_zero, "initial_data/climate/cleaned/zero_crossing.csv")

rm(
  autumn_data, first_autumn, last_autumn, only_sep_nov,
  zero_crossing_autumn, zero_crossing_spring
)


# Назначим новый столбец - Сезон
new <- Bakhta_daily |>
  left_join(intervals, by = "Year") |>
  mutate(Season = ifelse(Date >= First_day_of_year & Date < Spr_fst, "Winter_1",
    ifelse(Date >= Spr_fst & Date <= Spr_lst, "Spring",
      ifelse(Date > Spr_lst & Date < Aut_fst, "Summer",
        ifelse(Date >= Aut_fst & Date <= Aut_lst, "Autumn",
          "Winter_2"
        )
      )
    )
  ))


# Аггрегация по сезонам ------------------------------------------------

by_season <- new |>
  group_by(Year, Season) |>
  summarise(
    Tavg = mean(Tavg),
    Pr_sum = sum(Pr),
    Duration_of_season = n()
  )

# Вычислим среднюю температуру и сумму осадков в зимний период (mean(Winter_2(Year) + Winter_1(Year+1))
by_winter <- by_season |>
  ungroup() |>
  filter(Season %in% c("Winter_1", "Winter_2")) |>
  pivot_wider(names_from = Season, values_from = c(Tavg, Pr_sum, Duration_of_season)) |>
  mutate(
    Tavg_Winter_2_previous = lag(Tavg_Winter_2),
    Pr_sum_Winter_2_previous = lag(Pr_sum_Winter_2),
    Duration_Winter_2_previous = lag(Duration_of_season_Winter_2)
  ) |>
  filter(!(Year %in% c(1977, 2023))) |>
  mutate(
    Tavg = (Tavg_Winter_1 + Tavg_Winter_2_previous) / 2,
    Pr_sum = Pr_sum_Winter_1 + Pr_sum_Winter_2_previous,
    Duration_of_season = Duration_of_season_Winter_1 + Duration_Winter_2_previous
  ) |>
  mutate(Season = "Winter") |>
  select(Year, Season, Tavg, Pr_sum, Duration_of_season)




# Результат ---------------------------------------------------------------
# Среднием температуры и сумма осадков по сезонам
# Сезоны не календарные, а выделены по условию перехода через ноль градусов
# От первого перехода через ноль в положительные значения до последнего в весенние месяцы - так называемая "Весна"
# От первого перехода через ноль в отрицательные значения до последнего в осенние месяцы - так называемая "Осень"
# Между осенью и весной следующего года - "Зима"
# Между весной и осенью этого же года - "Лето"
result <- by_season |>
  filter(Season %in% c("Spring", "Summer", "Autumn")) |>
  rbind(by_winter) |>
  rename(Duration = Duration_of_season) |>
  arrange(Year)

write.csv2(result, "initial_data/climate/cleaned/Bakhta_by_season.csv")

result_for_modelling <- result |>
  pivot_wider(names_from = Season, values_from = c(Tavg, Pr_sum, Duration))

write.csv2(result_for_modelling, "initial_data/climate/cleaned/Bakhta_by_season_for_modelling.csv")
