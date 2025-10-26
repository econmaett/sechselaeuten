# 01-clean-data.R ----

## load packages ----
library(tidyverse)

## load data ----
### burn duration ----
# https://github.com/philshem/Sechselaeuten-data
burn_duration <- readr::read_csv(file = "https://raw.githubusercontent.com/philshem/Sechselaeuten-data/refs/heads/master/boeoegg_burn_duration.csv") |> 
  mutate(duration = round(burn_duration_seconds / 60, digits = 2)) |> 
  select(year, duration)

### variable selection ----
variable_selection <- c("tre200m0", "tre200mn", "tre200mx", "sre000m0", "sremaxmv", "rre150m0")

### climate metadaata ----
climate_metadata <- readr::read_delim(
  file = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/ogd-smn_meta_parameters.csv", 
  delim = ";"
) |> 
  select(c(1, ends_with("_en"), 10:13)) |> 
  filter(parameter_granularity == "M") |> 
  filter(parameter_shortname %in% variable_selection) |> 
  select(variable = parameter_shortname, description = parameter_description_en)

### climate data ----
climate_data <- readr::read_delim(
  file = "https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/sma/ogd-smn_sma_m.csv", 
  delim = ";"
) |>
  select(date = reference_timestamp, any_of(variable_selection)) |>
  mutate(date = dmy_hm(date), year = year(date), month = month(date)) |>
  filter(month %in% 6:8) |>
  group_by(year) |> 
  summarise(across(.cols = -c(date, month), .fns = \(x) mean(x, na.rm = TRUE))) |>
  ungroup() |> 
  mutate(sre000m0 = sre000m0 / 60) |> 
  mutate(across(.cols = -c(year), .fns = \(x) round(x, digits = 2))) |> 
  mutate(across(.cols = -c(year), .fns = \(x) ifelse(is.nan(x), NA, x)))

### combine datasets ----
sechselaeuten <- left_join(x = burn_duration, y = climate_data, by = join_by(year)) |> 
  mutate(record = ifelse(tre200m0 >= 19, TRUE, FALSE))

## plot ----
p <- ggplot(data = sechselaeuten, mapping = aes(x = duration, y = tre200m0)) +
  geom_point(size = 1.5, colour = "black") +
  geom_smooth(method = "lm", formula = "y ~ x", se = FALSE, colour = "#3d348b") +
  geom_smooth(method = "lm", formula = "y ~ x", se = TRUE, colour = "#7678ed") +
  geom_text(
    data = filter(sechselaeuten, record == TRUE), 
    mapping = aes(x = duration, y = tre200m0, label = year), 
    colour = "#d90429",
    position = position_jitter(width = 0.5, height = 0.5)
  ) +
  scale_x_continuous(breaks = seq(0, 60, 10), limits = c(0, 60)) +
  scale_y_continuous(breaks = seq(14, 22, 2), limits = c(14, 22)) +
  labs(
    title = "Can an exploding snowman predict the summer season?",
    subtitle = "Zurich's Boeoeg as a weather oracle, 1923-2025",
    x = "Duration until head exploded (minutes)", 
    y = "Average summer temperature (°C)"
  ) +
  theme_bw(base_size = 12)

print(p)  

ggsave(filename = "sechselaeuten.png", plot = p, path = ".", width = 14, height = 14, units = "cm", bg = "white")
