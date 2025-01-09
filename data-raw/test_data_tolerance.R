# January 9, 2025

# library(dplyr)
# library(lubridate)
 library(here)
# library(sensorstrings)
# library(qaqcmar)
# library(tidyr)

#' @importfrom dplyr %>% filter mutate relocate select
#' @importFrom here here
#' @importFrom lubridate as_datetime days
#' @importFrom sensorstrings  ss_convert_depth_to_ordered_factor ss_ggplot_variables

# DO data ----------------------------------------------------------------
timestamp_utc <- seq(
  as_datetime("2023-01-31"), as_datetime("2023-02-01"), by = "15 mins")

n <- length(timestamp_utc)

set.seed(242)
do1 <- data.frame(
  timestamp_utc,
  variable = "dissolved_oxygen_percent_saturation",
  sensor_type = rep("aquameasure", n),
  sensor_serial_number = rep(1000, n),
  value = rnorm(n, 100, 1)
)

set.seed(574)
do2 <- data.frame(
  timestamp_utc,
  variable = "dissolved_oxygen_percent_saturation",
  sensor_type = rep("aquameasure", n),
  sensor_serial_number = rep(456, n),
  value = rnorm(n, 100, 1)
) %>%
  mutate(
    value = case_when(
      hour(timestamp_utc) == 2 ~ 90,
      hour(timestamp_utc) == 18 ~ 110,
      TRUE ~ value)
  )

set.seed(454)
do3 <- data.frame(
  timestamp_utc,
  variable = "dissolved_oxygen_percent_saturation",
  sensor_type = rep("aquameasure", n),
  sensor_serial_number = rep(789, n),
  value = rnorm(n, 100, 1)
) %>%
  mutate(
    value = case_when(
      hour(timestamp_utc) == 4 ~ 92,
      hour(timestamp_utc) == 20 ~ 108,
      TRUE ~ value)
  )

do <- bind_rows(do1, do2, do3) %>%
  cv_round_timestamps() %>%
  cv_assign_tolerance_flag()

cv_plot_flags(do, colour_col = "qc_flag")

do %>%
  mutate(sensor_serial_number = factor(sensor_serial_number)) %>%
  cv_plot_flags(colour_col = "sensor_serial_number")


# temperature data --------------------------------------------------------

timestamp_utc <- seq(
  as_datetime("2023-02-01"), as_datetime("2023-02-02"), by = "15 mins")

n <- length(timestamp_utc)

set.seed(23)
temp1 <- data.frame(
  timestamp_utc,
  variable = "temperature_degree_c",
  sensor_type = rep("hobo", n),
  sensor_serial_number = rep(123, n),
  value = rnorm(n, 15, 0.05)
) %>%
  mutate(
    value = case_when(
      hour(timestamp_utc) == 2 ~ 14.6,
      hour(timestamp_utc) == 18 ~ 15.4,
      TRUE ~ value)
  )

set.seed(569)
temp2 <- data.frame(
  timestamp_utc,
  variable = "temperature_degree_c",
  sensor_type = rep("aquameasure", n),
  sensor_serial_number = rep(456, n),
  value = rnorm(n, 15, 0.05)
) %>%
  mutate(
    value = case_when(
      hour(timestamp_utc) == 4 ~ 14.5,
      hour(timestamp_utc) == 20 ~ 15.6,
      TRUE ~ value)
  )

set.seed(16584)
temp3 <- data.frame(
  timestamp_utc,
  variable = "temperature_degree_c",
  sensor_type = rep("vr2ar", n),
  sensor_serial_number = rep(789, n),
  value = rnorm(n, 15, 0.1)
) %>%
  mutate(
    value = case_when(
      hour(timestamp_utc) == 6 ~ 14.4,
      hour(timestamp_utc) == 22 ~ 15.8,
      TRUE ~ value)
  )

temp <- bind_rows(temp1, temp2, temp3) %>%
  cv_round_timestamps() %>%
  cv_assign_tolerance_flag()

cv_plot_flags(temp, colour_col = "qc_flag")

temp %>%
  cv_plot_flags(colour_col = "sensor_type")

# Pull data together ----------------------------------------------------------------

dat <- bind_rows(temp1, temp2, temp3, do1, do2, do3) %>%
  cv_round_timestamps() %>%
  cv_assign_tolerance_flag()

cv_plot_flags(dat, colour_col = "qc_flag")

dat %>%
  mutate(sensor_serial_number = factor(sensor_serial_number)) %>%
  cv_plot_flags(colour_col = "sensor_serial_number")

# Export rds file
saveRDS(dat, file = here("inst/testdata/test_data_tolerance.RDS"))

