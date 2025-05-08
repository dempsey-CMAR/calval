library(lubridate)

path <- system.file("testdata", package = "calval")

calval_tracking <- cv_read_calval_tracking()


# cv_val_trimtimes --------------------------------------------------------

tstamp_do <- as_datetime("2025-01-09 10:45:00")
tstamp_temp <- tstamp_do + lubridate::hours(12)

df_tt <- data.frame(
  variable = c("DO", "DO", "DO", "Temp", "Temp", "SAL", "SAL"),
  deployment_utc = c(
    tstamp_do, tstamp_do + lubridate::hours(1), tstamp_do + lubridate::hours(2),
    tstamp_temp, tstamp_temp + lubridate::hours(1),
    tstamp_temp, tstamp_temp + lubridate::hours(1))
) %>%
  mutate(retrieval_utc = deployment_utc + lubridate::days(1))


# cv_round_timestamps -----------------------------------------------------

df_round <- data.frame(
  variable = c(
    "dissolved_oxygen_percent_saturation",
    "salinity_psu",
    "temperature_degree_c",
    "dissolved_oxygen_percent_saturation"
  ),
  timestamp_utc = as_datetime(
    c("2024-01-09 11:21:00",
      "2021-07-30 17:57:00",
      "2020-03-15 08:14:00",
      "2020-03-15 08:14:00"
    ))
)


# cv_assign_tolerance_flag ------------------------------------------------

dat <- readRDS(
  paste(system.file("testdata", package = "calval"),
        "test_data_tolerance.RDS", sep = "/")
  )

dat_tol <- dat %>%
  cv_round_timestamps() %>%
  cv_assign_tolerance_flag() %>%
  mutate(hour_utc = lubridate::hour(timestamp_utc))

# cv_plot_flags(dat_tol, colour_col = "qc_flag")
#
# dat_tol %>%
#   mutate(sensor_serial_number = factor(sensor_serial_number)) %>%
#   cv_plot_flags(colour_col = "sensor_serial_number")

tol_do_4 <- dat_tol %>%
  filter(
    variable == "dissolved_oxygen_percent_saturation",
    ((hour_utc == 2 | hour_utc == 18) & sensor_serial_number == 456) |
      ((hour_utc == 4 | hour_utc == 20) & sensor_serial_number == 789)
  )

tol_do_1 <- dat_tol %>%
  filter(variable == "dissolved_oxygen_percent_saturation") %>%
  dplyr::anti_join(
    tol_do_4,
    by = join_by(timestamp_utc, variable, sensor_type,
                 sensor_serial_number, value, round_timestamp, tolerance, med, tol_lower,
                 tol_upper, qc_flag, hour_utc)
  )

tol_temp_4 <- dat_tol %>%
  filter(
    variable == "temperature_degree_c",
    ((hour_utc == 2 | hour_utc == 18) & sensor_serial_number == 123) |
      ((hour_utc == 4 | hour_utc == 20) & sensor_serial_number == 456) |
      ((hour_utc == 6 | hour_utc == 22) & sensor_serial_number == 789)
  )

tol_temp_1 <- dat_tol %>%
  filter(variable == "temperature_degree_c") %>%
  dplyr::anti_join(
    tol_temp_4,
    by = join_by(timestamp_utc, variable, sensor_type,
                 sensor_serial_number, value, round_timestamp, tolerance, med, tol_lower,
                 tol_upper, qc_flag, hour_utc)
  )



