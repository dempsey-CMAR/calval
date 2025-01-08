# DATE:
# NAME:
# NOTES:
# calval version:

# Code to visualize pre and post deployment validation data


# SET UP -----------------------------------------------------------------------

# Load libraries
#library(calval)
library(dplyr)
library(sensorstrings)
#library(tidyr)


# SECTION 1: **Generate File Path** -------------------------------------------------------------

val_id <- "VAL0055"

# Set the path to the folder with validation data
path <- cv_import_path(val_id)

# SECTION 2: Read in metadata and generate log -----------------------------------------------------------------

calval_tracking <- cv_read_calval_tracking() %>%
  filter(validation_id == val_id)

# create log from metadata
cv_create_val_log(path, calval_tracking)

# SECTION 3: Compile & View -----------------------------------------------------------------

dat_raw <- ss_compile_deployment_data(path, use_config = FALSE) %>%
  select(-c(county, waterbody, station, lease, latitude, longitude,
            string_configuration))

ss_ggplot_variables(dat_raw)

# SECTION 4: Trim & View -----------------------------------------------------------------

# could move this in with the trim function
# leaving here now so that it is easy to review and modify
trimtimes <- cv_val_trimtimes(calval_tracking)

dat <- cv_trim_dat(dat_raw, trimtimes)

dat %>%
  ss_pivot_wider() %>%
  ss_ggplot_variables()

# SECTION 5: Assign tolerance flags -----------------------------------------------------------------

# could combine this with above pipe
dat <- dat %>%
  cv_round_timestamps() %>%
  cv_assign_tolerance_flag()

cv_summarise_flags(dat) %>%
  arrange(desc(Fail))

cv_plot_flags(dat, colour_col = "qc_flag")

dat %>%
  mutate(sensor_serial_number = factor(sensor_serial_number)) %>%
  cv_plot_flags(colour_col = "sensor_serial_number")





