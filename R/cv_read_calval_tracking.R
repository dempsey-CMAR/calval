#' Read in CMAR calval tracking sheet
#'
#' @param link Link to the calval tracking sheet on Google Drive. Default is the
#'   CMAR tracking sheet.
#'
#' @param sheet Character string with the name of the sheet to read in.
#'
#' @importFrom dplyr select
#' @importFrom googlesheets4 gs4_deauth read_sheet
#' @importFrom lubridate with_tz
#' @importFrom stringr str_detect
#'
#'
#' @return Returns a data frame of the calval tracking sheet. Deployment and
#'   retrieval datetimes are appended in "Canada/Atlantic" and "UTC" timezones.
#' @export


cv_read_calval_tracking <- function(link = NULL, sheet = NULL) {

  if(is.null(link)) {
    link <- "https://docs.google.com/spreadsheets/d/1u1beyNL02NQvMblhkpGX9tazRqlhfZaJbzifvOKNP54/edit#gid=0"
  }

  if(is.null(sheet)) {
    sheet <- ifelse(
      str_detect(val_id, "POST"),
      "Post Deployment Validation",
      "Pre Deployment CalVal"
    )
  }

  googlesheets4::gs4_deauth()

  x <- googlesheets4::read_sheet(
    link,
    col_types = "cci-c-----ccDc--Dc--cnnnc----",
    na = c("", "NA", "N/A", "n/a")
  ) %>%
    dplyr::select(
      validation_id = `validation event id`,
      sensor_model = `sensor model`,
      sensor_serial_number = `serial number`,

      val_start_date = `validation start date`,
      val_start_time = `validation start time (AST)`,

      val_end_date = `validation end date`,
      val_end_time = `validation end time (AST)`,

      variable = `validation variable`,
      val_status = `validation status (CMAR USE)`,
      percent_bad_do = `percent bad do readings`,
      percent_bad_temp = `percent bad temp readings`,
      percent_bad_sal = `percent bad sal readings`,

      calibration_attendant = `name of calibration attendant`,
      validation_attendant = `name of validation attendant`,
      notes = notes
    ) %>%
    mutate(
      val_start_time = paste0(val_start_time, ":00"),
      val_end_time = paste0(val_end_time, ":00"),

      deployment_can = as_datetime(
        paste(val_start_date, val_start_time), tz = "Canada/Atlantic"),

      retrieval_can = as_datetime(
        paste(val_end_date, val_end_time), tz = "Canada/Atlantic"),

      deployment_utc = with_tz(deployment_can, tzone = "UTC"),
      retrieval_utc = with_tz(retrieval_can, tzone = "UTC")
    )
}
