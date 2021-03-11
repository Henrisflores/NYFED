# load_data

# -------------------------------------------------
#  User inputs.
#   vintage = '2016-06-29';  vintage dataset to use for estimation
#   country = 'US';          United States macroeconomic data
#   sample_start  = datenum('2000-01-01','yyyy-mm-dd'); estimation sample
# -------------------------------------------------

load_data <- function(datafile, specs, sample) {
	stopifnot(stringr::str_detect(datafile, ".xls"))
	`%>%` <- dplyr::`%>%`
	
	dataxls <- 
	readxl::read_xls(datafile) %>% 
      dplyr::mutate_at(dplyr::vars(Date), lubridate::as_date)

	Z <- dplyr::select(dataxls, Date, dplyr::pull(spec@fields, "SeriesID")) 
	
	spec_transformation <-
	spec@fields %>% 
      dplyr::mutate(Step = dplyr::case_when(Frequency == "q" ~ 3, Frequency == "m" ~ 1)) %>% 
      dplyr::select(c("SeriesID", "Step", "Transformation")) %>% 
      as.list()
	
	X <- 
	purrr::pmap(spec_transformation, ~ get(..3)(dplyr::pull(Z, ..1), ..2)) %>% 
      setNames(spec_transformation$SeriesID) %>% 
      dplyr::bind_cols(Date = Z$Date, .)
	
	Z <- dplyr::filter(Z, Date >= sample_start)
	X <- dplyr::filter(X, Date >= sample_start)
	
	list("X" = X, "Z" = Z)
}

