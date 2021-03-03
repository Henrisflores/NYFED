apply_option1 <- function(X, k) {
	`%>%` <- dplyr::`%>%`
	
	x <- dplyr::select(X, -Date)
	
	x_clean <-
	dplyr::mutate_if( x, 
									 ~ any(is.na(.x)), 
									 ~ replace(.x, is.na(.x), median(.x, na.rm = TRUE))
									 ) %>% 
		as.matrix()
	
	# TODO: Extract this filter structure to a separate function
	ones    <- matrix(1, k, ncol(x_clean))
	x_clean <- dplyr::as_tibble(rbind(ones * x_clean[1:k, ], x_clean[1:k, ], ones * x_clean[]))

	xMA <- 
		dplyr::mutate_all(x_clean, ~ signal::filter(matrix(1, 2 * k + 1, 1) / (2 * k + 1), 1, .x )) %>% 
		dplyr::filter(dplyr::row_number() >= 2 * k + 1) %>% 
		dplyr::bind_cols(Date = dplyr::select(X, Date), .)
}


x <- dplyr::select(XZ$X, -Date)

filter_rows_by_threshold <- function(x, e = 0.8) {
	row_condition <- which(rowSums(is.na(x)) / ncol(x) > 0.8)
	if (length(row_condition) != 1) {
		row_condition <- c(min(row_condition), max(row_condition))
	}
	
	dplyr::filter(x, !dplyr::row_number() %in% row_condition)
}

matlab_spline <- function(x) {
	`%>%` <- dplyr::`%>%`
	lapply(x, function(.x) {
		          clean_indices <- which(!is.na(.x))
		          interval <- seq(min(clean_indices), max(clean_indices))
		          
		          y <-
		          pracma::interp1( clean_indices
		          							 , .x[clean_indices]
		          							 , interval 
		          							 , "spline"
		          						   )
		          
		          replace(.x, interval, y)
	}) %>% 
		dplyr::bind_cols()
}

apply_option2 <- function(X, k) {
	`%>%` <- dplyr::`%>%`
 	x <- dplyr::select(X, -Date)
 	
 	x_filtered <- filter_rows_by_threshold(x)

 	x_splined <-	
 	matlab_spline(x_filtered) %>% 
		dplyr::mutate_all(~ replace(.x, is.na(.x), median(.x, na.rm = TRUE)))
 	
	ones    <- matrix(1, k, ncol(x_splined))
	x_ones  <- dplyr::as_tibble(rbind(ones, as.matrix(x_splined), ones))

	dplyr::mutate_all(x_ones, ~ signal::filter(matrix(1, 2 * k + 1, 1) / (2 * k + 1), 1, .x )) %>% 
	dplyr::filter(dplyr::row_number() >= 2 * k + 1) #%>%  dplyr::bind_cols(Date = dplyr::select(X, Date), .)
}
