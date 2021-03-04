fit_MA <- function(X, k) {
	J = nrow(X)
	N = ncol(X)
	x = as.matrix(X)
	
	ones <- matrix(1, k, N)
	y <- dplyr::as_tibble(
		    rbind( ones * x[1:k, ]
					   , x
					   , ones * x[(J - k + 1):J, ]
					   )
		    )
	
	dplyr::mutate_all(y, ~ signal::filter(matrix(1, 2 * k + 1, 1) / (2 * k + 1), 1, .x )) %>% 
	   dplyr::filter(dplyr::row_number() >= 2 * k + 1)
} 

apply_option1 <- function(X, k) {
	`%>%` <- dplyr::`%>%`
	
	x <- dplyr::select(X, -1)
	
	x_clean <-
	dplyr::mutate_if( x, 
                  ~ any(is.na(.x)), 
                  ~ replace(.x, is.na(.x), median(.x, na.rm = TRUE))
									 ) 
	
	xMA <- fit_MA(x_clean, k) 
	
	dplyr::coalesce(x, xMA) %>% 
		dplyr::bind_cols(Dates = dplyr::select(X, 1), .)
}

filter_rows_by_threshold <- function(x, e = 0.8) {
	condition <- cumsum(which(rowSums(is.na(x)) / ncol(x) >= e))
	filtered_rows <- 
	purrr::pmap_dbl(.l = list(condition, 
														1:length(condition), 
														length(condition):1
														),
	                .f = ~ (..1 == ..2 | ..1 == ..3)
	)
	
	x[!filtered_rows, ]
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
 	x <- dplyr::select(X, -1)
 	
 	x_filtered <- filter_rows_by_threshold(x)

 	x_splined <-	
 	matlab_spline(x_filtered) %>% 
		dplyr::mutate_all(~ replace(.x, is.na(.x), median(.x, na.rm = TRUE)))
 	
 	xMA <- fit_MA(x_splined, k)
	
 	dplyr::coalesce(x, xMA)	%>% 
 		dplyr::bind_cols(Date = dplyr::select(X, 1), .)
}

x <- dplyr::select(XZ$X, -Date)

# apply_option3 <- function(X, k) {}
# apply_option4 <- function(X, k) {}