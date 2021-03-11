# remNaNs_spline 

# -------------------------------------------------
#
# This module is a direct translation of the matlab counterpart.
# 
# -------------------------------------------------

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

filter_rows_by_threshold <- function(x, e = 0.8) {
  condition <- cumsum(rowSums(is.na(x)) / ncol(x) >= e)
  filtered_rows <- 
  purrr::pmap_lgl(.l = list(condition,  1:length(condition),  length(condition):1),
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

apply_option1 <- function(x, k) {
  `%>%` <- dplyr::`%>%`

  x_clean <-
  dplyr::mutate_if( x, 
                  ~ any(is.na(.x)), 
                  ~ replace(.x, is.na(.x), median(.x, na.rm = TRUE))
  ) 

  xMA <- fit_MA(x_clean, k) 
  x_final <- dplyr::coalesce(x, xMA) 

  list("X" = x_final, "indNaN" = tidyr::as_tibble(is.na(x)))
}

apply_option2 <- function(x, k) {
  `%>%` <- dplyr::`%>%`
  
  x_filtered <- filter_rows_by_threshold(x, 0.8)
  
  x_splined <-	
  matlab_spline(x_filtered) %>% 
  dplyr::mutate_all(~ replace(.x, is.na(.x), median(.x, na.rm = TRUE)))
  
  xMA <- fit_MA(x_splined, k)
  x_final <- dplyr::coalesce(x_filtered, xMA)	
  
  list("X" = x_final, "indNaN" = tidyr::as_tibble(is.na(x_filtered)))
}


apply_option3 <- function(x) {
  filter_rows_by_threshold(x, 1)
}

apply_option4 <- function(x, k) {
  `%>%` <- dplyr::`%>%`
  
  x_filtered <- filter_rows_by_threshold(x, 1)
  
  x_splined <-	
  matlab_spline(x_filtered) %>% 
    dplyr::mutate_all(~ replace(.x, is.na(.x), median(.x, na.rm = TRUE)))
  
  xMA <- fit_MA(x_splined, k)
  
  x_final <- dplyr::coalesce(x_filtered, xMA)
  
  list("X" = x_final, "indNaN" = tidyr::as_tibble(is.na(x_filtered)))
}

apply_option5 <- function(x, k) {
  `%>%` <- dplyr::`%>%`
  
  x_splined <- 
  matlab_spline(x) %>% 
  dplyr::mutate_all( ~ replace(.x, is.na(.x), median(.x, na.rm = TRUE)))
  
  xMA <- fit_MA(x_splined, k)
  
  x_final <- dplyr::coalesce(x, xMA)	
  
  list("X" = x_final, "indNaN" = tidyr::as_tibble(is.na(x)))
}

remNaNs_spline <- function(M, k, method) {
  cols_class <- vapply(M, class, "", USE.NAMES = FALSE)
  dates <- vector(mode = "numeric", length = nrow(M))
  if (cols_class[1] == "Date") {
    m <- dplyr::select(M, -1)
    dates <- dplyr::select(M, 1)
  } else {
    m <- M
  }

  mNan <-	
  switch (method, 
    apply_option1(m, k),
    apply_option2(m, k),
    apply_option3(m),
    apply_option4(m, k),
    apply_option5(m, k)
  )	

  if (cols_class[1] == "Date") {
    colname <- colnames(M)[1]
    mNan[[1]] <- dplyr::bind_cols(colname := dates, mNan[[1]])
  } 

  mNan	
}
