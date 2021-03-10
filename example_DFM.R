# TODO: dependencies do not deal with mat and xlxs extensions
includes <- 
c(  "functions/load_spec.R"
	, "functions/load_data.R"
	, "functions/remNaNs_spline.R"
	, "functions/transformation_functions.R"
)

lapply(includes, function(.x) source(.x))

plotly_colors <- c('#1f77b4',  # muted blue
                   '#ff7f0e',  # safety orange
                   '#2ca02c',  # cooked asparagus green
                   '#d62728',  # brick red
                   '#9467bd',  # muted purple
                   '#8c564b',  # chestnut brown
                   '#e377c2',  # raspberry yogurt pink
                   '#7f7f7f',  # middle gray
                   '#bcbd22',  # curry yellow-green
                   '#17becf')  # blue-teal

threshold = 1e-4

datafile <- "data/US/2016-06-29.xls"
sample_start <- lubridate::as_date("2000-01-01")
spec <- load_spec("Spec_US_example.xls")

XZ <- load_data(datafile, spec, sample_start)

if (FALSE) {
	summaries <- lapply(XZ, function(.x) summary(.x))
	
	col_plot <- "INDPRO"
	plot1 <-
	ggplot2::ggplot(XZ$Z, ggplot2::aes(x = Date, y = !!dplyr::sym(col_plot))) +
		ggplot2::geom_line(color = plotly_colors[1], na.rm = TRUE) +
		theme_minimal()
	
	plot2 <-
	ggplot2::ggplot(XZ$X, ggplot2::aes(x = Date, y = !!dplyr::sym(col_plot))) +
		ggplot2::geom_line(color = plotly_colors[2], na.rm = TRUE) +
		theme_minimal()
	
	gridExtra::grid.arrange(grobs = list(plot1, plot2), nrow = 2)
}

# Doing: dfm