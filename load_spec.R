raw <- readxl::read_xls("Spec_US_example.xls")

field_names <- 
c("SeriesID", "SeriesName", "Frequency", "Units", "Transformation", "Category")

frequency <-
c("d", "w", "m", "q", "sa", "a")

Spec <- setClass("Spec", slots = list(fields = "data.frame", 
																			blocks = "data.frame", 
																			units  = "character"))

Fields <-
raw %>%
  dplyr::filter(Model != 0) %>%
	dplyr::select(tidyselect::all_of(field_names)) %>% 
	mutate(Numeric_Frequency = case_when(
                                    		Frequency == "d"  ~ 1,
                                    		Frequency == "w"  ~ 7,
                                    		Frequency == "m"  ~ 30,
                                    		Frequency == "q"  ~ 3 * 30,
                                    		Frequency == "sa" ~ 6 * 30,
                                    		Frequency == "a"  ~ 12 * 30
	)) %>% 
	arrange(desc(Numeric_Frequency))

UnitsTransformed <-
case_when(
	Fields$Transformation == "lin" ~ "Levels (No Transformation)"	,
	Fields$Transformation == "chg" ~ "Change (Difference)",	
	Fields$Transformation == "ch1" ~ "Year over Year Change (Difference)",
	Fields$Transformation == "pch" ~ "Percent Change",
	Fields$Transformation == "pc1" ~ "Year over Year Percent Change",
	Fields$Transformation == "pca" ~ "Percent Change (Annual Rate)",
	Fields$Transformation == "cch" ~ "Continuously Compunded Rate of Change",
	Fields$Transformation == "cca" ~ "Continuously Compounded Annual Rate of Change",
	Fields$Transformation == "log" ~ "Natural Log"
)

Blocks <-
raw[,grep("Block", colnames(raw))] %>%
	mutate_if(~ any(is.na(.x)), ~ replace(.x, is.na, 0)) %>% 
	rename_all(~ str_replace(colnames(Blocks), "Block", ""))

spec <- Spec(fields = Fields, blocks = Blocks, units = UnitsTransformed)


