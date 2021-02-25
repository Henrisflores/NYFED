# load_data

# -------------------------------------------------
#  User inputs.
# vintage = '2016-06-29';  vintage dataset to use for estimation
# country = 'US';          United States macroeconomic data
# sample_start  = datenum('2000-01-01','yyyy-mm-dd'); estimation sample
# -------------------------------------------------

datafile <- "data/US/2016-06-29.xls"
[X,Time,Z] = load_data(datafile,Spec,sample_start);