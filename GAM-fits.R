library(data.table)
library(wqtrends)
library(stringr)
library(dplyr)
library(lubridate)

param <- "TempW"
ma <- "Alligator Harbor Aquatic Preserve"

data <- as.data.frame(load_data_table(param, table="data"))

# Creating day-of-year column
data$SampleDate <- as.Date(data$SampleDate)
data$doy <- yday(data$SampleDate)

# renaming columns to match conventions from wqtrends package
# ManagedAreaName becomes "station" - may not necessary
data <- plyr::rename(data, c("SampleDate" = "date",
                             "ManagedAreaName" = "station",
                             "ParameterName" = "param",
                             "ResultValue" = "value",
                             "DecDate" = "cont_year",
                             "Year" = "yr",
                             "Month" = "mo"))

# Selecting relevant data for use in GAM modeling
data <- data %>% 
  filter(Use_In_Analysis==TRUE & station==ma) %>%
  select(date, station, param, value, doy, cont_year, yr, mo)

# create modeling object
mod <- anlz_gam(data, trans = "log10")

anlz_smooth(mod)
anlz_fit(mod)

ylab <- "Water Temperature (* C)"

show_prddoy(mod, ylab = ylab)

show_prdseries(mod, ylab = ylab)

show_prdseason(mod, ylab = ylab)

show_prd3d(mod, ylab = ylab)

anlz_perchg(mod, baseyr = c(1998, 1999, 2000), testyr = c(2008, 2009, 2010))

show_perchg(mod, baseyr = c(2005), testyr = c(2008), ylab = ylab)
pc <- show_perchg(mod, baseyr = c(2005), testyr = c(2008), ylab = ylab)


