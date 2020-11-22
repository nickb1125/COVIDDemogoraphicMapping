library(tidycensus)
library(acs)
library(dplyr)
library(leaflet)
library(rgdal)
library(sp)
library(sf)
library(dplyr)
library(ggplot2)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
library(SwimmeR)
library(lubridate)
library(maptools)
library(readr)
library(rgeos)


## Upload and Clean National County ACS Data

setwd("/Users/nickbachelder/Desktop/Duke Internship 2020")
my_codes <- read_csv(file = "acs_2014_codes_updated.csv")

County_ACS <- get_acs(
  geography = "county",
  variables = na.omit(my_codes$acs_code),
  survey = "acs5",
  year = 2018,
  output = "tidy",
) 

County_ACS <- County_ACS %>%
  select(-moe) %>%
  spread(variable, estimate)

data.frame(County_ACS)

# change from codes to labels
x <- names(County_ACS)
for (i in 1:length(names(County_ACS))) {
  if (x[i] %in% c("GEOID", "NAME", "geometry")) {
    x[i] <- x[i]
  } else {
    x[i] <- my_codes$my_vrb[my_codes$acs_code == x[i]]
  }
}
names(County_ACS) <- x

num_occupants_1.01_up_all <- County_ACS %>%
  select(num_occupants_1.01_1.5_owned, num_occupants_1.51_2_owned,
         num_occupants_2.1_up_owned, num_occupants_1.01_1.5_rented,
         num_occupants_1.51_2_rented, num_occupants_2.1_up_rented) %>%
  rowSums()
County_ACS <- County_ACS %>%
  mutate(num_occupants_crowded = num_occupants_1.01_up_all)

phone_service_rented_owned <- County_ACS %>%
  select(phone_service_owned, phone_service_rented) %>%
  rowSums()

County_ACS <- County_ACS %>%
  mutate(phone_service = phone_service_rented_owned)

County_ACS <- data.frame(County_ACS) %>% 
  mutate(population = male_total,
         male_prop = male/male_total, # sex covariate
         # race covariates
         white_prop = white / white_total,
         pop_non_hispanic_black_prop = pop_non_hispanic_black/pop_total,
         hispanic_prop = hispanic / hispanic_total,
         # age covariates
         # 
         age_under_5_prop = (male_under_5 + 
                               female_under_5) / (female_total + male_total),
         age_5_17_prop = (male_5_9 + 
                            male_10_14 + 
                            male_15_17 +
                            female_5_9 + 
                            female_10_14 + 
                            female_15_17) / (male_total + female_total),
         age_18_39_prop = (male_18_19 +
                             male_20 +
                             male_21 +
                             male_22_24 +
                             male_25_29 +
                             male_30_34 +
                             male_35_39 +
                             female_18_19 +
                             female_20 +
                             female_21 +
                             female_22_24 +
                             female_25_29 +
                             female_30_34 +
                             female_35_39) / (male_total + female_total),
         age_40_64_prop = (male_40_44 + 
                             male_45_49 + 
                             male_50_54 + 
                             male_55_59 + 
                             male_60_61 + 
                             male_62_64 + 
                             female_40_44 + 
                             female_45_49 + 
                             female_50_54 + 
                             female_55_59 + 
                             female_60_61 + 
                             female_62_64) / (male_total + female_total),
         age_over_65_prop = (male_65_66 + 
                               male_67_69 + 
                               male_70_74 + 
                               male_75_79 + 
                               male_80_84 + 
                               male_85_over + 
                               female_65_66 + 
                               female_67_69 + 
                               female_70_74 + 
                               female_75_79 + 
                               female_80_84 + 
                               female_85_over) / (male_total + female_total),
         # SES variables
         poverty_prop = below_poverty_line/poverty_total,
         family_type_female_householder_prop = family_type_female_householder/family_type_total,
         family_type_single_prop = family_type_single/family_type_total,
         # female management + professional
         fem_mgmt_prof_prop = (fem_occ_management + fem_occ_professional)/(fem_occ_total),
         # males management + professional
         male_mgmt_prof_prop = (males_occ_management + males_occ_professional + fem_occ_management + fem_occ_professional)/(males_occ_total),
         mgmt_prof_prop = (males_occ_management + males_occ_professional)/(males_occ_total),
         housing_rental_prop = housing_rental/housing_total,
         crowded_prop = num_occupants_crowded/num_occupants_total,
         over_16_unemployed_prop = over_16_unemployed/over_16_total,
         over_25_less_than_hs_grad_prop = over_25_less_than_hs_graduate/over_25_total,
         phone_service_prop = phone_service/phone_service_total,
         public_assistance_prop = public_assistance/public_assistance_total,
         vehicle_none_prop = vehicle_none/vehicle_total, 
         internet_prop = (broadband_comp + broadband_comp_dial) / broadband_comp_total) %>%
  select(GEOID, NAME, population, ends_with("prop")) %>%
  ungroup %>%
  filter(!is.na(NAME))

County_ACS$County <- sub("\\,.*", "", County_ACS$NAME)
County_ACS$State <- sub('.*, ', '', County_ACS$NAME)

data("fips_codes")
fips <- fips_codes
fips$fipscode <- as.numeric(paste(fips$state_code, fips$county_code, sep=""))
fips <- fips %>%
  mutate(StateAb = state,
         County = county) %>%
  select(StateAb, fipscode, County)
County_ACS <- County_ACS %>%
  select(-NAME)
County_ACS$State <- state.abb[match(County_ACS$State, state.name)]
County_ACS <- County_ACS %>%
  rename(StateAb = State)


County_ACS <- left_join(County_ACS, fips)

View(County_ACS) ## This is just like clean ACS file for model but at county level with fips codes

## Import Health Data By County

County_Health <- read.csv("County_Health_Data.csv")
County_Health <- County_Health %>%
  select(Adult.smoking.raw.value, Adult.obesity.raw.value, X..Rural.raw.value, Median.household.income.raw.value,
         Diabetes.prevalence.raw.value, Air.pollution...particulate.matter.raw.value, High.school.graduation.raw.value,
         Excessive.drinking.raw.value, X5.digit.FIPS.Code) %>%
  filter(!is.na(X5.digit.FIPS.Code))
County_Health <- County_Health[-c(1, 2), ]

names(County_Health) <- c('smoking_prop', 'obesity_prop', 'rural_prop', 'median_income', 'diabetes_prop',
                          'polution_prop', 'high_school_prop', 'alcoholic_prop', 'fipscode')

## Combine Health and ACS Data

setdiff(County_Health$fipscode, County_ACS$fipscode) ## Nothing significant (just state rows really)
setdiff(County_ACS$fipscode, County_Health$fipscode) ## A few, but not significant enough to worry about at all
County_Health$fipscode <- as.numeric(County_Health$fipscode)
County_ACS$fipscode <- as.numeric(County_ACS$fipscode)

County <- left_join(County_ACS, County_Health, by = 'fipscode')

## Upload county test data

County_Tests <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"))
County_Tests$date <- as.Date(County_Tests$date, "%Y-%m-%d") 



# Make time df


County_Tests_T <- County_Tests %>%
  dplyr::select(-county, -state, -deaths)
County_Tests_T <- County_Tests_T %>% 
  complete(date, nesting(fips))
County_Tests_T$cases <- as.numeric(County_Tests_T$cases)

date <- seq(as.Date(min(County_Tests_T$date)), as.Date(max(County_Tests_T$date)), by="days")
days <- 0:(length(date)-1)
daysmaker <- data.frame(cbind(date, days))
daysmaker$date <- as.Date(as.numeric(daysmaker$date), origin = "1970-01-01")
County_Tests_T <- left_join(County_Tests_T, daysmaker, by = 'date')


# Make cumulative df

County_Tests_C <- County_Tests %>%
  filter(!is.na(fips),
         date == max(date)) %>%
  select(-county, -state, -date)


## Check that all are reasonable (They are)
ggplot(data=County_Tests_T, aes(x= date, y = cases, group = fips, color = fips)) + geom_line()


## Combine with County
County_Tests_C <- County_Tests_C %>%
  rename(fipscode = fips)
County_Tests_T <- County_Tests_T %>%
  rename(fipscode = fips)

County_T <- left_join(County_Tests_T, County, by = 'fipscode')
County_C <- left_join(County_Tests_C, County, by = 'fipscode')

County_C <- County_C %>%
  mutate(case_per_oht = cases/(population / 100000))
County_T <- County_T %>%
  mutate(case_per_oht = cases/(population / 100000))


## Upload county shape files

library(rgdal)
shape <- readOGR('/Users/nickbachelder/Desktop/Duke Internship 2020/tl_2019_us_county', layer = 'tl_2019_us_county')

# Combine county and state fips code into one
shape@data$fipscode <- as.numeric(paste(shape@data$STATEFP, shape@data$COUNTYFP, sep =''))

#####Simplify shape data so it will run

shape_simp <- gSimplify(shape,tol=0.01, topologyPreserve=TRUE)
shape <- SpatialPolygonsDataFrame(shape_simp, data = shape@data, match.ID = F)

# Combine County Data (Time and Cumulative) into shape files

setdiff(County_C$fipscode, shape@data$fipscode)
setdiff(shape@data$fipscode, County_C$fipscode) # A few, no problem

shape_C <- merge(shape, County_C, by = "fipscode")
shape_C@data$GEOID.x <- as.numeric(shape_C@data$GEOID.x)
shape_C@data$GEOID.y <- as.numeric(shape_C@data$GEOID.y)
shape_C@data$smoking_prop <- as.numeric(shape_C@data$smoking_prop)
shape_C@data$obesity_prop <- as.numeric(shape_C@data$obesity_prop)
shape_C@data$rural_prop <- as.numeric(shape_C@data$rural_prop)
shape_C@data$median_income <- as.numeric(shape_C@data$median_income)
shape_C@data$diabetes_prop <- as.numeric(shape_C@data$diabetes_prop)
shape_C@data$polution_prop <- as.numeric(shape_C@data$polution_prop)
shape_C@data$high_school_prop <- as.numeric(shape_C@data$high_school_prop)
shape_C@data$alcoholic_prop <- as.numeric(shape_C@data$alcoholic_prop)

View(shape_C) # Final Cumulative df

setdiff(County_T$fipscode, shape@data$fipscode)
setdiff(shape@data$fipscode, County_T$fipscode) # A few, no problem


## Combine into shape time files individually for each date (do this in shiny)
county_T <- County_T %>%
  filter(days == 188)

shape_T <- sp::merge(shape, county_T,by = "fipscode")
shape_T@data$GEOID.x <- as.numeric(shape_T@data$GEOID.x)
shape_T@data$GEOID.y <- as.numeric(shape_T@data$GEOID.y)
shape_T@data$smoking_prop <- as.numeric(shape_T@data$smoking_prop)
shape_T@data$obesity_prop <- as.numeric(shape_T@data$obesity_prop)
shape_T@data$rural_prop <- as.numeric(shape_T@data$rural_prop)
shape_T@data$median_income <- as.numeric(shape_T@data$median_income)
shape_T@data$diabetes_prop <- as.numeric(shape_T@data$diabetes_prop)
shape_T@data$polution_prop <- as.numeric(shape_T@data$polution_prop)
shape_T@data$high_school_prop <- as.numeric(shape_T@data$high_school_prop)
shape_T@data$alcoholic_prop <- as.numeric(shape_T@data$alcoholic_prop)

View(shape_T) # Final time df



## Test Leaflet for Cumulative (works)
pal <- colorNumeric(
  palette = "Blues",
  domain = shape_C$rural_prop)
leaflet(shape_C) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = ~pal(rural_prop)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -74.0060, lat = 40.7128, zoom = 10)

## Test Leaflet for Time data

county_T <- County_T %>%
  filter(days == 188)
shape_T <- sp::merge(shape, county_T,by = "fipscode")
shape_T@data$GEOID.x <- as.numeric(shape_T@data$GEOID.x)
shape_T@data$GEOID.y <- as.numeric(shape_T@data$GEOID.y)
shape_T@data$smoking_prop <- as.numeric(shape_T@data$smoking_prop)
shape_T@data$obesity_prop <- as.numeric(shape_T@data$obesity_prop)
shape_T@data$rural_prop <- as.numeric(shape_T@data$rural_prop)
shape_T@data$median_income <- as.numeric(shape_T@data$median_income)
shape_T@data$diabetes_prop <- as.numeric(shape_T@data$diabetes_prop)
shape_T@data$polution_prop <- as.numeric(shape_T@data$polution_prop)
shape_T@data$high_school_prop <- as.numeric(shape_T@data$high_school_prop)
shape_T@data$alcoholic_prop <- as.numeric(shape_T@data$alcoholic_prop)


pal1 <- colorNumeric(
  palette = "Blues",
  domain = shape_T$case_per_oht)
leaflet(shape_T) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7,
              color = ~pal1(case_per_oht)) %>%
  addProviderTiles(providers$CartoDB.Positron)

## Combine state and county column in shape_C

shape_C$identity <- paste(paste(shape_C$NAMELSAD, ",", sep = ''), shape_C$StateAb)



## Create national dataframe for comparison

US_ACS <- get_acs(
  geography = "us",
  variables = na.omit(my_codes$acs_code),
  survey = "acs5",
  year = 2018,
  output = "tidy",
) 

US_ACS <- US_ACS %>%
  select(-moe) %>%
  spread(variable, estimate)

# change from codes to labels
x <- names(US_ACS)
for (i in 1:length(names(US_ACS))) {
  if (x[i] %in% c("GEOID", "NAME", "geometry")) {
    x[i] <- x[i]
  } else {
    x[i] <- my_codes$my_vrb[my_codes$acs_code == x[i]]
  }
}
names(US_ACS) <- x




num_occupants_1.01_up_all <- US_ACS %>%
  select(num_occupants_1.01_1.5_owned, num_occupants_1.51_2_owned,
         num_occupants_2.1_up_owned, num_occupants_1.01_1.5_rented,
         num_occupants_1.51_2_rented, num_occupants_2.1_up_rented) %>%
  rowSums()
US_ACS <- US_ACS %>%
  mutate(num_occupants_crowded = num_occupants_1.01_up_all)

phone_service_rented_owned <- US_ACS %>%
  select(phone_service_owned, phone_service_rented) %>%
  rowSums()

US_ACS <- US_ACS %>%
  mutate(phone_service = phone_service_rented_owned)

US_ACS <- data.frame(US_ACS) %>% 
  mutate(population = male_total,
         male_prop = male/male_total, # sex covariate
         # race covariates
         white_prop = white / white_total,
         pop_non_hispanic_black_prop = pop_non_hispanic_black/pop_total,
         hispanic_prop = hispanic / hispanic_total,
         # age covariates
         # 
         age_under_5_prop = (male_under_5 + 
                               female_under_5) / (female_total + male_total),
         age_5_17_prop = (male_5_9 + 
                            male_10_14 + 
                            male_15_17 +
                            female_5_9 + 
                            female_10_14 + 
                            female_15_17) / (male_total + female_total),
         age_18_39_prop = (male_18_19 +
                             male_20 +
                             male_21 +
                             male_22_24 +
                             male_25_29 +
                             male_30_34 +
                             male_35_39 +
                             female_18_19 +
                             female_20 +
                             female_21 +
                             female_22_24 +
                             female_25_29 +
                             female_30_34 +
                             female_35_39) / (male_total + female_total),
         age_40_64_prop = (male_40_44 + 
                             male_45_49 + 
                             male_50_54 + 
                             male_55_59 + 
                             male_60_61 + 
                             male_62_64 + 
                             female_40_44 + 
                             female_45_49 + 
                             female_50_54 + 
                             female_55_59 + 
                             female_60_61 + 
                             female_62_64) / (male_total + female_total),
         age_over_65_prop = (male_65_66 + 
                               male_67_69 + 
                               male_70_74 + 
                               male_75_79 + 
                               male_80_84 + 
                               male_85_over + 
                               female_65_66 + 
                               female_67_69 + 
                               female_70_74 + 
                               female_75_79 + 
                               female_80_84 + 
                               female_85_over) / (male_total + female_total),
         # SES variables
         poverty_prop = below_poverty_line/poverty_total,
         family_type_female_householder_prop = family_type_female_householder/family_type_total,
         family_type_single_prop = family_type_single/family_type_total,
         # female management + professional
         fem_mgmt_prof_prop = (fem_occ_management + fem_occ_professional)/(fem_occ_total),
         # males management + professional
         male_mgmt_prof_prop = (males_occ_management + males_occ_professional + fem_occ_management + fem_occ_professional)/(males_occ_total),
         mgmt_prof_prop = (males_occ_management + males_occ_professional)/(males_occ_total),
         housing_rental_prop = housing_rental/housing_total,
         crowded_prop = num_occupants_crowded/num_occupants_total,
         over_16_unemployed_prop = over_16_unemployed/over_16_total,
         over_25_less_than_hs_grad_prop = over_25_less_than_hs_graduate/over_25_total,
         phone_service_prop = phone_service/phone_service_total,
         public_assistance_prop = public_assistance/public_assistance_total,
         vehicle_none_prop = vehicle_none/vehicle_total, 
         internet_prop = (broadband_comp + broadband_comp_dial) / broadband_comp_total) %>%
  select(GEOID, NAME, population, ends_with("prop")) %>%
  ungroup %>%
  filter(!is.na(NAME))

View(US_ACS) ## This is just like clean ACS file for model but at county level with fips codes

## Hand enter health data (All of this data is from the CDC and Cencus and EPA data tables, just scattered, so easy to enter by hand)
## (I will cite these in the application, along with other data sources) (updated for september 14 2020)

US_Health <- US_ACS
US_Health$smoking_prop <- 0.137
US_Health$obesity_prop <- 0.424
US_Health$rural_prop <- 0.193
US_Health$diabetes_prop <- 0.093
US_Health$polution_prop <- 8.21
US_Health$high_school_prop <- 0.9
US_Health$alcoholic_prop <- 0.16
shape_C$over_25_hs_grad_prop <- 1 - shape_C$over_25_less_than_hs_grad_prop
County_T$over_25_hs_grad_prop <- 1 - County_T$over_25_less_than_hs_grad_prop


## also import total US cases from github NY Times respitory to make case_per_oht for US at national level

US_tests <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/live/us.csv"))
US_tests <- US_tests %>%
  dplyr::select(date, cases) %>%
  mutate(case_per_oht = cases/330302123*100000)

setwd('/Users/nickbachelder/Desktop/Duke Internship 2020/Shiny Upload Files')
writeOGR(obj=shape, dsn="shape", layer="shape1", driver="ESRI Shapefile") 
write.csv(County_C,"/Users/nickbachelder/Desktop/Duke Internship 2020/Shiny Upload Files/County_C.csv", row.names = FALSE)
write.csv(US_Health,"/Users/nickbachelder/Desktop/Duke Internship 2020/Shiny Upload Files/US_Health.csv", row.names = FALSE)
write.csv(County_T,"/Users/nickbachelder/Desktop/Duke Internship 2020/Shiny Upload Files/County_T.csv", row.names = FALSE)
write.csv(US_tests,"/Users/nickbachelder/Desktop/Duke Internship 2020/Shiny Upload Files/US_tests", row.names = FALSE)


