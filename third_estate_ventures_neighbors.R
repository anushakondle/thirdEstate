## Function used to convert the Total Assessed Value column 
## from BID_grey.xlsx into a numeric value for easy processing downstream

get_money <- function(value){
  if(!is.na(value)){
    value <- stringr::str_split(value,'\\$')
    value <- value[[1]][2]
    value <- stringr::str_replace_all(value,',','') %>% 
      as.numeric()
    return(value)
  }else{
    return(as.numeric(NA))
  }
}


## Required libraries
library(tidyverse)

## Load pre-selected properties  (these are already selected 434 properties).
## We need to extend this grey area
BID_grey <- readxl::read_xlsx('BID grey.xlsx')


## Transform the total assesed value column into a workable integer type
aa <-  BID_grey %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(value = get_money(`Total Assessed Value`)) %>% 
  dplyr::ungroup()


## Current assessment roll for 2022-2023.
## Subset to those properties in required neighborhood(s)
all_value <- read.csv('Current_Assessment_Roll__2022-2023_.csv') %>% 
  dplyr::select(
    # SBL,
                Tax.District,
                Prop.Class.Description,
                Previous.Property.Class,
                House.Number,
                Street,
                Address,
                Zipcode,
                Zipcode.Extension,
                Council.District,
                Police.District,
                Neighborhood,
                Latitude,
                Longitude,
                Location, 
                Print.Key,
                Total.Value) %>% 
  dplyr::filter(Neighborhood %in% c('Allentown','Elmwood Bryant','Elmwood Bidwell')) %>%  # subset to required neighborhoods
  unique() %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(value = stringr::str_replace_all(`Total.Value`,',','') %>% 
                as.numeric()) %>% 
  dplyr::ungroup()


## All properties that are on the streets found in aa(pre selected grey area)
all_street <- all_value %>% 
  dplyr::filter(Street %in% aa$Street)

## All properties not in current list of selected properties (BID_grey)
all_new <- all_street %>% 
  dplyr::anti_join(aa %>% 
                     dplyr::select(Street, 
                                   House.Number = `House Number`))

## Properties that can have neighbours
can_neigh <- aa %>% 
  dplyr::filter(Street %in% all_new$Street)


## Get min, max house numbers that can be neighbors
## This is to get a proxy estimate on how many houses
## we can get to include as neighbors
filters_street <- can_neigh %>% 
  dplyr::group_by(Street) %>% 
  dplyr::summarise(min_val = min(`House Number`),
                   max_val = max(`House Number`)) %>% 
  dplyr::ungroup()


## Average house value (because we need to incl ~$240 Million from <= ~430 houses)
poss_neigh <- all_new %>% 
  dplyr::group_by(Street) %>% 
  dplyr::summarise(val = mean(`value`)) %>% 
  dplyr::ungroup()


## Look at all possible neighbors in a Street, i,e, sub group 
sub_grp <- all_new %>% 
  dplyr::filter(Street == 'NORTH ST') %>% 
  dplyr::arrange(`House.Number`)
