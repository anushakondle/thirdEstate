##
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

library(tidyverse)

BID_grey <- readxl::read_xlsx('BID grey.xlsx')

aa <-  BID_grey %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(value = get_money(`Total Assessed Value`)) %>% 
  dplyr::ungroup()

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


## All properties that are on the streets found in aa
all_street <- all_value %>% 
  dplyr::filter(Street %in% aa$Street)

## All properties not in current list
all_new <- all_street %>% 
  dplyr::anti_join(aa %>% 
                     dplyr::select(Street, 
                                   House.Number = `House Number`))

## Properties that can have neighbours
can_neigh <- aa %>% 
  dplyr::filter(Street %in% all_new$Street)


## filters 
filters_street <- can_neigh %>% 
  dplyr::group_by(Street) %>% 
  dplyr::summarise(min_val = min(`House Number`),
                   max_val = max(`House Number`)) %>% 
  dplyr::ungroup()


## possible nearest neighbours
poss_neigh <- all_new %>% 
  dplyr::group_by(Street) %>% 
  dplyr::summarise(val = mean(`value`)) %>% 
  dplyr::ungroup()


## sub group 
sub_grp <- all_new %>% 
  dplyr::filter(Street == 'NORTH ST') %>% 
  dplyr::arrange(`House.Number`)
