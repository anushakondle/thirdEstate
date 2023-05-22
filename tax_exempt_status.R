##### 
# Get all required addresses from the current Assessment Roll (2022-2023)
# https://data.buffalony.gov/Government/Current-Assessment-Roll-2022-2023-/4t8s-9yih
#####

## Required libraries
library(tidyverse)
library(glue)
## Load Current Assessment Roll
assessment_roll <- read.csv('Current_Assessment_Roll__2022-2023_.csv', header = T)

## From the 88 columns in the document, we only need ones relating to address, SBL and property class
assessment_subset <- assessment_roll %>% 
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
                Print.Key) %>% 
  dplyr::filter(Neighborhood %in% c('Allentown','Elmwood Bryant','Elmwood Bidwell')) %>%  # subset to required neighborhoods
  unique()

######################
####
## NOTE:
#        We know the identifiers like SBL, Print.Key numbers, address etc, for a given property/parcel of land from the assessment roll
#        BUT we do not have the mapping from these identifiers in the assessment roll to the P_ID identifier used in the database on the
#        paytax.erie.gov website. for eg, the property with SBL# {140200.111.250-2-1.000} has the P_ID {242750} and you can access the tax information page here at
#        {https://paytax.erie.gov/(S(wpi0rn5czb1fu22fdzkcribt))/Report/Web_Report_CrystalViewer.aspx?REPORTNAME=C:\eGov\Reports\ERI_WEB_TX.rpt&ReportParameter=YEAR_ID=2023;P_ID=242750;CURRENT_YEAR_ID=2023}.
#        As you can see the P_ID used in the webpage has no link to the SBL/prinkey etc.,

## POSSIBLE SOLUTIONS:
#        1. Get a mapping from SBL (or any other identifier from the assessment roll) to the P_ID
#        2. Do a exhaustive search for all possible P_IDs in a range. This can be very time consuming as is extremely inefficient if only done for 
#           three neighbourhoods (around 8000 parcels of land and the P_IDs are in six digits); But might be useful if we need the tax status of 
#           many or all neighbourhoods
####
######################

########### Webpage scrape using rvest 
library(rvest)
library(dplyr)

## Get if a property is tax exempt or not
get_tax_exempt_status  <- function(P_ID = NULL,
                        Report_version_YEAR = '2023',
                        Tax_YEAR = '2023'){
  if(is.null(P_ID)){
    return('give a P_ID')
  }else{
    url <- glue("https://paytax.erie.gov/(S(a4xfdvkw0othlmsf2z4eihu0))/Report/Web_Report_CrystalViewer.aspx?REPORTNAME=C:%5CeGov%5CReports%5CERI_WEB_TX.rpt&ReportParameter=YEAR_ID={Report_version_YEAR};P_ID={P_ID};CURRENT_YEAR_ID={Tax_YEAR}")
    webpage <- read_html(url)
    page_body <- html_nodes(webpage, 'body') %>% 
      html_text()
    
    # A valid P_ID will have a owner listed for the property
    # An invalid P_ID (for eg., 100000) will not have any owner listed and will be a blank page
    pid_valid <- grepl('Owner', page_body)
    if(pid_valid){
      aa <- str_split(page_body, "EXEMPTION")
      ## We want the first one
      aa <- aa[[1]][1]
      ab <- str_split(aa, '&nbsp;<')
      len_a <- length(ab[[1]])
      ab <- ab[[1]][len_a-1]
      ac <- str_split(ab, '>&nbsp')
      len_ac <- length(ac[[1]])
      monies <- ac[[1]][len_ac]
      
      if(stringr::str_length(monies) > 20){
        # this is more than trillion dollars in tax exemption
        # return('0') # not exempt at all - i.e 0 in exemption
        return('no')
      }
      
      ## Get amount of tax owed if there is only one exemption type
      # monies <- monies %>% 
      #   stringr::str_replace_all(.,';','') %>% 
      #   stringr::str_replace_all(.,',','')
      # return(monies)
      
      return('yes')
      
    }else{
      return("Could'nt find Owner in page, might be invalid P_ID")
    }
  }
}

get_tax_exempt_status('10000') # invalid P_ID
get_tax_exempt_status('242755') # no exemptions
get_tax_exempt_status('242751') # yes, has exemptions
