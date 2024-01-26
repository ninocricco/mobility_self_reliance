#********************************************************
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: DOWNLOADING AND PROCESSING DATA
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#********************************************************

# Attributions: I drew the structure of this code from 
# code developed by Ian Lundberg (thanks Ian!), posted in 
# the Harvard dataverse here: 
# https://doi.org/10.7910/DVN/JERJ0C

# This function does the opposite of %in% - works as an inverse selector
'%!in%' <- function(x,y)!('%in%'(x,y))

# Loading libraries
library(tidyverse)
library(scales)
library(reshape2)
library(haven)
library(foreach)
library(mgcv)
library(qgam)
library(janitor)
library(gridExtra)

# Analyzing PSID data
# Data cart available on https://psidonline.isr.umich.edu/ 
# Register, then choose data-data center-previous carts, enter my email address
# Select
# Select cart 328628	-	Dissertation File 25.01.2024. Download the file.

# Download the data file, load in Stata, then save the .dta file to the wd as psid.dta

# Reading in the PSID data
main.psid <- read_dta("data/psid.dta") %>%
  mutate(familyid = ER30001, # 1968 interview number
         person_number = ER30002, # 1968 person number
         person = paste0(familyid,"_",person_number), # unique individual identifier
         sex = ER32000) %>%
  # Restrict to SRC sample
  filter(familyid <= 3000) %>%
  # Restrict to Sample Members (original sample individuals and their descendants)
  filter(person_number < 170)

# Adjust for inflation using the
# Consumer Price Index for All Urban Consumers
# https://data.bls.gov/timeseries/CUUR0000SA0
# from 1967 to 2019
cpi <- readxl::read_xlsx("data/cpi.xlsx",
                         skip = 11) %>%
  select(-HALF1,-HALF2) %>%
  melt(id = "Year") %>%
  group_by(Year) %>%
  summarize(cpi = mean(value)) %>%
  group_by() %>%
  rename(year = Year) %>%
  # Adjust to 2016 US dollars
  mutate(cpi_2016 = mean(ifelse(year == 2016, cpi, NA), na.rm = T),
         to_multiply = cpi_2016 / cpi) %>%
  select(year, to_multiply) %>%
  # Add 1 to year since the PSID data will be reported in year k for income earned in year k - 1
  mutate(year = year + 1)

# Code to turn PSID documentation into usable output
# This takes a string like "[68]ER30003 [69]ER30022"
# and returns those variables from the data in a clean form
clean <- function(string, data = main.psid, years.data = years, varName = varName) {
  # Separate the many variable names in the character string
  vector_of_variables <- strsplit(string, split = " ")[[1]]
  
  # Split each variable into its year and variable name
  df_years_varNames <- (foreach(x = vector_of_variables, .combine = "rbind") %do% {
    separated <- data.frame(t(strsplit(x, split = "]")[[1]]))
    # Convert the year to 
    return(separated)
  }) %>%
    transmute(year = as.character(X1),
              variable = as.character(X2)) %>%
    mutate(year = str_replace(year,"\\[0","200"),
           year = str_replace(year,"\\[1","201"),
           year = str_replace(year,"\\[2","202"),
           year = str_replace(year,"\\[","19"),
           year = as.numeric(year))
  
  # Produce a tidy data frame of this variable,
  # with rows identified by person and year
  df_this_variable <- main.psid %>%
    select(person,
           matches(paste(df_years_varNames$var,collapse="|"))) %>%
    melt(id = "person", value.name = varName,
         warn = F) %>%
    mutate(variable = as.character(variable)) %>%
    left_join(df_years_varNames, by = "variable") %>%
    select(-variable)
  return(df_this_variable)
}

unrestricted <- clean("[68]ER30004 [69]ER30023 [70]ER30046 [71]ER30070 [72]ER30094 [73]ER30120 [74]ER30141 [75]ER30163 [76]ER30191 [77]ER30220 [78]ER30249 [79]ER30286 [80]ER30316 [81]ER30346 [82]ER30376 [83]ER30402 [84]ER30432 [85]ER30466 [86]ER30501 [87]ER30538 [88]ER30573 [89]ER30609 [90]ER30645 [91]ER30692 [92]ER30736 [93]ER30809 [94]ER33104 [95]ER33204 [96]ER33304 [97]ER33404 [99]ER33504 [01]ER33604 [03]ER33704 [05]ER33804 [07]ER33904 [09]ER34004 [11]ER34104 [13]ER34204 [15]ER34305 [17]ER34504 [19]ER34704 [21]ER34904",
                      varName = "age",
                      years = c(1968:1997,seq(1999,2021,2))) %>% 
  left_join(
    clean("[68]V81 [69]V529 [70]V1514 [71]V2226 [72]V2852 [73]V3256 [74]V3676 [75]V4154 [76]V5029 [77]V5626 [78]V6173 [79]V6766 [80]V7412 [81]V8065 [82]V8689 [83]V9375 [84]V11022 [85]V12371 [86]V13623 [87]V14670 [88]V16144 [89]V17533 [90]V18875 [91]V20175 [92]V21481 [93]V23322 [94]ER4153 [95]ER6993 [96]ER9244 [97]ER12079 [99]ER16462 [01]ER20456 [03]ER24099 [05]ER28037 [07]ER41027 [09]ER46935 [11]ER52343 [13]ER58152 [15]ER65349 [17]ER71426 [19]ER77448 [21]ER81775",
          varName = "familyIncome"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[68]V74 [69]V514 [70]V1196 [71]V1897 [72]V2498 [73]V3051 [74]V3463 [75]V3863 [76]V5031 [77]V5627 [78]V6174 [79]V6767 [80]V7413 [81]V8066 [82]V8690 [83]V9376 [84]V11023 [85]V12372 [86]V13624 [87]V14671 [88]V16145 [89]V17534 [90]V18878 [91]V20178 [92]V21484 [93]V23323 [94]ER4140 [95]ER6980 [96]ER9231 [97]ER12080 [99]ER16463 [01]ER20443 [03]ER24116 [05]ER27931 [07]ER40921 [09]ER46829 [11]ER52237 [13]ER58038 [15]ER65216 [17]ER71293 [19]ER77315 [21]ER81642",
          varName = "headIncome"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[68]V75  [69]V516 [70]V1198 [71]V1899 [72]V2500 [73]V3053 [74]V3465 [75]V3865 [76]V4379 [77]V5289 [78]V5788 [79]V6398 [80]V6988 [81]V7580 [82]V8273 [83]V8881 [84]V10263 [85]V11404 [86]V12803 [87]V13905 [88]V14920 [89]V16420 [90]V17836 [91]V19136 [92]V20436 [93]V21807 [94]ER4144 [95]ER6984 [96]ER9235 [97]ER12082 [99]ER16465 [01]ER20447 [03]ER24135 [05]ER27943 [07]ER40933 [09]ER46841 [11]ER52249 [13]ER58050 [15]ER65244 [17]ER71321 [19]ER77343 [21]ER81670",
          varName = "spouseIncome"),
    by = c("person", "year")
  ) %>%
  
  left_join(
    clean("[68]V47 [69]V465 [70]V1138 [71]V1839 [72]V2439 [73]V3027 [74]V3423 [75]V3823 [76]NA [77]V5232 [78]V5731 [79]V6336 [80]V6934 [81]V7530 [82]V8228 [83]V8830 [84]V10037 [85]V11146 [86]V12545 [87]V13745 [88]V14835 [89]V16335 [90]V17744 [91]V19044 [92]V20344 [93]V21634 [94]ER4096 [95]ER6936 [96]ER9187 [97]ER12174 [99]ER16471 [01]ER20399 [03]ER24080 [05]ER27886 [07]ER40876 [09]ER46767 [11]ER52175 [13]ER57976 [15]ER65156 [17]ER71233 [19]ER77255 [21]ER81582",
          varName = "headWrkhrs"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[68]V53 [69]V475 [70]V1148 [71]V1849 [72]V2449 [73]V3035 [74]V3431 [75]V3831 [76]NA [77]V5244 [78]V5743 [79]V6348 [80]V6946 [81]V7540 [82]V8238 [83]V8840 [84]V10131 [85]V11258 [86]V12657 [87]V13809 [88]V14865 [89]V16365 [90]V17774 [91]V19074 [92]V20374 [93]V21670 [94]ER4107 [95]ER6947 [96]ER9198 [97]ER12185 [99]ER16482 [01]ER20410 [03]ER24091 [05]ER27897 [07]ER40887 [09]ER46788 [11]ER52196 [13]ER57997 [15]ER65177 [17]ER71254 [19]ER77276 [21]ER81603",
          varName = "spouseWrkhrs"),
    by = c("person", "year")
  ) %>%
  
  left_join(
    clean("[68]V117 [69]V1008 [70]V1239 [71]V1942 [72]V2542 [73]V3095 [74]V3508 [75]V3921 [76]V4436 [77]V5350 [78]V5850 [79]V6462 [80]V7067 [81]V7658 [82]V8352 [83]V8961 [84]V10419 [85]V11606 [86]V13011 [87]V14114 [88]V15130 [89]V16631 [90]V18049 [91]V19349 [92]V20651 [93]V22406 [94]ER2007 [95]ER5006 [96]ER7006 [97]ER10009 [99]ER13010 [01]ER17013 [03]ER21017 [05]ER25017 [07]ER36017 [09]ER42017 [11]ER47317 [13]ER53017 [15]ER60017 [17]ER66017 [19]ER34504 [21]ER78017",
          varName = "headAge"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[68]V118 [69]V1011 [70]V1241 [71]V1944 [72]V2544 [73]V3097 [74]V3510 [75]V3923 [76]V4438 [77]V5352 [78]V5852 [79]V6464 [80]V7069 [81]V7660 [82]V8354 [83]V8963 [84]V10421 [85]V11608 [86]V13013 [87]V14116 [88]V15132 [89]V16633 [90]V18051 [91]V19351 [92]V20653 [93]V22408 [94]ER2009 [95]ER5008 [96]ER7008 [97]ER10011 [99]ER13012 [01]ER17015 [03]ER21019 [05]ER25019 [07]ER36019 [09]ER42019 [11]ER47319 [13]ER53019 [15]ER60019 [17]ER66019 [19]ER34506 [21]ER78019",
          varName = "spouseAge"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[68]V313 [69]V794 [70]V1485 [71]V2197 [72]V2823 [73]V3241 [74]V3663 [75]V4093 [76]V4684 [77]V5608 [78]V6157 [79]V6754 [80]V7387 [81]V8039 [82]V8663 [83]V9349 [84]V10996 [85]V12400 [86]V13640 [87]V14687 [88]V16161 [89]V17545 [90]V18898 [91]V20198 [92]V21504 [93]V23333 [94]ER4158 [95]ER6998 [96]ER9249 [97]ER12222 [99]ER16516 [01]ER20457 [03]ER24148 [05]ER28047 [07]ER41037 [09]ER46981 [11]ER52405 [13]ER58223 [15]ER65459 [17]ER71538 [19]ER77599 [21]ER81926",
          varName = "headEd"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[72]V2687 [73]V3216 [74]V3638 [75]V4102 [76]V4695 [77]V5567 [78]V6116 [79]V6713 [80]V7346 [81]V7998 [82]V8622 [83]V9308 [84]V10955 [85]V12401 [86]V13641 [87]V14688 [88]V16162 [89]V17546 [90]V18899 [91]V20199 [92]V21505 [93]V23334 [94]ER4159 [95]ER6999 [96]ER9250 [97]ER12223 [99]ER16517 [01]ER20458 [03]ER24149 [05]ER28048 [07]ER41038 [09]ER46982 [11]ER52406 [13]ER58224 [15]ER65460 [17]ER71539 [19]ER77600 [21]ER81927",
          varName = "spouseEd",
    years = c(1972:1997,seq(1999,2017,2))),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[68]V239 [69]V607 [70]V1365 [71]V2072 [72]V2670 [73]V3181 [74]V3598 [75]V4053 [76]V4603 [77]V5650 [78]V6197 [79]V6790 [80]V7435 [81]V8087 [82]V8711 [83]V9419 [84]V11065 [85]V12426 [86]V13665 [87]V14712 [88]V16187 [89]V17565 [90]V18916 [91]V20216 [92]V21522 [93]V23336 [94]ER4159A [95]ER6999A [96]ER9250A [97]ER12223A [99]ER16423 [01]ER20369 [03]ER24150 [05]ER28049 [07]ER41039 [09]ER46983 [11]ER52407 [13]ER58225 [15]ER65461 [17]ER71540 [19]ER77601 [21]ER81928",
          varName = "marstat"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[68]ER30003 [69]ER30022 [70]ER30045 [71]ER30069 [72]ER30093 [73]ER30119 [74]ER30140 [75]ER30162 [76]ER30190 [77]ER30219 [78]ER30248 [79]ER30285 [80]ER30315 [81]ER30345 [82]ER30375 [83]ER30401 [84]ER30431 [85]ER30465 [86]ER30500 [87]ER30537 [88]ER30572 [89]ER30608 [90]ER30644 [91]ER30691 [92]ER30735 [93]ER30808 [94]ER33103 [95]ER33203 [96]ER33303 [97]ER33403 [99]ER33503 [01]ER33603 [03]ER33703 [05]ER33803 [07]ER33903 [09]ER34003 [11]ER34103 [13]ER34203 [15]ER34303 [17]ER34503 [19]ER34703 [21]ER34903",
          varName = "relhead"),
    by = c("person", "year")
  ) %>%
  left_join(
    clean("[83]ER30404 [84]ER30434 [85]ER30468 [86]ER30503 [87]ER30540 [88]ER30575 [89]ER30611 [90]ER30647 [91]ER30694 [92]ER30738 [93]ER30811 [94]ER33106 [95]ER33206 [96]ER33306 [97]ER33406 [99]ER33506 [01]ER33606 [03]ER33706 [05]ER33806 [07]ER33906 [09]ER34006 [11]ER34106 [13]ER34206 [15]ER34307 [17]ER34506 [19]ER34706 [21]ER34906",
          varName = "birth_year",
          years = c(1983:1997,seq(1999,2017,2))),
    by = c("person","year")
  ) %>% 
  left_join(main.psid %>% select(person, sex), by = "person")
