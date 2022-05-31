library(haven)
library(tidyverse)

# Diccionario de variables 
# https://dhsprogram.com/pubs/pdf/DHSG4/Recode7_DHS_10Sep2018_DHSG4.pdf

# Individual Women's Data - Individual Recode (IR)
# This dataset has one record for every eligible woman as defined by the household schedule. 

data <- read_dta("data/mwi_data.dta", n_max = 1000)

# Este codigo sirve para seleccionar todas las filas pero solo
# cierta lista de columnas
data <- read_dta("data/mwi_data.dta", col_select = c("v001", "v002", "v003", "v004"))

# v001
# Cluster number is the number identifying the sample point 
# as used during the fieldwork.
# Hay 850 clusters y en cada cluster hay cierto numero de casas

# v002 
# Household number is the number identifying the household 
# within the cluster or sample point.
# Hay 

# v003
# Respondent's line number is the line number in the household 
# schedule of the person responding to the questions asked 
# in the household questionnaire. If nobody in the household
# was available for interview, this variable is coded 00.

# ---------------------- 1.
data %>% filter((v001 == 2) & (v002 == 2)) %>% View("b")

data %>% filter((v005 == 972409)) %>% View()

data %>% filter(str_detect(caseid, " 104 ")) %>% select(caseid)

data %>% select(caseid, v001, v002, v003, v011, v012) %>% View()

# ---------------------- 2.


data2 <- data %>%
  filter((v001 == 6) & (v002 == 70)) %>%
  pivot_longer(
    cols = matches("(mm(1|2|4|7|8|9)_0(1|2|3|4|5|6|7|8|9))"),
    names_to = c(".value", "sibling_no"),
    names_pattern = "(mm\\d+)_0(.)"
  ) %>%
  select(matches("(mm\\d+$)|sibling_no|caseid")) %>%
  rename_with(
    .col = c("mm1", "mm2", "mm4", "mm7", "mm8", "mm9"),
    ~ c(
      "sex",
      "survival_status",
      "birth_cmc",
      "age_at_death",
      "death_cmc",
      "pregnant_when_died"
    )
  ) %>% 
  filter (survival_status == 0)

data2 %>% View()

