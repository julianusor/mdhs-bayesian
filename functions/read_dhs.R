read_dhs <- function(path, n_max) {
  
  # Read data
  data <- read_dta(path, n_max = n_max)
  
  # Obtain the format sibling-per-row for the columns mm1, mm2, mm4, etc...
  data_siblings <-
    mortDHS_reshape(data = data, sib_cols = c(1, 2, 4, 7, 8, 9, 11, 16))
  
  # Add new columns
  columns_to_add <- c("caseid", "v008")
  data_siblings <- data_siblings %>% left_join(data[, columns_to_add])
  
  # Delete original dataframe
  rm(data)
  
  # Separate caseid into three columns
  data_siblings$caseid <-
    gsub("( )+(?!\\d)", "", data_siblings$caseid, perl = T)
  data_siblings$caseid <- str_sub(data_siblings$caseid, 2, -1)
  data_siblings <-
    separate(
      data = data_siblings,
      col = caseid,
      into = c("cluster", "household", "person"),
      sep = " "
    )
  
  # Add the number of siblings to each row (if a person has two siblings the two rows will have the column n_siblings = 2)
  data_siblings <-
    data_siblings %>% group_by(cluster, household, person) %>% mutate(n_siblings = n())
  
  # Make distinction by columns mm1, mm2, ...
  data_siblings <-
    data_siblings %>% group_by(cluster, household) %>% distinct(mm1, mm2, mm4, mm8, mm9, mm16, n_siblings, v008)
  
  # Remove the column number of siblings
  data_siblings <- select(data_siblings, -n_siblings)
  
  # Rename the columns
  data_siblings <- data_siblings %>% rename_with(
    .col = c("mm1", "mm2", "mm4", "mm8", "mm9", "v008"),
    ~ c(
      "sex",
      "survival_status",
      "birth_cmc",
      "death_cmc",
      "pregnant_when_died",
      "interview_cmc"
    )
  )
  
  data_siblings
}