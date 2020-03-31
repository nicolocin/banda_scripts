# FUNCTIONS TO REVERSE SCORE
require(dplyr)
require(reshape2)
require(tidyverse)
require(REDCapR)
require(psych)
require(stringr)

#source('banda_scoring_info.R')


################################################################################
# SELECT RAW DATA
################################################################################
# Input: Raw item-level data with or w/o label
# Output: Raw item-level data for one assessment for one session
get_raw_data <- function(dat, var_name, session='i'){
  
  print(paste('Pulling', toupper(var_name), 'from session', session,'...'))
  pat <- paste0(var_name, ".*[0-9]+[a-z]?_", session)
    
  select(dat, matches(pat)) %>% select_if(is.numeric)
}




################################################################################
# RECODE ITEMS
################################################################################
# SHAPS & Chapman Handedness 
# Input: get_raw_master() output; raw item-level data for one assessment for one session
# Output: 

# Helper functions
recode_master <- list(handed = function(x) {case_when(x == 1 ~ 3L,
                                       x == 2 ~ 1L,
                                       x == 3 ~ 2L)},
                    shaps = function(x) {case_when(x <= 1 ~ 1,
                                      x >= 2 ~ 0)},
                    rmbi = function(x) replace(x, x==3, NA)) # Recode for dropbox data, don't use for NDA

recode_nda <- list(handed = function(x) {case_when(x == 1 ~ 3L,
                                                      x == 2 ~ 1L,
                                                      x == 3 ~ 2L)},
                      shaps = function(x) {case_when(x <= 1 ~ 1,
                                                     x >= 2 ~ 0)},
                      fhs = function(x) replace(x, x==9, " ")) 



recode_items <- function(rawdata, var_name, session='i', all_recode=recode_master){
  
  # Chapman Handedness Scale
  if(var_name %in% c('handed', 'shaps', 'rmbi')) {
    
    print(paste('Recoding', toupper(var_name), 'for session', session,'...'))
    recode_dat <- mutate_all(rawdata, all_recode[[var_name]])
    return(recode_dat)} 
  
  # Does not need to be recoded
  return(rawdata)
}


################################################################################
# REVERSE SCORING
################################################################################
# Input: Raw item-level data for one assessment for one session
# Output: Reverse-scored item-level data for one assessment for one session
rev_score <- function(dat, var_name, session='i', rev_items=rev_master,
                      mins=item_mins, maxes=item_maxes){
  

  if(!(var_name %in% names(rev_master))) { 
    print(paste(toupper(var_name), 'does not need to reverse scored'))
    return(dat)}
  
  print(paste('Reverse scoring', toupper(var_name), 'for session', session,'...'))
  
  rev_items <- rev_master[[var_name]]                        # Get items to be reverse-scored
  q_no <- as.numeric(str_extract(names(dat), '\\d+'))        # Get question number from item 
  rev_keys <- rep(1, ncol(dat))                              # Generate key for reverse.code
  rev_keys[q_no %in% rev_items] <- -1
  
  
  var_min = mins[var_name]                          # Get min and max of recoded subscale
  var_max = maxes[var_name]
  
  reverse.code(rev_keys, dat, var_min, var_max)
}


################################################################################
# PUTTING EVERYTHING TOGETHER
################################################################################
# Input: Raw data directly from REDCap
# Output: Reverse-scored and recoded item-level data for one assessment for one session only

rev_items_by_ssn <- function(dat, var_name, session='i',all_rev=rev_master,
                             mins=item_mins, maxes=item_maxes, all_recode=recode_master){
  
  dat %>% 
    get_raw_data(var_name, session) %>%
    recode_items(var_name, session, all_recode) %>%
    rev_score(var_name, session, all_rev, mins, maxes) %>%
    as_tibble %>% return()
}


# Input: Raw data directly from REDCap
# Output: Reverse-scored item-level data for one assessment for all 3 sessions
rev_items_all <- function(dat, var_name, sessions=c('i', '1', '2'), all_rev=rev_master,
                             mins=item_mins, maxes=item_maxes, all_recode=recode_master){
  
  sids <- if('subject_id_i' %in% names(dat)){dat$subject_id_i} else {dat$subject_id}
  df <- data.frame(subject_id = sids)
  
  for(ses in sessions){
    df <- bind_cols(df, rev_items_by_ssn(dat, var_name, session=ses))}
  
  return(df)
}


# (2) ALL REDCap BEHAVIORAL REVERSE-SCORED ITEMS for one session
# Input: Raw data directly from REDCap
# Output: Reverse-scored item-level data for ALL assessment for one session


all_rev_items_by_ssn <- function(rawdata, session='i', ado=T, all_rev=rev_master,
                                 mins=item_mins, maxes=item_maxes, all_recode=recode_master){

  sids <- if('subject_id_i' %in% names(rawdata)){rawdata$subject_id_i} else {rawdata$subject_id} 
  var_names <- if(ado) {ado_scales} else {pr_scales}
    
  revdata <- sapply(var_names, rev_items_by_ssn, dat=rawdata, session=session, 
                     all_rev=all_rev, mins=mins, maxes=maxes, all_recode=all_recode) %>%
    bind_cols(data.frame(subject_id = sids), .) %>%
    as_tibble
  
  return(revdata)
  }


