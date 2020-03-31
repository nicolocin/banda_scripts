
################################################################################
#source('redcap_data_cleaning.R')
# STEP 1 - Set working directory (the place you want to save your file)
# Use setwd() or manually set it in the 'Files' panel 
setwd("~/Desktop/")

# Load dependencies
require(dplyr)
require(reshape2)
require(REDCapR)
require(stringr)

################################################################################
# STEP 2 - Import REDCap data from API
api_url <- "https://redcap.partners.org/redcap/api/"

# Nicole's API keys; substitute yours 
api_keys <- list(clin = "98A8017C2C92DA7E7A53C50E1670C399",   
                 banda_dat = "EDD2EE5E5FAA347F65D24D575B9B1125",
                 clin_ss = 'B1EDFAD0DA74AF59597DCB7C18AC7578')


# Pull data from REDCap
clin <- redcap_read_oneshot(api_url, api_keys$clin)$data 
clin_ss <- redcap_read_oneshot(api_url, api_keys$clin_ss)$data
banda_dat <- redcap_read_oneshot(api_url, api_keys$banda_dat)$data


################################################################################
# STEP 3 - Load all code up to step 4 

## Read latest BANDA participant labels (banda_id and subject_id)
labels_cur <- banda_dat %>% 
  select(banda_id, subject_id, age, sex) %>%
  arrange(banda_id) %>% 
  mutate(subject_id = as.numeric(str_extract(subject_id, '\\d{3}'))) %>%
  filter(banda_id != '', str_detect(banda_id, 'BANDA'))


## Clean data from 'BANDA Clinical' and 'BANDA Clinical Second Scoring'
# Pulls both initial and 12m clinical data
clin_vars <- intersect(names(clin), names(clin_ss))

clin_clean <- clin %>% as_tibble %>%
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9]+'))) %>%
  select(subject_id, clin_vars) %>%
  melt(id.vars='subject_id') 

ss_clean <- clin_ss %>% as_tibble %>%
  mutate(subject_id = as.numeric(str_extract(redcap_id_ss, '[0-9]+'))) %>%
  select(subject_id, clin_vars) %>%
  melt(id.vars='subject_id')  

## Find entries that are different between clin_clean and ss_clean
ss_diff <- function(clin_dat = clin_all, ss_dat = ss_all, labels = labels_cur,
                    merge_vars = c('subject_id', 'variable')){
  
  df <- merge(clin_dat, ss_dat, by= merge_vars) %>%
    rename(clin = value.x, ss = value.y) %>%
    filter(clin != ss) %>%
    merge(labels_cur, ., by='subject_id')  %>%
    arrange(banda_id)
  return(df)
}




clin_diff<- ss_diff(clin_clean, ss_clean, labels_cur) %>%
  filter(clin!="[document]")

################################################################################
# STEP 4 - Save file

# Set file name
cur_date <- format(Sys.Date(),'%m%d%y')  
file_name <- paste0('ban
                    da_clin_ss_diffs_', cur_date, '.csv')

# Save file
write.csv(clin_diff, file_name, row.names=F)