# Read most current labels on the 'BANDA Data' REDCap project
# https://cran.r-project.org/web/packages/httr/vignettes/secrets.html
#install.packages(c('REDCapR', 'dplyr', 'reshape2', 'stringr'), dependencies = T)
require(REDCapR)   # Load REDCapR first!


api_url <- "https://redcap.partners.org/redcap/api/"

banda_dat <- redcap_read_oneshot(api_url, "EDD2EE5E5FAA347F65D24D575B9B1125")$data
clin <- redcap_read_oneshot(api_url, "98A8017C2C92DA7E7A53C50E1670C399")$data



require(dplyr)
require(reshape2)
require(stringr)


# Helper functions
id_format <- function(df) which(grepl('BANDA[0-9]{3}', df$banda_id) & 
                                  grepl('[0-9]{3}', df$subject_id))




################################################################################
# Grab IDs, labels, gender, clinical site and age from BANDA DATA
# Adjust n for the first n BANDA subjects
# Valid variable names include: banda_id, subject_id, sex, age, test_date, clin_site, 

pull_info <- function(dat, n=NA, return_vars = c('banda_id', 'subject_id', 
                                                  'sex', 'age', 'test_date')){
  
  df <- slice(dat, id_format(dat)) %>%  # Get only subjects with valid IDs
    select(return_vars) %>% arrange(banda_id) %>%
    mutate(clin_site = case_when(subject_id < 300 ~ 'BU',
                                 subject_id >= 500 ~ 'McLean',
                                 TRUE ~ 'MGH')) 
    
  if('sex' %in% return_vars) df <- mutate(df, sex = ifelse(sex, 'M', 'F'))
  if('clin_site' %in% return_vars) {
    df <- mutate(df, clin_site = case_when(subject_id < 300 ~ 'BU',
                                           subject_id >= 500 ~ 'McLean',
                                           TRUE ~ 'MGH'))}
  return(df)}




################################################################################
# GET LABELS FROM KSADS

# lifetime=T - get lifetime diagnosis as well
# thres - score cutoff

get_all_diagnos <- function(clindat, infodat=banda_dat,lifetime=F, all_ses=F, thres=NULL){
  
  ses <- ifelse(all_ses, '_2$', '_i$')
  pat <- ifelse(lifetime, paste0('_(life|lifetime)', ses), 
                paste0('_(cur|current)', ses))
  
  # Get IDs and demopgraphic information sex etc.
  banda_info <- pull_info(infodat)
  
  # Long form of all above threhold diagoses
  df <- clindat %>%
    select(-starts_with('cssrs'))%>%
    select(subject_id, matches(pat)) %>%
    mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9]+'))) %>% 
    merge(banda_info, ., by.x = 'subject_id') %>%
    melt(id.vars=names(banda_info)) %>%
    mutate(time = str_extract(variable, 'cur|life'),
           ses = ifelse(str_detect(variable, '_i$'), 'initial', '12m'),
           type = ifelse(str_detect(variable, 'mdd|dep|dysthymia'), 'dep',
                         ifelse(str_detect(variable, 'anx|phobia|avoid|panic|ago'), 'anx', 'other'))) %>%
    arrange(banda_id, time, variable)
  
  
  if(is.null(thres)) return(df)
  if(is.numeric(thres) & thres <= 4) return(filter(df, value >= thres))
  print('Check threshold input')
}




# Derive labels from KSADS data
get_init_labels <- function(clindat, banda_info=banda_dat){
  
  banda_info <- pull_info(banda_info)
  #banda_info <- pull_info(redcap_read_oneshot(api_url, "EDD2EE5E5FAA347F65D24D575B9B1125")$data)
  
  # Get current diagonsis at initial session
  all_diag <- clindat %>%
    get_all_diagnos(lifetime=F, all_ses=F, thres=3) %>%
    mutate(variable = paste0(variable, '=', value)) %>%
    group_by(banda_id, subject_id, clin_site, sex, age, time, ses) %>% 
    summarize(n=n(), details = paste(variable, collapse=", "),
              diagnosis = paste(unique(type), collapse="/")) %>%
    mutate(diagnosis=str_remove(diagnosis, '/?other'), 
           label= ifelse(str_detect(diagnosis, 'dep'), 'dep', 'anx')) %>%
    merge(banda_info, .,all.x=T) %>%
    replace_na(list(n=0, diagnosis='control', label='control')) %>%
    select(banda_id, subject_id, clin_site, sex, age, diagnosis, label, n, details)
    
  }
  

all_diag <- clin %>%
  get_all_diagnos(banda_dat, lifetime=F, all_ses=F, thres=3) %>%
  mutate(variable = paste0(variable, '=', value)) %>%
  group_by(banda_id, subject_id, clin_site, sex, age, time, ses) %>% 
  summarize(n=n(), details = paste(variable, collapse=", "),
            diagnosis = paste(unique(type), collapse="/")) %>%
  mutate(diagnosis=str_remove(diagnosis, '/?other'), 
         label= ifelse(str_detect(diagnosis, 'dep'), 'dep', 'anx')) %>%
  #merge(banda_info, .,all.x=T) %>%
  replace_na(list(n=0, diagnosis='control', label='control')) %>%
  select(banda_id, subject_id, clin_site, sex, age, diagnosis, label, n, details)


#fhs_vars <- paste0('fhs', rep(c(8:10), 2), 'b_', c('mother', 'father'), '_i' ) %>% sort()


fhs_i <- clin %>% select(subject_id, matches('^fhs.*_i$')) %>%
  select(-contains('name')) %>%
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9]{3}'))) %>% 
  left_join(select(banda200, banda_id:sex), .) %>%
  arrange(banda_id)

cssrs_i <- clin %>% select(subject_id, matches('^cssrs.*_i$')) %>%
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9]{3}'))) %>% 
  left_join(select(banda200, banda_id:sex), .) %>%
  arrange(banda_id)


wasi <- clin %>% select(subject_id, matches('^wasi')) %>%
  select(-contains('date'), -contains('file')) %>%
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9]{3}'))) %>% 
  left_join(select(banda200, banda_id:sex), .) %>%
  arrange(banda_id)
