require(REDCapR)
require(jsonlite)
require(dplyr)
require(stringr)


url <- "https://redcap.partners.org/redcap/api/"
api_keys <- fromJSON("_api_keys.json")



# Demographics data ---------------------------
subject_info <- redcap_read_oneshot(url, api_keys$subject_info)$data %>%
  select(banda_id, subject_id, sex) %>%
  filter(grepl('BANDA', banda_id)) %>%         # filter out ineligible participants  
  mutate(sex = ifelse(sex==0, "F", "M")) %>%
  arrange(banda_id)
  
  


# Clinical data ---------------------------
# Includes K-SADS, CSSRS, WASI 
clin <- redcap_read_oneshot(url, api_keys$clin)$data %>%
  select(-redcap_id) %>%
  select_if(function(x){!all(is.na(x))}) %>%   # remove empty columns
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9]+'))) %>%
  merge(subject_info, ., by='subject_id') %>%
  arrange(banda_id)

# Metadata
clin_meta <- redcap_metadata_read(url, api_keys$clin)$data %>%
  select(field_name, form_name)
  

# Adolescent self-report data ---------------------------
# Includes MFQ, RCADS, STAI, SHAPS, BISBAS, RBQA, NEO, Chapman Handedness, Tanner

# The female and male forms are exactly the same except for the tanner assessment
female <- redcap_read_oneshot(url, api_keys$female)$data %>%
  select(-starts_with('prescan'))
male <- redcap_read_oneshot(url, api_keys$male)$data %>% 
  rename(subject_id=subject_id_i) %>%
  select(-starts_with('prescan'))
names(female) <- str_remove(names(female), '_girls')
names(male) <- str_remove(names(male), '_boys')

# Merge data
ado <- bind_rows(female, male) %>%
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9-]{3}'))) %>%
  select(-redcap_id) %>%
  select_if(function(x){!all(is.na(x))}) %>%   # remove empty columns
  merge(subject_info, ., by='subject_id') %>%
  arrange(banda_id)

# Metadata
ado_meta <- redcap_metadata_read(url, api_keys$female)$data %>%
  select(field_name, form_name) %>%
  mutate(field_name = str_remove(field_name, '_girls'))
  

# Parent self-report data ---------------------------
par <- redcap_read_oneshot(url, api_keys$par)$data%>%
  select(-redcap_id) %>%
  select_if(function(x){!all(is.na(x))}) %>%   # remove empty columns
  rename(subject_id = subject_id_i) %>%
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9]+'))) %>%
  merge(subject_info, ., by='subject_id') %>%
  arrange(banda_id)

par_meta <- redcap_metadata_read(url, api_keys$female)$data %>%
  select(field_name, form_name) 





