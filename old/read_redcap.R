require(dplyr)
require(reshape2)
require(stringr)
require(magrittr)

require(REDCapR)


#setwd("~/Desktop/nlo/behavioral/github/scripts")

################################################################################
# Import data from API
api_url <- "https://redcap.partners.org/redcap/api/"

# Nicole's API keys
api_keys <- list(clin = "98A8017C2C92DA7E7A53C50E1670C399",   
                 banda_dat = "EDD2EE5E5FAA347F65D24D575B9B1125",
                 parent = "791D741B35F81F57987D97350DD481D0",
                 female ="BBCEAAF7EB975D9842549CA5B13DB36B",
                 male = "7EC8D15B09F9575BF8807667D4E9CCAB",
                 ss = "B1EDFAD0DA74AF59597DCB7C18AC7578")


# CLINICAL - KSADS, FHS, WASI
# PARENT - MASQ, MFQ_PR, STAI_PR, RMBI_PR, CBCL_PR
# ADOLESCENT - MFQ, RCADS, STAI, SHAPS, BISBAS, RBQA, NEO, HANDED, TANNER
# BANDA DATA - labels, demographics, medication

clin <- redcap_read_oneshot(api_url, api_keys$clin)$data
par <- redcap_read_oneshot(api_url, api_keys$parent)$data
female <- redcap_read_oneshot(api_url, api_keys$female)$data
male <- redcap_read_oneshot(api_url, api_keys$male)$data 
banda_dat <- redcap_read_oneshot(api_url, api_keys$banda_dat)$data


# Adolescent combined 
names(female) <- str_remove(names(female), '_girls')
names(male) <- str_remove(names(male), '_boys')
female <- female %>% select(-prescan6a) 
male <- male %>% rename(subject_id=subject_id_i)  %>% select(-prescan6a) 

ado_complete <- bind_rows(female, male) %>%
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9-]{3}'))) #%>%
  #select(subject_id, prescan1:stai_gen_20)
#  select(-starts_with('mfq_'), -starts_with('stai_pres'), -starts_with('stai_gen'))


scan_dates <- read.csv('BANDA_ScanDates.csv') %>%
  select(subject_id=Clinical.Subject.ID, scan_date=Scan.Appointment)

banda_info <-  pull_info(banda_dat, return_vars=c('banda_id', 'subject_id', 
                                      'sex', 'age','dob', 'test_date')) %>%
  merge(ado_complete, by='subject_id') %>%
  #mutate(scan_date = as.Date(str_extract(scan_date, '[0-9/]+'), format="%m/%d/%y"),
  #       dob=as.Date(str_extract(dob, '[0-9-]+'), format="%Y-%m-%d"),
  #       test_date=as.Date(str_extract(test_date, '[0-9-]+'), format="%Y-%m-%d"),
  #       scan_age=(scan_date - dob)/365.25) %>%
  arrange(banda_id) %>%
  mutate(scan_age_round = round(scan_age))

  
banda200 <- banda_dat[c('banda_id', 'subject_id', 
                                'sex', 'age','dob', 'test_date')] %>% 
  arrange(banda_id) %>% 
  filter(str_starts(banda_id, 'BANDA')) %>% slice(1:200)
  #mutate(test_date=as.Date(test_date,format="%Y-%m-%d"), 
  #       days_since_first = Sys.Date() - test_date) %>%
  #filter(days_since_first > 425) 



# All self-report 
par_comp_i <- par %>% 
  select(subject_id=subject_id_i,ends_with('_i')) %>%
  select(-starts_with('demo')) %>%
  comp_scoring_cs(session='i', ado=F) %>%
  merge(banda200, ., by='subject_id') %>% arrange(banda_id)


par_raw_2 <- par %>% 
  select(subject_id=subject_id_i,ends_with('_2')) %>%
  select(-starts_with('demo')) %>%
  #all_rev_items_by_ssn(session='2', ado=F) %>%
  merge(banda200, ., by='subject_id') %>% arrange(banda_id)


write.csv(par_rev_i, 'par_item_rev_initial_banda200.csv', row.names=F)
#write.csv(par_rev_2, 'par_item_rev_12m_banda200_100719.csv', row.names=F)

# All self-report reverse scored
ado_comp_i <- ado_complete %>% 
  select(subject_id, ends_with('_i')) %>%
  comp_scoring_cs(session='i', ado=T) %>%  #all_rev_items_by_ssn(session='2') %>%
  merge(labels_cur, ., by='subject_id') %>% arrange(banda_id)



ado_rev_2 <- all_rev_items_by_ssn(ado_complete, session='2') %>%
  merge(banda200, ., by='subject_id') %>%
  arrange(banda_id)
write.csv(ado_rev_2, 'ado_item_rev_12m_banda200_031520.csv', row.names=F)


ado_comp_2 <- ado_complete %>% 
  select(subject_id, ends_with('_2')) %>%
  comp_scoring_cs(session='2', ado=T) %>%  #all_rev_items_by_ssn(session='2') %>%
  merge(labels_cur, ., by='subject_id') %>% arrange(banda_id)
write.csv(ado_comp_2, 'ado_comp_12m_banda200_031520.csv', row.names=F)



ksads_raw_i <- clin %>%
  mutate(subject_id = as.numeric(str_extract(subject_id, '[0-9]{3}'))) %>%
  select(-starts_with('cssrs'), -starts_with('fhs'), -redcap_id) %>%
  select(-matches('subtype|sxs|dur|onset|eps')) %>%
                   ##contains('sxs'), -contains('dur'), -contains('onset')) %>%
  select(subject_id, ends_with('_i')) %>%        # init / 12m
  #select(subject_id, contains('life')) %>%        # cur / life
  left_join(banda200, .,by='subject_id') %>% 
  arrange(banda_id)
