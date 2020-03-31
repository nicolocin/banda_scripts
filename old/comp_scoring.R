# COMPOSITE SCORING


require(dplyr)
require(reshape2)
require(stringr)
require(psych)
require(REDCapR)

#source('')

api_url <- "https://redcap.partners.org/redcap/api"

# Nicole's API keys
api_keys <- list(clin = "98A8017C2C92DA7E7A53C50E1670C399",   
                 banda_dat = "EDD2EE5E5FAA347F65D24D575B9B1125",
                 parent = "791D741B35F81F57987D97350DD481D0",
                 female ="BBCEAAF7EB975D9842549CA5B13DB36B",
                 male = "7EC8D15B09F9575BF8807667D4E9CCAB")


#shaps_all <- bind_rows(comp_scoring_by_ssn(female), 


################################################################################
# COMPOSITE CALCULATION
################################################################################
# Input: dat = all reverse-scored items for an assessment/scale
#        ss_items = question number of items that belong to a subscale  
# Output: subscale score

calc_ss <- function(scale_dat, ss_items){

  q_no <- as.numeric(str_extract(names(scale_dat), '\\d+')) 
  ss_dat <- scale_dat[q_no %in% ss_items]
  
  ss_score <- rowSums(ss_dat, na.rm=T) 
  incomplete <- apply(ss_dat, 1, function(x) all(is.na(x)))
  
  ss_score[incomplete] <- NA                    # NA if all items are missing
  return(ss_score)
}



# 1) CALC_COMPS (and subscales)
# Input: dat - a dataframe that only include the reversed-scored items for one composite
#                 for a single session
# Output: Cleaned composite for all subjects without labels

calc_comp <- function(dat, var_name, all_ss=ss_master){
  
  incomplete <- apply(dat, 1, function(x) all(is.na(x)))   

  # When there's only one composite, sum everything
  if(!var_name %in% names(all_ss)) {    
    comp <- rowSums(dat, na.rm=T) %>%
      as_tibble %>% rename(total = value)
   # comp[incomplete] <- NA               # NA for missing assessments
    comp[incomplete,] <- NA
    return(comp)} 
  
  # When there are multiple subscales
  var_ss <- all_ss[[var_name]]
  ss_scores <- as_tibble(sapply(var_ss, calc_ss, scale_dat=dat))      # Calculate all subscores
  return(ss_scores)
}


################################################################################
# COMPOSITE CALCULATION
################################################################################
# Calculate composite score for a single assessment for one session
# Input: Raw data
# Output: Cleaned composites without labels for one assessment for one session
comp_scoring <- function(dat, var_name, session='i',all_rev=rev_master, all_recode=recode_master,
                                mins=item_mins, maxes=item_maxes, all_ss=ss_master){
  
  sids <- if('subject_id_i' %in% names(dat)){dat$subject_id_i} else {dat$subject_id}

  
 df <- rev_items_by_ssn(dat, var_name, session, all_rev, mins, maxes, all_recode) %>% 
    calc_comp(var_name, all_ss) # %>% bind_cols(subject_id = sids, .) 
 names(df) <- paste0(var_name, '_', names(df))
 return(df)
}





# Input: Raw data from one session
# Output: Cleaned composites for all assessments without labels for one session (cross-sectional)
comp_scoring_cs <- function(rawdata, session='i', ado=T, all_rev=rev_master, all_recode=recode_master,
                            mins=item_mins, maxes=item_maxes, all_ss=ss_master) {
  
  sids <- if('subject_id_i' %in% names(rawdata)){rawdata$subject_id_i} else {rawdata$subject_id}
  vars <- if(ado) {ado_scales} else {pr_scales}  #subscale names
  
  #for(var in var_names){
  #  df <- bind_cols(df, comp_scoring(dat=rawdata, var_name=var,session=session, all_rev=all_rev, 
   #                                  all_recode=all_recode, mins=mins, maxes=maxes, all_ss=all_ss))}
  
  df <- sapply(vars, comp_scoring, dat=rawdata, session=session, 
                    all_rev=all_rev, all_recode=all_recode, mins=mins, maxes=maxes) %>%
    bind_cols(data.frame(subject_id = sids), .) %>%
    as_tibble
  
  return(df)
}


# Input: Raw item-level data for one assessment for all three sessions
# Output: Cleaned composites without labels for all three sessions
comp_scoring_long <- function(dat, var_name) {
  
  sids <- if('subject_id_i' %in% names(dat)){dat$subject_id_i} else {dat$subject_id}

  df <- data.frame(subject_id = sids, 
                   comp_scoring(dat, var_name, 'i'),
                   comp_scoring(dat, var_name, '1'),
                   comp_scoring(dat, var_name, '2'))
  names(df)[2:ncol(df)] <- paste0(var_name, '_', c('init', '6m', '12m'))
  
  return(df)
}



