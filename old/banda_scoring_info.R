# BANDA REVERSE SCORING AND RECODE INFO
# + WASI


################################################################################
# ALL SCALES 
ado_scales <- c('bisbas','handed','mfq', 'neo', 'rbqa',
                'rcads', 'shaps','stai_state','stai_trait','tanner')

pr_scales <- c('cbcl', 'masq','mfq','rmbi','stai_state','stai_trait')


################################################################################
# REVERSE SCORED ITEMS
# If recode/reverse scoring is not necessary, defaults to NULL

rev_master <- list(bisbas = setdiff(1:24,c(2,22)),
                   masq = c(3, 7, 10, 15, 22, 27, 39, 43, 47, 49, 56, 58, 60),
                   neo = c(1, 4, 7, 10),
                   rmbi = c(4, 5, 7, 11, 13, 15),
                   shaps = c(2, 4, 5, 7, 9, 12, 14),               
                   stai_state=c(1, 2, 5, 8, 10, 11, 15, 16, 19, 20), 
                   stai_trait=c(1, 6, 7, 10, 13, 16, 19))


################################################################################
# ALL MIN & MAXES
# If recode/reverse scoring is not necessary, defaults to NULL

item_mins <- c(bisbas = 1,
               cbcl = 0,
               handed = 1,
               masq = 1,
               mfq = 0,
               neo = 1,
               rbqa = 0,
               rcads = 0,
               rmbi = 0,
               shaps = 0,
               stai_state = 1,
               stai_trait = 1)

item_maxes <- c(bisbas = 4,
                cbcl = 2,
                handed = 3,
                masq = 5,
                mfq = 2,
                neo = 5,
                rbqa = 4,
                rcads = 3,
                rmbi = 2,    # Code 3=Don't Know as NA!!
                shaps = 1,   # Range is 0-1 after recoding 
                stai_state = 4,
                stai_trait = 4)


################################################################################
# SUBSCALES
# Use named lists if there are more than 1 composites
ss_master <- list(
  
  #BISBAS 
  bisbas = list(bas_drive = c(3, 9, 12, 21),
                bas_fun = c(5,10,15,20),
                bas_reward = c(4, 7, 14, 18, 23),
                bis = c(2, 8, 13, 16, 19, 22, 24)),
  
  # CBCL    
  cbcl = list(wd = c(42, 65, 69, 75, 80, 88, 102, 103, 111),  #88 or 99?
              soma = c(51, 54, 56),
              anx_dep = c(12, 14, 31, 32, 33, 34, 35, 45, 50, 52, 71, 89, 103, 112),
              social = c(1,11,25,38,48,55,62,64),
              thought = c(9,40,66,70,80,84,85),
              atten = c(1,8,10,13,17,41,45,46,62,80),
              delinq = c(26,39,43,63,67,72,81,82,90,96,101,105,106),
              aggr = c(3,7,16,19,20,21,22,23,27,37,57,68,74,86,87,92,94,95,97,104),
              inter = c(42, 65, 69, 75, 80, 88, 102, 103, 111, 51, 54, 56,12, 14, 
                        31, 32, 33, 34, 35, 45, 50, 52, 71, 89, 103, 112), #88 or 99?
              exter = c(26,39,43,63,67,72,81,82,90,96,101,105,106,3,7,16,19,20,21,
                        22,23,27,37,57,68,74,86,87,92,94,95,97,104)),
  
  
  # MASQ
  masq = list(gen_dis_dep = c(1, 5, 9, 12, 21, 23, 29, 31, 34, 36, 38, 45),
              gen_dis_anx = c(4, 8, 11, 14, 16, 20, 26, 32, 35, 55, 59),
              loss_int = c(18, 25, 41, 50, 51, 57, 61),
              anx_arou = c(2, 6, 13, 17, 19, 24, 28, 30, 37, 40, 42, 44, 46, 48, 52, 54, 62),
              anh_dep = c(3, 7, 10, 15, 22, 27, 39, 43, 47, 49, 56, 58, 60)),
  
  
  # RCADS
  rcads = list(social = c(4, 7, 8, 12, 20, 30, 32, 38, 43),
               panic = c(3, 14, 24, 26, 28, 34, 39, 41),
               dep = c(2, 6, 11, 15, 19, 21, 25, 29, 40, 47),
               sep_anx = c(5, 9, 17, 18, 45, 46),
               gen_anx = c(1, 13, 22, 27, 35, 37),
               ocd = c(10, 16, 23, 31, 42, 44)),
  
  
  # RMBI
  rmbi = list(fear_inhb = c(1,6,10,16,18),
              risk_avoid = c(7,8,13),
              non_app = c(2,4,5,9,11,15),
              shy_sen = c(3,12,14,17)))






