######################################################################
#                     EXTRACT SELECTED VARIABLES                     #
######################################################################

# define supplemental variables specific to studies. Same strategy as for CF_ADD
SV_ADD <- list(IA2015= c("slangq", "slangrm", "slangi", "s250", "s474a", "s475", "hw2"),
               ZW1988 = c("sstrata")  )


vars_names <- c(                           
  ### Respondent's basic data Section
  "caseid",               
  paste0("v00", 0:9),
  paste0("v0", 10:16),
  "v008a",
  "v021", "v022", "v023",
  "v024",                             # needed for India 1999
  "v045a", "v045b", "v045c", "v046",  # language of the interview
  
  ### Respondent's basic data Section, continued
  "v101","v102",                  # region and type of place of residence (urban/rural)
  "v106","v107", "v108","v133",   # education
  "v113", "v115", "v116",         # drinking water source & toilet facility
  "v119","v122", "v130","v131",   # electricity & refrigerator, religion & ethnicity
  "v123", "v124", "v125",         # bicycle / motorcycle / car
  "v155",
  "v161",                         # type of cooking fuel
  "v190", "v191",                 # wealth index
  "v190a", "v191a",               # wealth index
  
  ### Reproduction Section W21
  "bidx", "bord",         # birth history index and reverse
  paste0("b", 0:9),       # b5 is alive at interview
  "b17", "b18",           # day of birth of child (only avail. for some dhs 7), CDC of birth of child
  "b19",                  # age of child in months based on day of birth
  
  ### Reproduction Section W22
  "v201", "v208",         # total number of children ever born, of births in the last five years
  "v213",                 # currently pregnant
  "v224",                 # number of entries in the birth history (=v201 if less than 20 births)
  
  ### Maternity Section
  "midx",                 # index to the birth history 
  "m2", "m3",
  "m4", "m5",             # duration of breastfeeding. If still bf : m4=95, m5=age at interview
  "m6", "m7",             # duration of pp amenorrhea. Same as m4 and m6 for still ameno
  "m15",                  # place of delivery (starting at DSH-2)
  "m17",
  "m27",                  # flag variable for breastfeeding
  "m28",                  # flag variable for amenorrhea
  "m34", 
  "m35","m36",            # number of times child bf previous night/daylight hours. No longer part of latest DSH
  "m38",                  # drank from bottle with nipple
  "m55",
  
  ### Contraceptive Use 
  "v312", "v313",         # Current contraceptive method + type (modern, traditional, folkloric ) 
  "v323",                 # Brand of pill used
  
  ### Maternity and feeding Section W42
  "v401",                 # last child born by caeserean section ?
  "v404", "v405",         # currently BF a child, currently postpartum amenorrheic
  "v407", "v408",         # number of times the last child was breastfed during the previous night / daylight hours
  
  "v415",                 # child drank anything from a bottle with a nipple in previous day and night
  "v437","v438","v445",   # weight, height, body mass index (BMI)
  "v457",                 # anemia level
  "v501",                 # current marital status
  
  "v701",                 # partner's highest level of education attended 
  "v703",                 # literacy of the respondent's current or last partner
  
  "v711", "v712","v714",  # respondent working 
  
  # Complementary food variables
  CF,
  # Other survey specific studies
  unique(unlist(SV_ADD))
)

vars_all <- search_variables(dataset_filenames = datasets, 
                             reformat=REFORMAT, 
                             variables = vars_names)
setDT(vars_all)



# remove ADD variables from vars_all. Then add them one by one, to
# avoid including them for surveys where they have a different meaning
ADD <- c(CF_ADD, SV_ADD)
vars_compl <- vars_all[variable %in% unlist(ADD)]
vars_all <- vars_all[!variable %in% unlist(ADD)]
for(I in seq_along(ADD)){
  var_compl_loc <- vars_compl[variable %in% ADD[[I]] & 
                                survey_id == paste0(names(ADD)[I], "DHS") ]
  vars_all <- rbindlist(list(vars_all, var_compl_loc))
  rm(var_compl_loc)
}
rm(vars_compl)
rm(ADD)


#####################################################################
#                            EXTRACT DATA                            #
######################################################################

# make a list in order to parallelize call to function extract_dhs
vars_all_l <- lapply(X = vars_all[,unique(dataset_filename)], 
                     FUN = function(s_a, dt_a){ dt_a[dataset_filename == s_a]},
                     dt_a=vars_all)
names(vars_all_l) <- vars_all[,unique(dataset_filename)]


# CL = makeCluster(60)
CL = makeCluster(3)
invisible( clusterEvalQ(CL, {library(rdhs)}) )

dtb_dhs_l <- parLapplyLB(cl = CL, 
                         X = names(vars_all_l), 
                         fun = function(x, v){extract_dhs(v[[x]])[[1]]},
                         v = vars_all_l)
names(dtb_dhs_l) <- names(vars_all_l)
stopCluster(CL)
rm(vars_all_l)

# non parallelized version
# dtb_dhs_l <- extract_dhs(vars_all, add_geo = F)


######################################################################
#                 DEFINE COMPLEMENTARY FOOD VARIABLE                 #
######################################################################


# classify types of CF
vars_all[, CF_type := NA_character_]

vars_all[variable %in% CF, CF_type := "other"]

vars_all[grepl("water", description, ignore.case = T) & 
           !grepl("sugar", description, ignore.case = T), CF_type := "water"]

vars_all[grepl("\\b(formula|milk|lait)\\b", description, ignore.case = T), CF_type := "milk"]

vars_all[grepl("other milk prod", description, ignore.case = T), CF_type := "other"]

vars_all[grepl("\\b(juice|soda|beverage|sugar water|other liquid)\\b",
               description, ignore.case = T), CF_type := "other_liquid"]

# avoid variable %in% CF above, but then make sure no other variable is misclasified
vars_all[! variable %in% CF, CF_type := NA_character_]


# # check
# View(unique(vars_all[variable %in% CF,.(description, CF_type, dataset_filename)], 
#             by=c("description", "CF_type")) )

# surveys for which no variable is available for one the categories
# vars_all[, sum(CF_type=="water", na.rm=T), by=.(survey_id)][V1==0]
# vars_all[, sum(CF_type=="milk", na.rm=T), by=.(survey_id)][V1==0]
# vars_all[, sum(CF_type=="other_liquid", na.rm=T), by=.(survey_id)][V1==0]
# vars_all[, sum(CF_type=="other", na.rm=T), by=.(survey_id)][V1==0]



# Definition of scores is performed here to avoid having variables that are mostly NA

CF_f <- function(X, food_var_a, rdm_chck_a=T){
  
  # takes a data.table as an argument, returns a vector of  
  # scores of CF for each individual in the data.table
  
  food_var_loc <- food_var_a[survey_id == X$SurveyId[1], variable]
  
  RESULTS <- rep(0, nrow(X))
  
  if(length(food_var_loc)==0){
    return(RESULTS) # if no variable, assume 0. NB: for surveys without any CF var, this 
    # must be set to NA afterwards
  }
  
  # loop on CF elements found in the dataset
  for(VAR in food_var_loc){
    
    x_loc0 <- X[, VAR] # original variable
    
    x_loc <- tolower(x_loc0)
    x_loc[is.na(x_loc)] <- "no"   # if not available, assume no. As for 
    # "age at" = 0 means "yes", don't translate na in 0
    
    if( class(x_loc0) %in% c("numeric", "integer") & !all(x_loc0 %in% c(NA, 1, 2)) ){ # trivial case
      
      x_loc0[is.na(x_loc0)] <- 0            # NB : x_loc0, not x_loc
      x_loc_new <- (as.numeric(x_loc0)>0 ) 
      
      
    } else{
      
      x_loc[substr(x_loc, 1, 3) == "yes"] <- "yes"
      
      # values taken
      VT <- names(table(x_loc))  
      
      
      x_loc_new <- rep(NA_integer_, nrow(X)) # will store modified versions
      
      NO <- c("no", "noe", "none", "non",
              "missing", "not given",  "not given yet", "didn't ate yesterday", 
              "dk", "don't know", "dont know",  "don t know", "dont'know",
              "don't know/don't remember",
              "0", "98", "99")
      YES <- c("yes", "si", "oui",
               "gave but don't know times",  "otro",  "1 or more times",
               "juice", "tea/other home", "otro solido/papilla",
               1:60, # by default, all positive numerical values are yes
               "6+", "7+",  "7 times or more", "11+", 
               "child died before" 
      )
      
      # cat("\n", VAR, "- values taken:\n") ; print(VT)
      
      if( all( suppressWarnings(as.numeric(x_loc0)) %in% 1:2 ) ){ 
        YES = YES[YES!="2"]
        NO = c(NO, "2")  } 
      
      if( (all(c("yes", "no") %in% VT) & (!"1" %in% VT)) | (!"8"%in% VT)) {
        YES = YES[YES!="9"]
        NO  = c(NO, "9")  }
      
      
      if( any(c("7+", "7 times or more") %in% VT) ){
        YES = YES[!YES %in% c("8","9")] 
        NO  = c(NO, c("8","9") )  }
      if( "6+" %in% VT ){
        YES = YES[!YES %in% c("7", "8","9")] 
        NO = c(NO, c("7", "8","9") ) }
      
      if("not given yet" %in% VT){
        YES = c(YES, c("0") )
        NO = NO[!NO %in% c("0")] 
      }
      
      x_loc_new[x_loc %in% NO] <- 0
      x_loc_new[x_loc %in% YES] <- 1
      
      # print(mean(x_loc_new))
      
      ### checks
      if(! all(x_loc_new %in% c(0, 1))){ stop("check for ", X[1, "v000"], " variable ", VAR, "\n") }
      # random check on classification. 1/20 variable examined
      if( rdm_chck_a & (runif(1, 0, 1) < 1/20)) {
        print(table( "New value" = x_loc_new, "Original value" = x_loc0, useNA = "if"))
      }
      
      
    } # end of case where not all values are not numeric
    
    RESULTS = RESULTS + x_loc_new
  } # end of loop on variables
  
  return(RESULTS)
}


# test <- CF_f(dtb_dhs_l$NPKR7HFL, vars_all[CF_type == "other"])


vars_all[, table(CF_type, useNA = 'a')]

wrapper_f <- function(type_a){
  res <- lapply(X = dtb_dhs_l,
                FUN =  CF_f, 
                food_var_a = vars_all[CF_type == type_a],
                rdm_chck_a = F)
  return(unlist(res))
}

CF_types <- c("water", "milk", "other_liquid", "other")

CF_scores <- lapply(FUN = wrapper_f, X = CF_types)
names(CF_scores) <- CF_types 
rm(wrapper_f); rm(CF_types)



######################################################################
#                             STACK DATA                             #
######################################################################


# remove CF variables from variables kept and stacked
dtb_dhs <- lapply(dtb_dhs_l,
                  function(x){ 
                    keep <- names(x)[names(x) %in% c("SurveyId", 
                                                     vars_names[!vars_names%in% CF])]
                    return(x[, keep])  })

dtb_dhs <- rbindlist(dtb_dhs, use.names = T, fill=T)

# info from datasets
dtb_dhs[datasets, on=.(SurveyId), `:=`(DHS_CountryCode=DHS_CountryCode,
                                       SurveyYear=SurveyYear)]


# define age at interview
# https://dhsprogram.com/data/calculating-the-age-of-children.cfm
# nb : for those died, age_interview is age would have had at interview date
dtb_dhs[, age_interview := as.numeric(v008-b3)] 

dtb_dhs[, age_interview_grouped:=paste0("g", floor(age_interview/2))]
# dtb_dhs[, .N, keyby=.(age_interview_grouped, age_interview)] # check
dtb_dhs[age_interview==0, age_interview:=1/3] # cf. Trussell et al. 1992
dtb_dhs[, mab:= round((b3- v011)/12)] # maternal age at birth of the child, in years



# bring back CF scores
dtb_dhs[, `:=` (CFS_water = CF_scores$water,
                CFS_milk  = CF_scores$milk,
                CFS_oliq  = CF_scores$other_liquid,
                CFS_other = CF_scores$other)] 

rm(CF_scores)

# check
# dtb_dhs[is.na(CFS_water*CFS_milk*CFS_oliq*CFS_other), .N, by=SurveyId]

no_CF_surveys <- datasets[N_food_all==0, SurveyId]

dtb_dhs[SurveyId %in% no_CF_surveys, `:=` (CFS_water = NA_integer_,
                                           CFS_milk  = NA_integer_,
                                           CFS_oliq  = NA_integer_,
                                           CFS_other = NA_integer_)]

rm(no_CF_surveys)

# # check.
# # After 24 mths : 0s by construction. This is not a problem because 
# # BF == FALSE => exclusive BF == FALSE 
# plot(dtb_dhs[bidx==1, mean(CFS_water+CFS_milk+CFS_oliq+CFS_other, na.rm=T),
#          keyby=.(age_interview)])


######################################################################
#              DEFINE NEW VARIABLES WITH GROUPED LEVELS              #
######################################################################


# dtb_dhs[,table(b9, useNA = "a")]
dtb_dhs[,lives_with_resp:=b9]
dtb_dhs[lives_with_resp %in% c("15+ & live elsewhere", "father", "lives elsewhere", 
                               "other relative", "someone else"),
        lives_with_resp:="no"]
dtb_dhs[lives_with_resp == "9", lives_with_resp:="missing"]
dtb_dhs[lives_with_resp == "respondent", lives_with_resp:="yes"]



# dtb_dhs[,table(v213, useNA = "a")]
dtb_dhs[, cur_preg:=v213] # to match WFS style


##### residence
# dtb_dhs[,table(v102, useNA = "a")]
dtb_dhs[, residence:=v102]
dtb_dhs[residence %in% c("menos de 2,500", "2,500 - 19,999", "20,000 y mas"), 
        residence:="rural"]
dtb_dhs[residence == "areas metropolitanas", 
        residence:="urban"]
# dtb_dhs[,table(residence, useNA = "a")]


##### electricity
dtb_dhs[, electricity_detailed:=v119]
dtb_dhs[electricity_detailed %in% c("not a de jure resident",  "not dejure resident", "not a dejure resident"),
        electricity_detailed:="ndjr"]
dtb_dhs[electricity_detailed =="9", electricity_detailed:="missing"]
# dtb_dhs[,table(electricity_detailed, useNA = "a")]

dtb_dhs[, elec_g:=""]
dtb_dhs[electricity_detailed=="yes", elec_g:="y"]
dtb_dhs[electricity_detailed=="no", elec_g:="n"]



##### refrigerator
# dtb_dhs[, fridge:=v122]
# dtb_dhs[fridge %in% c("not a de jure resident",  "not dejure resident", "not a dejure resident"),
#         fridge:="ndjr"]
# dtb_dhs[fridge =="9", fridge:="missing"]
# # dtb_dhs[,table(fridge, useNA = "a")]


##### highest level of education
dtb_dhs[, education:=v106]
dtb_dhs[, table(education, useNA = 'a')]
dtb_dhs[education %in% c("dk", "unknown - certificate", "missing", "other", "others", "9"),
        education:="dk - missing"]
dtb_dhs[education %in% c("no education/preschool"), education:="no education"]
dtb_dhs[education %in% c("basic", "primary or less"), education:="primary"]
dtb_dhs[education %in% c("some secondary"), education:="secondary"]
dtb_dhs[education %in% c("slc and above"), education:="higher"]

dtb_dhs[, educ_g:=""]
dtb_dhs[education %in% c("primary", "secondary", "higher"), educ_g:="y"]
dtb_dhs[education == "no education", educ_g:="n"]
dtb_dhs[, table(educ_g, useNA = 'a')]


##### partner's highest level of education
dtb_dhs[, education_partner:=v701]
# dtb_dhs[, table(educ_partner, useNA = 'a')]
dtb_dhs[education_partner %in% c("dk", "don't know", "don't know level", "other", "missing","9"),
        education_partner:="dk - missing"]
dtb_dhs[education_partner %in% c("no education / pre-school", "no education/preschool"),
        education_partner:="no education"]
dtb_dhs[education_partner %in% c("preparatory", "elementary", "basic"),
        education_partner:="primary"]
dtb_dhs[education_partner %in% c("secondary special", "new secondary", "intermediate diploma"),
        education_partner:="secondary"]
dtb_dhs[education_partner %in% c("university"),
        education_partner:="higher"]

dtb_dhs[, educ_partner_g:=""]
dtb_dhs[education_partner %in% c("primary", "secondary", "higher"), educ_partner_g:="y"]
dtb_dhs[education_partner == "no education", educ_partner_g:="n"]
# dtb_dhs[, table(educ__partner_g, useNA = 'a')]


##### religion
dtb_dhs[, religion:=v130]
# dtb_dhs[,table(religion, useNA = "a")]

##### ethnicity
dtb_dhs[, ethnicity:=v131]
# dtb_dhs[,table(ethnicity, useNA = "a")]


# dtb_dhs[,table(v714, useNA = "a")]
dtb_dhs[, working_cur:=v714]
dtb_dhs[working_cur == "9", working_cur:="missing"]
# dtb_dhs[,table(working_cur, useNA = "a")]


### time to water source
dtb_dhs[, time_to_water := v115]
dtb_dhs[grep("(delivered)|(premise)", time_to_water), time_to_water:="0"]
dtb_dhs[(grep("(resid)|(missing)|(know)|(dk)", time_to_water)), time_to_water:=NA_character_]
dtb_dhs[(time_to_water %in% c("995", "999", "998") ), time_to_water:=NA_character_]
dtb_dhs[time_to_water=="more than 12 hours", time_to_water:= "720"]
dtb_dhs[time_to_water=="one day or longer", time_to_water:= "1440"]
dtb_dhs[time_to_water=="995+", time_to_water:= "995"]
dtb_dhs[, table(time_to_water, useNA = 'a')]

# # there are surveys for which base of v115 is All respondents except those with 
# # household water piped into the residence 
# # check surveys with missing values (but for which not all values are missing!)
check_surveys <- dtb_dhs[,mean(is.na(time_to_water)), by=SurveyId][V1<1, SurveyId]
# dtb_dhs[is.na(v115) & SurveyId%in% check_surveys, .N]
# dtb_dhs[is.na(v115) & SurveyId%in% check_surveys, summary(as.factor(v113), useNA = 'a')]
# set correct value for time_to_water in those cases
dtb_dhs[is.na(v115) & SurveyId%in% check_surveys & !is.na(v113), time_to_water:="0"]
rm(check_surveys)

dtb_dhs[, time_to_water := as.numeric(time_to_water)]
dtb_dhs[, water30less_g := ""]
dtb_dhs[time_to_water<30, water30less_g :=  "y"]
dtb_dhs[time_to_water>=30, water30less_g :=  "n"]


### wealth quintiles
# dtb_dhs[,table(v190)]
# check that second indeed means lower wealth than fourth
# dtb_dhs[v190%in% c("lowest", "second", "fourth", "highest"), table(SurveyId)]
# dtb_dhs[SurveyId== "UG2016DHS", mean(v191),  by=v190]
# dtb_dhs[SurveyId!= "UG2016DHS", mean(v191, na.rm=T),  by=v190]
# dtb_dhs[, wealth:=v190]
# dtb_dhs[v190=="lowest", wealth:="poorest"]
# dtb_dhs[v190=="second", wealth:="poorer"]
# dtb_dhs[v190=="fourth", wealth:="richer"]
# dtb_dhs[v190=="highest", wealth:="richest"]
# dtb_dhs[v190=="", wealth:=NA_character_]
# # dtb_dhs[,table(wealth)]


### for BF and amenorrhea, new, current status, variables will be defined anyway afterwards

# dtb_dhs[,table(m4, useNA = "a")]
dtb_dhs[m4=="not currently breastfeeding", 
        m4:="ever breastfed, not currently breastfeeding"]
dtb_dhs[m4=="sill breastfeeding",
        m4:="still breastfeeding"]
dtb_dhs[m4 %in% c("don't know", "don t know", "dk",  "ne sait pas", "99",
                  "missing", "missing (duration not asked if child had died)"), 
        m4:="missing"]

# dtb_dhs[,table(m6, useNA = "a")]
dtb_dhs[m6 %in% c("pd. not returned"), 
        m6:="period not returned"]
dtb_dhs[m6 %in% c("don t know", "dk", "ne sait pas"),
        m6:="don't know"]


######################################################################
#                       CURRENT CONTRACEPTIVE METHOD                 #
######################################################################


# dtb_dhs[,table(v313, useNA = "a")]
# hormonal contraception is modern
# don't lose time on other methods
# dtb_dhs[v313=="modern method",table(v312, useNA = "a")]

dtb_dhs[, contracep_cur:=v312]

# MELA = Amenorrea por lactancia (Dominican Replublic)
dtb_dhs[contracep_cur %in% c("lactational amenorrhea","lactational amenorrhea (lam)",
                             "lact. amen. method", "lact. amen. method", "mela"),
        contracep_cur:="LAM"]

dtb_dhs[contracep_cur %in% c("emergency contraception", "emergency contraceptive"), 
        contracep_cur:="emergency"]

dtb_dhs[contracep_cur %in% c("implants", "implants / norplant", "implants/norplant", "norplant"), 
        contracep_cur:="implants"]

dtb_dhs[contracep_cur %in% c("injections", "injection 1 months", "injections 2 month", 
                             "injections 3 month", "injection (3 monthly)", "monthy injection",
                             "injections every three months",
                             "injection (monthly)", "injection 1 months"),
        contracep_cur:="injections"]

dtb_dhs[contracep_cur %in% c("diaphragm", "diaphragm /foam/jelly", "diaphragm, foam, jelly", 
                             "diaphragm/ foam/ jelly", "diaphragm/foam/jelly", "diaphragm/jelly", 
                             "foam, jelly", "foam or jelly", "foam, jelly, diaphr.", "foaming tablets",
                             "foam/jelly", "foam/jelly/tablets/ovule/diaphragm"),
        contracep_cur:="diaphragm/foam/jelly"]

dtb_dhs[contracep_cur %in% c("monthly pill", "monthly pill/chinese pill",
                             "chinese (monthly) pill", "monthly/chinese pill"),
        contracep_cur:="monthly pill"]

dtb_dhs[contracep_cur %in% c("vaginal ring", "vaginal methods"),
        contracep_cur:="vaginal method"]

dtb_dhs[contracep_cur %in% c("collier", "methods of fixed days: cs methods", 
                             "collier (cs)", "mjf/collier du cycle", 
                             "cycle beads/standard days",
                             "fixed days (string)", "standard days method", "mjf",
                             "standard days method (sdm)", "standard days (sdm)", "standard days",
                             "fertility wheel calculator", "billings method", 
                             "basal body temperature", "symptothermal",
                             "mucus/billing/ovulation", "mucus/billings/ovulation", "mucus/ billings/ ovulation"),
        contracep_cur:="natural"] # LAM left aside

dtb_dhs[contracep_cur %in% c("oher modern method"),
        contracep_cur:="other modern method"]

# dtb_dhs[,table(v313, useNA = "a")]
# dtb_dhs[v313=="modern method",table(contracep_cur, useNA = "a")]


dtb_dhs[,hormonal_contracep_cur:=FALSE]

dtb_dhs[contracep_cur %in% c("implants", "injections", "contraceptive patch",
                             "pill", "monthly pill", "iud"),
        hormonal_contracep_cur:=TRUE]


# # Check
# dtb_dhs[,.(M=mean(hormonal_contracep_cur)*100),
#         by=.(SurveyId, DHS_CountryCode)][DHS_CountryCode=="NP"]
# dtb_dhs[,.(M=mean(hormonal_contracep_cur)*100), by=.(SurveyId )][M<1]


######################################################################
#                SPECIAL CASES WHERE SAMPLING WEIGHT 0               #
######################################################################

dtb_dhs[v005==0, table(SurveyId)]

# "The 2017-18 PDHS presents national data totals for Pakistan that 
# exclude Azad Jammu and Kashmir as well as Gilgit Baltistan."
dtb_dhs[v005==0 & SurveyId == "PK2017DHS", table(v024)]
# we simply remove strata  
dtb_dhs = dtb_dhs[!(SurveyId == "PK2017DHS" & v024 %in% c("ajk", "gb") ) ]

# remove few cases of 0 sampling weight that seem errors
dtb_dhs = dtb_dhs[!(SurveyId == "BD2011DHS" &  v005==0)]
dtb_dhs = dtb_dhs[!(SurveyId == "YE1991DHS" &  v005==0)]


######################################################################
#                        SPECIAL CASE KENYA 2014                     #
######################################################################

###### Treat the special case of Kenya 2014 (see dl_datasets.R)

vars_households <- search_variables(dataset_filenames = datasets_households, 
                                    variables = c("hv000", "hv001", "hv002","hv027") )

hh <- extract_dhs(vars_households, add_geo = F) # returns a list of data.frames
hh <- as.data.table(hh[[1]]) # take 1st (and only) element (guard against issues if new versions)

# how many should be EXCLUDED
# dtb_dhs[paste0(v000, v001, v002) %in%  
#       hh[hv027==0,paste0(hv000, hv001, hv002 )], .N]
# should be approx. 0.5 of :    dtb_dhs[v000=="KE6", .N]  

# keep those not in list of short questionnaires
dtb_dhs <- dtb_dhs[! paste0(v000, v001, v002) %in%  
                     hh[hv027==0, paste0(hv000, hv001, hv002 )], ]

rm(vars_households) 
rm(hh)


######################################################################
#                      SPECIAL CASE INDIAN ZONES                     #
######################################################################

# dtb_dhs[DHS_CountryCode_mod=="IA", table(v024, useNA = 'a')]
# dtb_dhs[DHS_CountryCode_mod=="IA",  table(gsub("^\\[[a-z]{2}\\]" , "", v024), useNA = 'a')]

dtb_dhs[, `:=`(SurveyId_mod=SurveyId, 
               DHS_CountryCode_mod = DHS_CountryCode)]

dtb_dhs[DHS_CountryCode=="IA", v024 := gsub("^\\[[a-z]*\\]" , "", v024) ]
dtb_dhs[DHS_CountryCode=="IA", v024 := trimws(v024)]

dtb_dhs[Indian_states, on=.(v024), DHS_CountryCode_mod := region]

# in case of undue modification (Punjab region of Pakistan)
dtb_dhs[DHS_CountryCode!="IA", DHS_CountryCode_mod  := DHS_CountryCode]

dtb_dhs[DHS_CountryCode=="IA", SurveyId_mod:= paste0(DHS_CountryCode_mod, SurveyYear, "DHS") ]

datasets_DHS <- unique(dtb_dhs[,.(SurveyId, 
                                  SurveyId_mod, 
                                  DHS_CountryCode_mod)])

# retrieve all info stored in table datasets
datasets_DHS <- merge(datasets_DHS, datasets, by = c("SurveyId"))

setnames(datasets_DHS, c("SurveyId", "DHS_CountryCode"), c("SurveyId_original", "DHS_CountryCode_original"))
setnames(dtb_dhs, c("SurveyId", "DHS_CountryCode"), c("SurveyId_original", "DHS_CountryCode_original"))

# remove now useless table
rm(datasets)

# don't bother with useless variables
datasets_DHS[,`:=`(FileFormat=NULL, DatasetType=NULL, 
                   FileType=NULL, FileDateLastModified=NULL, 
                   FileSize=NULL, SurveyNum=NULL)]

