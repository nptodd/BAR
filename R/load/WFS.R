#######################################################
#######################################################
############### World Fertility Surveys ###############
#######################################################

#######################################################
#        create the equivalent of datasets_DHS        #
#######################################################

datasets_WFS <- setDT( readxl::read_xlsx("../data/WFS/all_surveys.xlsx",
                                         sheet = 1) )
datasets_WFS[,`:=`(Notes=NULL,
                   amenorrhea= NULL, 
                   amenorrhea_additionnal=NULL,
                   amenorrhea_available = NULL)]

datasets_WFS <- datasets_WFS[!is.na(FileName)] # remove section titles
datasets_WFS[, DHS_CountryCode_original:=toupper(substr(FileName, 1,2))]

datasets_WFS[, `:=`(DHS_CountryCode_mod=DHS_CountryCode_original,
                    SurveyType="WFS",
                    bany_avail=T,
                    ameno_avail=T,
                    N_food=0,
                    N_food_all=0) ]
datasets_WFS[, SurveyId_original := paste0(DHS_CountryCode_mod, SurveyYear, "WFS")]
datasets_WFS[, SurveyId_mod := SurveyId_original]


#######################################################
#                     load files                      #
#######################################################

dtb_wfs_l <- lapply(list.files("../data/WFS/reshaped/", full.names = T), 
                    readstata13::read.dta13, 
                    convert.factors=T, generate.factors=T,
                    select.cols=c("v001", "v003", "v004", "v006", "v007", "v008", 
                                  "v206", # pregnant
                                  "v301", "v302", # breastfeeding last births
                                  "v702", # type of place of residence (urban/rural)
                                  
                                  "v705",  # literacy
                                  "v706", "v707", # religion and ethnic group
                                  
                                  "birth", "b1", "b2", "b3", 
                                  "f121", "f122", "s303",'s402',
                                 
                                  "s714", "s715"
                                  ))

names(dtb_wfs_l) <- gsub(pattern = ".dta", replacement =  "", 
                         x = list.files("../data/WFS/reshaped/"), fixed = T)

dtb_wfs <- rbindlist(dtb_wfs_l, use.names = T, fill = T, idcol="survey")


# restrict datasets_WFS to dtb_wfs effectively used
datasets_WFS = datasets_WFS[FileName %in% names(dtb_wfs_l)]



######## Convert variables to appropriate type ########

# change birth order variable's name to match DHS
setnames(dtb_wfs, "birth", "bord")
# dtb_wfs[, table(bord)]
dtb_wfs[, bord := gsub(pattern = "[[:alpha:]]", replacement = "", bord)]
dtb_wfs[, bord := as.numeric(bord)]


# set type of all variables to character
dtb_wfs <- dtb_wfs[, lapply(.SD, as.character)]

dtb_wfs[, sample_weight := as.numeric(v006)]  # /1e6 ?

setnames(dtb_wfs, c("v003", "v004"), c("stratum", "psu"))

setnames(dtb_wfs, c("v705", "v706", "v707"), c("literacy", "religion", "ethnicity"))


############# Add survey-level var. needed ############

dtb_wfs[datasets_WFS, on=c(survey="FileName"), 
        `:=`(SurveyId_original = SurveyId_original, 
             SurveyId_mod = SurveyId_mod, 
             DHS_CountryCode_original=DHS_CountryCode_original, 
             DHS_CountryCode_mod=DHS_CountryCode_mod, 
             SurveyYear=SurveyYear)]


################# Curate key variables ################


############### Currently pregnant

dtb_wfs[,cur_preg:=v206]

dtb_wfs[grepl("^no", v206, ignore.case = T),  cur_preg:="no or unsure"]
dtb_wfs[grepl("^(yes)|(oui)", v206, ignore.case = T),  cur_preg:="yes"]

dtb_wfs[,table(cur_preg, useNA = 'a')]



############### Type of place of residence
dtb_wfs[,residence:=v702]

residence_groups=list( urban=c("Abidjan","City", "Kigali", "Maseru", 
                               "Nairobi or Mombasa", "Town", "Tunis et banlieue"),
                       rural=c("Countryside", "Village"),
                       unknown=c("Non declare", "Not stated") ) 

dtb_wfs[grepl("(urba(i)?n)|(ville)", v702, ignore.case = T),   residence:="urban"]
dtb_wfs[grepl("rural", v702, ignore.case = T),   residence:="rural"]

dtb_wfs[residence %in% residence_groups$urban,   residence:="urban"]
dtb_wfs[residence %in% residence_groups$rural,   residence:="rural"]
dtb_wfs[residence %in% residence_groups$unknown, residence:="rural"] # so few cases, doesn't matter

dtb_wfs[, table(residence, useNA = 'a')] # check all cases have been treated
rm(residence_groups)


############### Date of interview
dtb_wfs[, table(v007, useNA = 'a')]
dtb_wfs[, to_treat := is.na(as.numeric(v007))]
dtb_wfs[(to_treat), table(v007, useNA = 'a')]
dtb_wfs[, `:=`(year_int=NA_integer_, month_int=NA_integer_)]

### year
dtb_wfs[(to_treat), year_int:=as.numeric(substrRight(v007, 4))]
dtb_wfs[,table(year_int, useNA = 'a')]
### month
dtb_wfs[to_treat & grepl("Jan", v007), month_int:=1]
dtb_wfs[to_treat & grepl("Feb", v007), month_int:=2]
dtb_wfs[to_treat & grepl("Mar", v007), month_int:=3]
dtb_wfs[to_treat & grepl("Apr", v007), month_int:=4]
dtb_wfs[to_treat & grepl("May", v007), month_int:=5]
dtb_wfs[to_treat & grepl("Jun", v007), month_int:=6]
dtb_wfs[to_treat & grepl("Jul", v007), month_int:=7]
dtb_wfs[to_treat & grepl("Aug", v007), month_int:=8]
dtb_wfs[to_treat & grepl("Sep", v007), month_int:=9]
dtb_wfs[to_treat & grepl("Oct", v007), month_int:=10]
dtb_wfs[to_treat & grepl("Nov", v007), month_int:=11]
dtb_wfs[to_treat & grepl("Dec", v007), month_int:=12]
dtb_wfs[(to_treat), table(month_int, useNA = 'a')]

dtb_wfs[!(to_treat),  date_interview := as.numeric(v007)]
dtb_wfs[(to_treat), date_interview := 12*(year_int-1900) + month_int]
# check
# dtb_wfs[(to_treat), mean(date_interview)]
# dtb_wfs[(!to_treat), mean(date_interview)]


############### Date of birth (cmc)
dtb_wfs[,table(b2, useNA = 'a')]
# dtb_wfs[is.na(b2),table(survey)]
dtb_wfs[b2 %in% c("9999", "Not stated"), b2:=NA]
# dtb_wfs[b2 %in% c("1", "2", "7"),table(survey)]
dtb_wfs[b2 %in% c("1", "2", "7"), b2:= NA]
dtb_wfs[, b2:=as.numeric(b2)]

############### Age at interview
dtb_wfs[,age_interview := (date_interview - b2)]
dtb_wfs[age_interview==0, age_interview := 1/3] # see Trussell et al. 1992
dtb_wfs[is.na(age_interview), table(survey)]
dtb_wfs[age_interview>=12*5, table(survey)]


# remove missing dates and children  > age 5
dtb_wfs <- dtb_wfs[!is.na(age_interview) & age_interview<12*5]

############### maternal date of birth, cmc
dtb_wfs[, v008:=as.numeric(v008)] 

############### maternal age at birth of the child, in years
dtb_wfs[, mab:=round((b2- v008)/12)] 
# dtb_wfs[, table(mab, useNA = 'a')] # check



############### Reverse birth order
# for each woman, get age of latest_birth child
Date_last_birth_per_woman = dtb_wfs[,.(D=min(age_interview)), by=.(survey, v001)]
# reimport this age of latest_birth child
dtb_wfs[Date_last_birth_per_woman, on=.(survey, v001), DLB := D]
# who is the last child ?
dtb_wfs[, latest_birth := abs(age_interview-DLB)<1e-3]
rm(Date_last_birth_per_woman)


############## amenorrhea status
dtb_wfs[,still_ameno_guide:=NA]
### f121
dtb_wfs[(grepl("^No", f121)), f121:="no"]
dtb_wfs[f121 %in% c("Yes", "Oui"), f121:="yes"]
dtb_wfs[,table(f121, useNA = 'a')]
dtb_wfs[f121 == "yes", still_ameno_guide:=TRUE]
dtb_wfs[f121 == "no",  still_ameno_guide:=FALSE]
### s303
dtb_wfs[survey=="egsr03",table(s303, useNA = 'a')]
dtb_wfs[survey=="egsr03" & s303=="Have not started", still_ameno_guide:=TRUE]
dtb_wfs[survey=="egsr03" & s303!="Have not started", still_ameno_guide:=FALSE]
### s402
dtb_wfs[survey=="bdsr03",table(s402, useNA = 'a')]
dtb_wfs[survey=="bdsr03" & s402=="Still amenorrhoea", still_ameno_guide:=TRUE]
dtb_wfs[survey=="bdsr03" & s402!="Still amenorrhoea", still_ameno_guide:=FALSE]

dtb_wfs[,table(still_ameno_guide, useNA = 'a')]

# for non last children, still_ameno_guide is false
dtb_wfs[!(latest_birth), still_ameno_guide := FALSE]

# correct residual missing cases
dtb_wfs[is.na(still_ameno_guide), still_ameno_guide:=FALSE]

# if later conception (birth or just preg.), ovulation returned!!!
dtb_wfs[, still_ameno_no_preg := still_ameno_guide & latest_birth & cur_preg!="yes"] 

# final status adopted : we assume a woman's periods can't have returned in first month (lochia)
dtb_wfs[, still_ameno:=ifelse(age_interview<1, 1, still_ameno_no_preg)]

# dtb_wfs[still_ameno_no_preg!=still_ameno, table(age_interview)]


############## breastfeeding status

bf_groups = list(
  not_bf = c("Did not breastfeed", "DID NOT BREASTFEED",
             "n'a pas allaite", "N'a pas allaite", "Not bfed,not alive"),
  bf_died = c("jusqu'a mort enfant", "Jusqu'au deces",
              "Until child died", "UNTIL CHILD DIED"),
  no_data = c("No data", "Non declare", "Non reponse", "Not stated",
              "Pas d'information", "pas des donnees", 
              "non declaree", "NOT STATED",
              "87", "97"),
  still_bf_any = c("Allaite encore", "allaite encore", 
                   "Still breastfeeding", "STILL BREASTFEEDING", "96")
)
### For open birth interval
dtb_wfs[v301%in% bf_groups$not_bf,  v301:="not breastfed"]
dtb_wfs[v301%in% bf_groups$bf_died, v301:="until child died"]
dtb_wfs[v301%in% bf_groups$no_data, v301:="no data"]
dtb_wfs[v301%in% bf_groups$still_bf_any, v301:="still breastfeeding"]
# dtb_wfs[, table(v301, useNA = 'a')]
# dtb_wfs[, unique(v301)]
# dtb_wfs[, table(as.numeric(v301))]

# check inconsistent cases with bf > age_interview
dtb_wfs[latest_birth & as.numeric(v301)>age_interview, unique(paste(v301, age_interview))]
# treatment of inconsistent cases. See Trussell et al. 1992
dtb_wfs[latest_birth & as.numeric(v301)>age_interview, v301:="still breastfeeding"]

### We must also treat last closed interval for countries where if
### woman is pregnant again the birth interval is considered closed
dtb_wfs[v302 %in% bf_groups$not_bf,   v302:="not breastfed"]
dtb_wfs[v302 %in% bf_groups$bf_died,  v302:="until child died"]
dtb_wfs[v302 %in% bf_groups$no_data,  v302:="no data"]
dtb_wfs[v302 %in% bf_groups$still_bf_any, v302:="still breastfeeding"]
# dtb_wfs[, table(v302, useNA = 'a')]

rm(bf_groups)


dtb_wfs[, still_bf_any:=NA]
dtb_wfs[v301 == "still breastfeeding", still_bf_any := TRUE]
dtb_wfs[v301 != "still breastfeeding", still_bf_any := FALSE]
dtb_wfs[, table(still_bf_any, useNA = 'a')]



# if still_bf_any (necessarily latest_birth) it is most usually because 
# current pregnancy (if any) is considered as closing the interval 
dtb_wfs[is.na(still_bf_any), table(v206)]
dtb_wfs[is.na(still_bf_any) & v206 %in% c("Yes", "Oui", "oui", "YES"), 
        still_bf_any:= (v302 == "still breastfeeding")]

# few cases left : considered not bf
dtb_wfs[is.na(still_bf_any), still_bf_any:=FALSE]
# for non last children, still_bf_any is false
dtb_wfs[!(latest_birth), still_bf_any := FALSE]


# plot(dt[survey %in% c("phsr05"), 
#         mean(still_bf_any, na.rm=T), 
#         keyby=.(age_interview)])


########## Hormonal contraception
### assumed 0 for all dtb_wfs. 
dtb_wfs[,hormonal_contracep_cur:=F]
########## Current contraception
### assumed 0 for all dtb_wfs. Only needed for sterilization
### so even if folklore methods used in WFS, no worries
dtb_wfs[,contracep_cur:="not using"]

