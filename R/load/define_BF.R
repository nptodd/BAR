######################################################################
#                               CURATION                             #
######################################################################


########### 1- Variable m4 (breastfeeding)


# see m4 values but avoid uninteresting numeric values (otherwise too long table)
dtb_dhs[! m4 %in% as.character(0:60), table(m4, useNA = "a")]

dtb_dhs[,table(m27, useNA = "a")] # flag for inconsistent bf duration

# question on duration of breastfeeding is asked condiitonal on "still bf ?" question answered no
# the only important thing is that this inconsistent cases will be at still_bf = F
dtb_dhs[m27 %in% c("> age at death", 
                   "> interval",
                   "> interval by 1 mnth",  # we could assume (trussell 1992) weaned in current month. 
                   "> interval by 1 month",
                   "during preg in cal."),
        m4:="inconsistent"]

dtb_dhs[! m4 %in% as.character(0:60), table(m4, useNA = "a")]


# abnormal duration of bf still present. All Indian dhs 7 (for which there m27=="no flag" for all)
dtb_dhs[m4=="83", .(age_interview, b5, m27, v000)]
dtb_dhs[m4=="83", m4:="ever breastfed, not currently breastfeeding"]


dtb_dhs[is.na(m4), table(SurveyId_original)]

# for the studies with very very few cases. Should be "missing"
dtb_dhs[is.na(m4) & 
          SurveyId_original %in% c("BR1991DHS", "DR1996DHS", "EG1988DHS", 
                                   "MZ2003DHS", "PH2008DHS", "SN1997DHS", 
                                   "UG1988DHS", "UG1995DHS", "ZM1996DHS"), 
        m4:="missing"]

dtb_dhs[is.na(m4), table(SurveyId_original)]

# other NA values are due to universe issues for BF questions : NA stems from question not asked. 
# For instance (see https://www.idhsdata.org/idhs-action/variables/BRSFEDMO#universe_section) :
# India 2015: Last-born children born and children who were never breastfed, born in the 5 years before the survey to women age 15-49.
# Bangl. 2011: Last-born children born in the 5 years before the survey to ever-married women age 12-49.


dtb_dhs[SurveyId_original == "BD2011DHS" & is.na(m4), table(bidx, b5)] # dead or 2+ bidx
dtb_dhs[SurveyId_original == "PG2017DHS" & is.na(m4), table(bidx, b5)] # dead or 2+ bidx
dtb_dhs[SurveyId_original == "CO2005DHS" & is.na(m4), table(bidx)] # all 2+ bidx
dtb_dhs[SurveyId_original == "IA2015DHS" & is.na(m4), table(bidx)] # all 2+ bidx
dtb_dhs[SurveyId_original == "RW2005DHS" & is.na(m4), table(b5)] # all dead
dtb_dhs[SurveyId_original == "TR2013DHS" & is.na(m4), table(bidx)] # all 2+ bidx
dtb_dhs[SurveyId_original == "ZA2016DHS" & is.na(m4), table(bidx, b5)] # all dead or 2+ bidx
dtb_dhs[SurveyId_original == "ZM2013DHS" & is.na(m4), table(b5)] # all dead


# special case : Bangladesh 2014. Universe is :
#     last-born child. born in the 3 yrs before the survey to ev.-married women 15-49.
# for children born >= 3yrs before the survey we can't decide on the bf status
dtb_dhs[SurveyId_original == "BD2014DHS" & is.na(m4) & bidx==1, table(b5, below_36=age_interview<36)] # either dead or above 3 years or 2+ bidx
dtb_dhs[SurveyId_original == "BD2014DHS"  & bidx==1 & age_interview >= 36, table(m4, useNA = "a")]

# special case : Bangladesh 2017. Universe is :
#     last-born child. born in the 3 yrs before the survey to ev.-married women 15-49.
# for children born > 3yrs before the survey we can't decide on the bf status
dtb_dhs[SurveyId_original == "BD2017DHS" & is.na(m4) & bidx==1, table(b5, below_36=age_interview<36)] # either dead or above 3 years or 2+ bidx
dtb_dhs[SurveyId_original == "BD2017DHS"  & bidx==1 & age_interview >= 36, table(m4, useNA = "a")]


# Except for those alive last born kids > 36 for Bangl. 2014 & 2017, though we can't decide on the
# value of m4, we know that it is certain / extremely likely that still_bf is false
# Conclusion : we set !is.na(m4) as a condition for still_bf, except for Bangl. 2014

# For ever BF : inconsistent cases classified as ever bf, but missing cases as never BF


########### 2- Variable m6 (amenorrhea)

dtb_dhs[, table(m6, useNA = "a")]

# check value of maternal level variable
# dtb_dhs[bidx==1, table(m6, v405, useNA = 'a')]

dtb_dhs[is.na(m6), table(SurveyId_original)]
# dtb_dhs[is.na(m6),table(paste(country_name, survey_year))]

dtb_dhs[is.na(m6) & SurveyId_original %in% c("BR1991DHS", "DR1996DHS", "MW1992DHS", 
                                             "MZ2003DHS", "PH2003DHS", "ZM1996DHS"), 
        m6:="missing"]

dtb_dhs[is.na(m6) & SurveyId_original == "ML2006DHS",table( bidx, useNA = "a")] # all 2+ bidx
dtb_dhs[is.na(m6) & SurveyId_original == "TL2016DHS",table( bidx, useNA = "a")] # all 2+ bidx

dtb_dhs[is.na(m6) & SurveyId_original == "BD2014DHS",
        table(above_36=age_interview>35, useNA = "a")] # all > 35months
dtb_dhs[is.na(m6) & SurveyId_original == "BD2017DHS",
        table(above_36=age_interview>35, useNA = "a")] # all > 35months



######################################################################
#                           DEFINE VARIABLES                         #
######################################################################

# check (bidx==2 & b0=="1st of multiple") means twin of the bidx==1 birth (last birth for sure)
# for each woman, compute age of youngest child
age_loc <- dtb_dhs[,.(age_min=min(age_interview)), by=.(SurveyId_original, caseid)]
# for each birth, compute age of youngest sibling
dtb_dhs[age_loc, on=.(SurveyId_original, caseid), min_age_sib:=age_min]
rm(age_loc)
# define variable latest_birth : "was part of latest birth ?"
dtb_dhs[, latest_birth:=(age_interview==min_age_sib)]
dtb_dhs[, min_age_sib:=NULL]
# check
# dtb_dhs[, .(prop_young=mean(latest_birth)), keyby=.(bidx, b0)]
# dtb_dhs[(latest_birth), table(bidx)]


dtb_dhs[, `:=`(still_bf_any = (!is.na(m4) & b5=="yes" & m4=="still breastfeeding"),
               
               ever_bf  = !(is.na(m4) | (m4 %in% c("never breastfed", "missing"))),
               
               # computed to replicate guide to DHS stats estimates
               still_ameno_guide = (!is.na(m6) & m6=="period not returned"))  ]

dtb_dhs[, still_ameno_no_preg := still_ameno_guide & latest_birth & cur_preg!="yes"] # if later conception (birth or just preg.), ovulation returned!!!

# final status adopted : we assume a woman's periods can't 
# have returned in first month 
dtb_dhs[, still_ameno := still_ameno_no_preg]
dtb_dhs[age_interview<1, still_ameno:=TRUE]

dtb_dhs[SurveyId_original %in% c("BD2014DHS", "BD2017DHS") & age_interview > 35, 
        `:=`(still_bf_any        = NA,
             ever_bf             = NA,
             still_ameno_guide   = NA,
             still_ameno_no_preg = NA,
             still_ameno         = NA)]


######################################################################
#                  DEFINE OTHER BREASTFEEDING STATUS                 #
######################################################################

# !!!! in some surveys questions on complementary food are 
# !!!! not asked for kids 24 months and older (will thus be 0)
# !!!! We then assume still_bf_water is approximated by still_bf_any
# !!!! still_bf_exclu == T is impossible

dtb_dhs[, still_bf_water := (still_bf_any & (CFS_milk + CFS_oliq + CFS_other==0)) ]

dtb_dhs[, still_bf_exclu := (age_interview < 24 & still_bf_water & CFS_water == 0 ) ]

# correct BD2014DHS
dtb_dhs[is.na(still_bf_any), `:=`(still_bf_water=NA, 
                                  still_bf_exclu=NA)]

# correct countries without food variables
no_CF_surveys <- datasets_DHS[N_food_all==0, SurveyId_mod]
dtb_dhs[SurveyId_mod %in% no_CF_surveys, `:=` (still_bf_water = NA,
                                               still_bf_exclu  = NA)]

rm(no_CF_surveys)

