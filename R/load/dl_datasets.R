######################################################################
#                   DOWNLOAD AVAILABLE DATA FILES (KIDS)             #
######################################################################

############ Get flat files 
datasets_main <- dhs_datasets(surveyType = "DHS",
                              fileFormat = "FL",    
                              fileType = "KR")

# remove Indian states from the list (not approved for download)
datasets_main <- datasets_main[!(DHS_CountryCode=="IA" & substr(FileName, 1, 2)!="IA")]


############ Get stata versions of error-generating flat files
error_gen_dhs <- c("BR1991DHS", "DR1996DHS", "MZ2003DHS", "ZM1996DHS")
datasets_main <- datasets_main[! SurveyId %in% error_gen_dhs]
datasets_compl <- dhs_datasets(surveyType = "DHS",
                               surveyIds = error_gen_dhs,
                               fileFormat = "DT",    
                               fileType = "KR")

datasets <- rbindlist(list(datasets_main, datasets_compl))

downloads <-  get_datasets(datasets, reformat = REFORMAT)

rm(list = c("datasets_main", "datasets_compl", "error_gen_dhs"))


######################################################################
#                DOWNLOAD AVAILABLE DATA FILES (WOMEN)               #
######################################################################

## Downloaded mainly to get the documentation

datasets_women <- dhs_datasets(surveyType = "DHS",
                               fileFormat = "DT", 
                               fileType = "IR")

datasets_women <- datasets_women[!(CountryName=="India" & 
                                     substr(FileName, 1, 2)!="IA")]

get_datasets(datasets_women, 
             download_option="zip",
             output_dir_root=paste0(path_to_data, "/datasets_women"))


######################################################################
#             DOWNLOAD AVAILABLE DATA FILES (HOUSEHOLDS)             #
######################################################################

#### Kenya 2014 

# Variable HV027 is needed to restrict to the long woman's questionnaires subsample

# Final report p. 6 :
# Thus,  a  total  of  five  questionnaires  were  used  in  the  2014  KDHS: 
# (1)  a  full  Household  Questionnaire,  
# (2)  a  short  Household  Questionnaire,  
# (3)  a  full  Woman’s  Questionnaire, 
# (4)  a  short Woman’s Questionnaire, and 
# (5) a Man’s Questionnaire. 
# The 2014 KDHS sample was divided into halves. 
# In   one   half,   households   were   administered   the   full   Household   Questionnaire,   the   full   Woman’s   
# Questionnaire,  and  the  Man’s  Questionnaire.  In  the  other  half,  households  were  administered  the  short  
# Household Questionnaire and the short Woman’s Questionnaire. Selection of these subsamples was done 
# at  the  household  level—within  a  cluster,  one  in  every  two  households  was  selected  for  the  full  
# questionnaires, and the remaining households were selected for the short questionnaires. 


datasets_households <- dhs_datasets(surveyIds = "KE2014DHS",
                                    fileFormat = "DT", 
                                    fileType = "HR")

get_datasets(datasets_households, 
             download_option="rds",
             output_dir_root=paste0(path_to_data, "/datasets_households"))

