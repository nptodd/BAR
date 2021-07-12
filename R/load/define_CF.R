######################################################################
#         DEFINE LIST OF COMPLEMENTARY FOOD VARIABLES NEEDED         #
######################################################################

# complementary food eaten ***in the last 24 hrs***


############# 1- VARIABLES IN THE RECODE FILES MANUALS

#### Complementary food variables given in last 24hrs that are part of 
### the Recode Files Manuals and Guide to DHS Statistics 2018 edition

CF_RFM <- c("m39", "m39a", 
            
            apply(MARGIN=1, FUN=paste, collapse="",
                  X=expand.grid("v", 409:414, c("", letters)) ),
            
            paste0("m37", c(letters,"xx", "xy", "xz") ),
            
            paste0("v469", c(letters,"xx", "xy", "xz") )  ) 



# restrict CF_RFM to variables that actually exist
CF_ex <- unique(search_variables(datasets, CF_RFM, reformat = REFORMAT)$variable)
CF_RFM <- CF_RFM[ CF_RFM %in%  CF_ex]
rm(CF_ex)


# get number of variables of CF_RFM found AND USED in each survey
N_var_f <- function(filename_a, food_v){
  
  name_loc <- gsub("\\.(ZIP|zip)", "", filename_a)
  print(name_loc)
  B <- readRDS(downloads[[name_loc]])
  N_var <- 0
  
  for(var_loc in intersect(food_v, names(B))){
    N_var = N_var + (length(unique(B[,var_loc]))>1)
  }
  cat(filename_a, ": ", N_var, "\n")
  return(N_var)
}


# CL = makeCluster(40)
CL = makeCluster(3)
clusterExport(cl=CL, "downloads")
# stored externally to avoid confusion on variables' names
N_var_v <- parSapplyLB(cl=CL, 
                       X = datasets[,FileName], 
                       FUN = N_var_f, 
                       food_v=CF_RFM)
datasets[, N_food := N_var_v]
rm(N_var_v) 
# do the same for breastfeeding
breast_v <- parSapplyLB(cl=CL, 
                       X = datasets[,FileName], 
                       FUN = N_var_f, 
                       food_v="m4")
datasets[, bany_avail := (breast_v>0)]
rm(breast_v) 
# do the same for amenorrhea
ameno_v <- parSapplyLB(cl=CL, 
                        X = datasets[,FileName], 
                        FUN = N_var_f, 
                        food_v="m6")
datasets[, ameno_avail := (ameno_v>0)]
rm(ameno_v) 
stopCluster(CL)



############# 2- CF VARIABLES NOT IN THE RECODE FILES MANUALS

##### A- Search candidates

# # candidates will be searched only in surveys with less than 6 complementary food variables
# # Why 6 ? in dhs 1 individual recode manual, 6 variables to assess exclusive bf status (v409 to v414)
# 
# query_c<- paste0("(food)|(solid)|(yesterday)|(last day)|(formula)|(^baby:)",
#                  "|(child drank)|(child ate)",
#                  "|(giving the child)|(times drank)|(times ate)|(24 h)")
# 
# SV <- search_variable_labels(datasets[N_food < 6], query_c, reformat = REFORMAT)
# setDT(SV);setkey(SV, survey_id, variable);rm(query_c)
# setcolorder(SV, c( "survey_id", "dataset_filename", "variable"))
# 
# # remove uninteresting variables
# SV[,description:=tolower(description)]
# SV <- SV[substr(variable, 1, 1)=="s"]
# SV <- SV[ !grepl("(soap)|(7 days)|(alcohol)|(^age )|(^mother)|(husband)|(household)", description)]
# SV <- SV[ !grepl("(smok)|(hepatitis)|( tb)|(tb spread)|(diarr)|(punish)|(anemia)", description)]
# SV <- SV[ !grepl("(precautions)|(serving)|(added to)|(added sug)|(doctor)|(prepar)|(aids)", description)]
# SV <- SV[ !grepl("(more,less)|(months)", description)]
# # SV[, table(survey_id)]
# SV[, .(unique(survey_id))][,.N]
# # View(SV)


##### B- Check specific studies

datasets[N_food < 6, 
         .(SurveyId, FileName, CountryName, SurveyYear, N_food)]

# + manually check questionnaire in the final report (dhsprogram.com)
# + manually check doc file in women recode folder

# FLN <- "IDKR01FL"
# A <- search_variable_labels(dataset_filenames = FLN,
#                             search_terms = "",
#                             reformat = REFORMAT)
# setDT(A) ;  
# # View(A[substr(variable, 1, 1)=="s"])
# B <- readRDS(downloads[[FLN]])
# head(B[,paste0("s415", letters[1:6])] )
# rm(FLN)


##### C- List selected variables 

CF_ADD = list(AM2005 = c(paste0("s469", letters[1:7]), 
                         paste0("s470c", letters[1:17]),
                         "s472"),
              
              BD2004 = c("s447a", 
                         paste0("s449", letters[1:10], "1")),
              
              TD2004 = paste0("s449", letters[1:14]),
              
              # CO2015DHS = , # NO ADDITIONAL VARIABLES FOUND
              
              CG2005 = c("s452", 
                         paste0("s492b_", letters[1:7]),
                         paste0("s493b_", letters[1:11])),
              
              # EG1988 : some important variables seem to be missing
              # the StatCompiler doesn't provide an estimate. Study left aside
              # EG1988 = c("s723", "s725", "s728", "s729", "s730", "s731", "s733", # "age at" questions
              #            paste0("s724", letters[1:6]), # use all variables just in case
              #            paste0("s726", letters[1:5])),
              
              EG2005 = c(paste0("s1203", letters[1:4]),
                         paste0("s1204a_0", 1:9),
                         paste0("s1204a_", 10:21),
                         "s1206"),
              
              ET2005 = c(paste0("s469", letters[1:6]), 
                         paste0("s470", letters[1:19]), 
                         "s472"),
              
              # GU1987 = , # NO ADDITIONAL VARIABLES FOUND
              
              ID1987 = paste0("s415", letters[1:6]),
              
              # JO2009 = , # NO ADDITIONAL VARIABLES FOUND
              
              MB2005 = c(paste0("s567", letters[1:5]),
                         paste0("s568c", letters[1:21]),
                         "s570"),
              
              PK2006 = paste0("s498", letters[1:4]),
              
              # same for PH2013 as for EG1988
              # PH2013 = c("s460"), 
              PH2017 = c("s467b"),
              
              # RW2008 = , # NO ADDITIONAL VARIABLES FOUND
              
              TG1988 = paste0("rc416", c("c", "d", "e", "g")),
              
              TR2003 = paste0("s435", letters[1:14]),
              TR2008 = paste0("s472", letters[1:13]),
              TR2013 = paste0("s449", letters[1:17]), # !!! not in KR files (either flat or stata)
              
              UG1988 = c("s415e"))


# N_food_all is N_food updated with new food variables
# Update only needed for datasets mentionned in CF_ADD
datasets[, N_food_all := N_food]

for(I in names(CF_ADD)){
  
  food_loc <- c(CF_RFM, CF_ADD[[I]])
  
  N_var_loc <-  N_var_f(datasets[SurveyId==paste0(I, "DHS"), FileName], food_loc)
  
  datasets[SurveyId==paste0(I, "DHS"), N_food_all := N_var_loc]
  
  rm(list=c("food_loc", "N_var_loc"))
}


# restrict to datasets with breastfeeding variable available
datasets <- datasets[(bany_avail & ameno_avail)]


# vector of complementary food variables
CF <- c(CF_RFM, unique(unlist(CF_ADD) ))

