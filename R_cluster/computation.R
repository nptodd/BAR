
# get info on array (number of jobs, which job we're in)
chunk_number <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
n_cores <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_COUNT") )

cat("..... Job ", chunk_number, " out of ", n_cores, " jobs in array\n\n\n\n\n")


library(data.table)
library(survey)
library(scam)

options(survey.lonely.psu="adjust")

# functions needed
source("../R/misc_functions.R")
source("../R/load/BPF_f.R")
source("summarize_surv_f.R")
source("../R/bf/estimate_f.R")


# retrieve levels of energy1, energy2 etc. Time lost but code made much more shorter  
dtb <- fread(file = "../data/final_data/dtb.csv",
             na.strings = "",
             nThread = 1, nrows = 7e5)
dtb <- dtb[, .SD, .SDcols = names(dtb) %like% "^energ"]
dtb <- unique(dtb)
gc()

datasets <- fread(file = "../data/final_data/datasets.csv")
# datasets[, which_core:=.I %% n_cores+1 ] # old: not handling Indian very large surveys

datasets[,indian_recent:=(DHS_CountryCode_original=="IA" & SurveyYear>2010)]
n_cores_india <- datasets[(indian_recent),.N]
n_cores_other <- n_cores - n_cores_india
datasets[, which_core:=.I %% n_cores_other+1 ]
datasets[(indian_recent), which_core:= (n_cores_other + 1:n_cores_india)]

# # checks
# datasets[!(indian_recent), table(which_core)]
# datasets[(indian_recent), .(which_core)]



local_datasets <- datasets[which_core == chunk_number]
# if recent India survey, then 75 replicates should be okay for bootstrap
# Otherwise 200
replicates_arg <- ifelse(any(local_datasets[, indian_recent]), 75, 200)



################################################################################
############################# COUNTRYWIDE ESTIMATES ############################

if(!dir.exists("../results_cluster/country")){
  dir.create("../results_cluster/country")}

COUNTRY_chunk <- lapply(FUN = summarize_surv_f,
                        X = local_datasets[, SurveyId_mod],
                        subset_a = "hormonal_contracep_cur==F",
                        datasets_a = datasets,
                        replicates_a = replicates_arg)

COUNTRY_chunk <- rbindlist(COUNTRY_chunk, use.names = T, fill=TRUE)

fwrite(COUNTRY_chunk,
       paste0("../results_cluster/country/COUNTRY_c", chunk_number,".csv"))

rm(COUNTRY_chunk)
cat("\n\n\n\n\n")



################################################################################
################################ BY ENERGY GROUP ###############################

cat("STARTING VARIABLE: ........ ", toupper("energy4_parity"), "........ \n")

path_results <- file.path("../results_cluster/energy4_parity")

if(!dir.exists(path_results)){dir.create(path_results)}

energy_groups = names(table(dtb[, energy4_parity]))

energy_groups <- expand.grid(SurveyId_mod_a=local_datasets[, SurveyId_mod],
                             energy_group=energy_groups,
                             stringsAsFactors = F)
setDT(energy_groups)
energy_groups[, subset_a := paste0("hormonal_contracep_cur==F & energy4_parity=='",
                                   energy_group, "'")]

ENERGY_chunk <- mapply(FUN = summarize_surv_f,
                       SurveyId_mod_a = energy_groups[, SurveyId_mod_a],
                       subset_a = energy_groups[, subset_a],
                       MoreArgs=list(datasets_a = datasets,
                                     replicates_a = replicates_arg),
                       SIMPLIFY = F )

ENERGY_chunk <- rbindlist(ENERGY_chunk,  use.names = T, fill=TRUE)

if(ENERGY_chunk[,.N] != energy_groups[,.N]){
  stop("INCORRECT NUMBER OF GROUPS !!!!!!!!!!!!")}

ENERGY_chunk[, `:=`(group = energy_groups$energy_group)]

setcolorder(ENERGY_chunk, c("SurveyId_mod", "group"))

fwrite(ENERGY_chunk,
       paste0(path_results, "/ENERGY_c", chunk_number,".csv"))

rm(ENERGY_chunk)

