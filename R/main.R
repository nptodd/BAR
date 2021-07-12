library(dplyr); library(parallel); library(lme4)
library(data.table); library(rdhs); library(survey)
library(scam) # also needed: readstata13

options(survey.lonely.psu="adjust")

source("load/define_design_f.R")
source("misc_functions.R")
source("load/indian_states.R")

REFORMAT <- T # use reformatted datasets ?

# folder to store data downloaded from the DHS website
path_to_data <- "../data/recodeFiles" 
dir.create(path_to_data, showWarnings = F)
if(file.exists("../data/recodeFiles/client_dhs.rds")){
  file.remove("../data/recodeFiles/client_dhs.rds" )} 

# folder to store final datasets
dir.create("../data/final_data/dtb_cntry/", showWarnings = F, recursive = T)

# folder to store figures
dir.create("../figs/", showWarnings = F)

# folder to store results
dir.create("../results/", showWarnings = F)


# # (re-)create rdhs.json (info for download using rdhs)
# # if file format has been changed, then script must be rerun
# source("rdhs_config.R")


######################################################################
##                     Load and curate datasets                     ##
######################################################################

# download Kids datasets and create "downloads" and "datasets" 
# download women (for doc) and necessary households (Kenya) datasets 
# + creates the datasets_women and datasets_households
source("load/dl_datasets.R")

# define list of CF variables needed to compute excl BF status.
# dataset is restricted to surveys for which breastfeeding and
# amenorrhea status is available.
source("load/define_CF.R")     

# extract variables from datasets + compute CF score 
# NB : SurveyId_mod = SurveyId_original, except for India. 
# SurveyId_mod defines studies for which estimations are needed
# defined datasets_mod and remove datasets (storing survey-level info)
source("load/extract_data.R")

# define still_bf, ever_bf, still_ameno, still_bf_exclu
source("load/define_BF.R")   

define_design_f(dtb_dhs)

# load WFS data
source("load/WFS.R")


######################################################################
#                          MERGE WFS AND DHS                         #
######################################################################

# merge table of survey-level info
datasets_WFS[, type:="WFS"]
datasets_DHS[, type:="DHS"]
datasets <- rbindlist(list(datasets_WFS, datasets_DHS), use.names = T)

# merge individual-level data
cols <- colnames(dtb_wfs)[colnames(dtb_wfs) %in% colnames(dtb_dhs)]
cols <- grep("^(v|b|m)[[:digit:]]+$", cols, value = T, invert = T) 

dtb <- rbindlist(list(dtb_wfs[, ..cols], dtb_dhs), fill = T)

rm(cols)


######################################################################
#                   ADD WORLD REGION TO datasets                     #
######################################################################

cntry_table <- dhs_countries()
cntry_table[,Region:=ifelse(RegionName %in% c("Central Asia","South & Southeast Asia","Oceania"),
                            "Asia-Pacific", RegionName)]
# cntry_table[, table(RegionName, Region)]

datasets[cntry_table, on=c(DHS_CountryCode_original="DHS_CountryCode"),
         `:=`(Region = Region, iso3 = ISO3_CountryCode)]
rm(cntry_table)

datasets[is.na(Region)]
# manually add data for Syria
datasets[DHS_CountryCode_mod=="SY", 
         `:=`(iso3="SYR", Region="North Africa/West Asia/Europe")]


######################################################################
##                            REGION TABLE                          ##
######################################################################

# associate to each World Region a given colour once and for all
region_table = data.table(Region=c("Asia-Pacific", 
                                   "Latin America & Caribbean", 
                                   "North Africa/West Asia/Europe",
                                   "Sub-Saharan Africa"),
                          Code=c("ASI", "LAC", "EUR", "SSA"),
                          Col=colorspace::rainbow_hcl(6, c=60, l=75)[c(5, 2, 6, 3)])


######################################################################
#                        DEFINE ENERGY STATUS                        #
######################################################################

# define parity group : 1 or (2 or 3) or 4+
dtb[, parity_g:= pmin(as.numeric(bord), 4)]
dtb[, parity_g:= gsub("3", "2", parity_g)]

dtb[,table(residence, useNA = 'a')]
dtb[,table(elec_g, useNA = 'a')]
dtb[,table(water30less_g, useNA = 'a')]
dtb[,table(educ_g, useNA = 'a')]
dtb[,table(parity_g, useNA = 'a')]

dtb[, energy4:= paste0(dplyr::recode(residence, urban="u", rural="r"), 
                       elec_g, water30less_g, educ_g)]

dtb[, energy4_parity:= paste0(energy4, parity_g )]

dtb[nchar(energy4)!=4 | grepl("NA", energy4), `:=`(energy4=NA, energy4_parity=NA)]

# # checks
dtb[,table(energy4, useNA = 'a')]
dtb[,table(energy4_parity, useNA = 'a')]


######################################################################
#                             SAVE TABLES                            #
######################################################################

datasets[ region_table, on=.(Region), `:=`(Region_code=Code, Region_col=Col)]
datasets[,table(Region_code, useNA = 'a')]

fwrite(dtb, file = "../data/final_data/dtb.csv")
fwrite(datasets, file = "../data/final_data/datasets.csv")

for(C in dtb[,unique(DHS_CountryCode_mod)]){
  fwrite(dtb[DHS_CountryCode_mod == C], 
         paste0("../data/final_data/dtb_cntry/", C,".csv")) }


######################################################################
#                              OTHER LOAD                            #
######################################################################

# load Bongaarts-Potter function (BPF) + its associated data
source("load/BPF_f.R")
source("load/BPF_data.R")

# load Human Development Index
source("load/development_index.R")


######################################################################
##                           Summary stats                          ##
######################################################################

gc()

datasets[,table(type)]
datasets[type=="DHS",length(unique(SurveyId_original))]
dtb[, range(SurveyYear)]
dtb[,.N]
dtb[,length(unique(paste(SurveyId_original, caseid) ))]
dtb[,length(unique(DHS_CountryCode_original))]
dtb[,length(unique(SurveyId_original))]
dtb[,length(unique(SurveyId_mod))]

dtb[,.(N_surveys=length(unique(SurveyId_original))), 
    keyby=.(DHS_CountryCode_original) ][N_surveys==max(N_surveys)]


# surveys where energy groups can be defined
dtb[,mean(!is.na(energy4_parity)&energy4_parity!=''), by=SurveyId_mod][V1>0,.N]


# country with questions for children born in the past 3 years only
# dtb[,max(age_interview) , by=SurveyId_original][V1<58]

# draw map (fig S1)
source("load/map_surveys.R")


######################################################################
##            FUNCTIONS FOR COMPUTATION OF MEAN DURATIONS           ##
######################################################################

# function to compute estimated durations (amenorrhea etc.)
source("bf/estimate_f.R")
# function to draw survival profiles
source("bf/plot_ctry_f.R")
# function to draw amenorrhea, breastfeeding and exc. breastf. for all
# surveys available for a given country
source("bf/ctry_prof_f.R")


######################################################################
##                   COMPUTATION OF MEAN DURATIONS                  ##
######################################################################

# example country profiles for ms
source("bf/example_profiles.R")

# draw systematic country profiles + data.table of results
source("bf/ctry_profiles.R")
gc()



######################################################################
#                      CHECK METHOD DIFFERENCES                      #
######################################################################

ESTIM_NO_HC_TRUSSELL <- merge(ESTIM_NO_HC_TRUSSELL, ESTIM_NO_HC, 
      by = c("SurveyId_mod", "SurveyYear", "DHS_CountryCode_mod"))

par(mfrow=c(1,3))
ESTIM_NO_HC_TRUSSELL[,plot(A.x, A.y, pch=20, col="dimgrey",
                           xlab="Old method (months)", ylab="New method (months)")]
abline(0, 1, lwd=2)
mtext("Amenorrhea", line=-2, font=2)
ESTIM_NO_HC_TRUSSELL[,plot(BANY.x, BANY.y, pch=20, col="dimgrey",
                           xlab="Old method (months)", ylab="New method (months)")]
mtext("Breastfeeding", line=-2, font=2)
abline(0, 1, lwd=2)
ESTIM_NO_HC_TRUSSELL[,plot(BEX.x, BEX.y, pch=20, col="dimgrey",
                           xlab="Old method (months)", ylab="New method (months)")]
mtext("Exclusive breastfeeding", line=-2, font=2)
abline(0, 1, lwd=2)


######################################################################
#                       DESCRIPTION OF RESULTS                       #
######################################################################

quick_summary = function(data_a, Region_code_a=NULL){
  
  if(is.null(Region_code_a)){
    Region_code_a=data_a[,unique(Region_code)] # all regions
  }
  
  res <-  data_a[Region_code %in% Region_code_a, 
                 .(median=round(median(DBP), 1), 
                   # SD=round(sd(DBP), 1),
                   IQR_l=round(quantile(DBP, 0.25), 1), 
                   IQR_u=round(quantile(DBP, 0.75), 1)
                 )]
  
  print(res, digits=2)
}

quick_summary(ESTIM_NO_HC)
quick_summary(ESTIM_NO_HC, "SSA")
quick_summary(ESTIM_NO_HC, "ASI")

ESTIM_NO_HC[Region_code =="SSA", mean(DBP>0)]
ESTIM_NO_HC[Region_code =="SSA" & DBP>0, .N]
ESTIM_NO_HC[Region_code =="SSA", .N]
ESTIM_NO_HC[Region_code =="ASI", mean(DBP>0)]
ESTIM_NO_HC[Region_code =="ASI" & DBP>0, .N]
ESTIM_NO_HC[Region_code =="ASI", .N]
ESTIM_NO_HC[Region_code =="LAC", mean(DBP>0)]
ESTIM_NO_HC[Region_code =="EUR", mean(DBP>0)]

ESTIM_NO_HC_rk1[, .(mean= mean(DBP)), by=Region_code]
ESTIM_NO_HC_MAB2030_rk23[, .(mean=mean(DBP), sd=sd(DBP)), by=Region_code]


static_plot_f <- function(estim_a, 
                          type_a = "mean", 
                          BPF_f_a=NULL,
                          BPF_pop_a=NULL,
                          add_leg_a=T,
                          ...){
  
  XLIM=c(0, 37) ;  YLIM=c(0,24)  
  
  estim_loc = copy(estim_a)
  
  if(type_a=="mean"){
    setnames(estim_loc, old=c("A", "BANY"), new=c("Aloc", "Bloc"))
  } else if (type_a=="median"){
    setnames(estim_loc, old=c("Amd", "BANYmd"), new=c("Aloc", "Bloc"))
  }
  
  if(estim_loc[,max(Bloc, na.rm = T)] > XLIM[2]){ XLIM[2] = estim_loc[, max(Bloc, na.rm = T)]+1 }
  if(estim_loc[,max(Aloc, na.rm = T)] > YLIM[2]){ YLIM[2] = estim_loc[, max(Aloc, na.rm = T)]+1 }
  
  plot(estim_loc[,.(Bloc, Aloc)], lwd=2,
       col = estim_loc[, Region_col], pch = 16, 
       xlim = XLIM, ylim = YLIM,
       xlab = paste0(Hmisc::capitalize(type_a)," duration of breastfeeding (months)"),
       ylab = paste0(Hmisc::capitalize(type_a)," duration of postpartum amenorrhea (months)"),
       axes=F, ...)
  
  axis(1, at=seq(0, 36, 6), tck=-0.03, cex.axis=1.2)
  axis(1, at=seq(0, 36), labels=rep("", 36+1), tck=-0.015) 
  axis(2, las=2, seq(YLIM[1], YLIM[2], 6), tck=0.03, cex.axis=1.2)
  axis(2, at=seq(YLIM[1], YLIM[2]), labels=rep("", YLIM[2]-YLIM[1]+1), tck=0.015) 
  
  legend_table <- unique(estim_loc[,.(Region, Region_col)])
  setkey(legend_table, Region)
  
  
  if(add_leg_a){
    legend(x = XLIM[1]+1, y=YLIM[2]-1, 
           legend = legend_table[,Region], 
           col = legend_table[,Region_col],
           pch=16, pt.cex=2, bty="n")
  }
  
  if(!is.null(BPF_f_a)){ curve(BPF_f_a, 0, 35, col="dimgrey", lwd=2, add=T) }
  if(!is.null(BPF_pop_a)){ BPF_pop_a[,points(BANY, A, col="gray62", cex=1, pch=16)] }
  
}


pdf("../ms/figs/Fig1.pdf")
static_plot_f(estim_a = ESTIM_NO_HC, 
              type_a = "mean", cex.lab=1.3,
              BPF_f_a = BPF_f, 
              BPF_pop_a = BPF_pop)
text(31, 23, "BPF", font=2, col="dimgrey")

# add country-specific trajectories to plot
add_traj_f = function(DHS_CountryCode_mod_a, 
                      estim_a=ESTIM_NO_HC, 
                      type_a = "mean", 
                      up_to_survey_a=NULL,
                      ...){
  
  dhs_loc <- estim_a[DHS_CountryCode_mod==gsub(" ", "",DHS_CountryCode_mod_a)]
  
  setkey(dhs_loc, DHS_CountryCode_mod, SurveyYear)
  
  if(!is.null(up_to_survey_a)){dhs_loc <- dhs_loc[1:up_to_survey_a]}
  
  if(type_a == "mean"){setnames(dhs_loc, c("BANY", "A"), c("BANYloc", "Aloc"))}
  if(type_a == "median"){setnames(dhs_loc, c("BANYmd", "Amd"), c("BANYloc", "Aloc"))}
  
  lines(dhs_loc[, .(BANYloc, Aloc)], col="gray31", lwd=1.3)
  
  # add arrows only if enough distance between successive points
  midpoint_f <- function(x){as.numeric(stats::filter(x, c(0.55, 0.45), sides = 2))}
  dhs_loc[, `:=`(Aloc_mid=midpoint_f(Aloc), BANYloc_mid=midpoint_f(BANYloc))]
  dhs_loc[, dist_loc := sqrt((Aloc_mid-Aloc)^2 + (BANYloc_mid-BANYloc)^2 )]
  dhs_loc[dist_loc > 0.5,
          arrows(x0 = BANYloc, y0 = Aloc, 
                 x1 = BANYloc_mid, y1 = Aloc_mid, 
                 length = 0.065, col="gray31", lwd=1.1)]
  
  text(dhs_loc[SurveyYear==min(SurveyYear), .(BANYloc, Aloc)], 
       labels= DHS_CountryCode_mod_a, 
       font=2,
       cex = 0.9, ...)
}

add_traj_f("BD", pos=3, offset = 0.3) # Bangladesh
add_traj_f("       NP", pos=3, offset = 0.3) # Nepal

add_traj_f("PH", pos=3, offset = 0.3) # Philippines

add_traj_f("CM", pos=4, offset = 0.2) # Cameroon
add_traj_f("ET", pos=3, offset = 0.3) # Ethiopia

add_traj_f("CO", pos=1, offset = 0.3) # Colombia

dev.off()


############ Sensitivity analysis 

pdf("../ms/figs_sup/figS2.pdf", width = 10,height = 11)
par(mfrow=c(2,2))
static_plot_f(estim_a = ESTIM_NO_HC, type_a = "median")
title("A", adj=0.1, font=2, line = -1)
static_plot_f(estim_a = ESTIM_NO_HC_MAB2030_rk23, type_a = "mean" )
title("B", adj=0.1, font=2, line = -1)
static_plot_f(estim_a = ESTIM_NO_HC_rk1, type_a = "mean")
title("C", adj=0.1, font=2, line = -1)
static_plot_f(estim_a = ESTIM_NO_RESTRIC , type_a = "mean")
title("D", adj=0.1, font=2, line = -1)
dev.off()

# check estimates using API values
source("bf/comparison_api.R")


######################################################################
#  Analysis of the exclusive breastfeeding - amenorrhea relationship #
######################################################################

source("bf_exclu/group_surveys.R")
source("bf_exclu/trend_BEX_A.R")


