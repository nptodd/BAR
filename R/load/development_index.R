################################################################################
##############################          HDI         ############################
################################################################################

# Retrieve HDI at the national and subnational level for for 1990-2017 :
# https://globaldatalab.org/shdi/download_files/
# National estimates (necessary for Sri Lanka) are downloaded directy from 
# http://hdr.undp.org/en/content/human-development-index-hdi
# National estimates for the 1980s downloaded from Our World in Data
# https://ourworldindata.org/human-development-index

# years for which to provide values of the HDI
TARGET_YEARS <- as.character(seq(datasets[,min(SurveyYear)], format(Sys.Date(), "%Y")))

################################################################################
############################## National estimates ##############################

HDI_NAT <- setDT(readxl::read_excel("../data/dev_index/2018_all_indicators.xlsx", sheet="Data"))

HDI_NAT <- HDI_NAT[indicator_name=="Human Development Index (HDI)"]

HDI_NAT <- melt(HDI_NAT, id.vars = c("country_name", "iso3"),
                measure.vars = intersect(names(HDI_NAT), TARGET_YEARS),
                variable.name = "year", value.name = "hdi", variable.factor = F)

# retrieved from Our World in Data
HDI_longterm <- fread("../data/dev_index/human-development-index_OWID.csv")

HDI_longterm[, year:=as.character(year)]
HDI_longterm[HDI_NAT, on=.(iso3, year), hdi_new:=hdi]
# HDI_longterm[year>1989,summary(hdi_new-hdi_lt)] # for years where we have both
# HDI_longterm[year>1989 & is.na(hdi_new)] # Korea missing from OWID dataset
setnames(HDI_longterm, "hdi_lt", "hdi")

# HDI_NAT merged to part of HDI_longterm we don't have
HDI_longterm = HDI_longterm[is.na(hdi_new), .(country_name, iso3, year, hdi)]
HDI_NAT <- rbindlist(l = list(HDI_NAT, HDI_longterm))
rm(HDI_longterm)

# add DHS-specific code
HDI_NAT[datasets, on=.(iso3), `:=`(DHS_CountryCode_mod = DHS_CountryCode_mod)]

# remove India (partionned in regions in the analysis) and countries without a DHS
HDI_NAT <- HDI_NAT[!is.na(DHS_CountryCode_mod) & !country_name=="India"]


################################################################################
###################### National and subnational estimates ######################

# national estimates are not available for all countries (in particular Sri Lanka)
HDI_SUB <- fread("../data/dev_index/SHDI Complete 3.0.csv")

HDI_SUB[, region:=tolower(region)]

setnames(HDI_SUB, c("shdi"), c("hdi"))

# add DHS-specific code
HDI_SUB[datasets, on=c(iso_code="iso3"), 
        `:=`(DHS_CountryCode_mod = DHS_CountryCode_mod)]

# restrict to to Indian states
HDI_SUB <- HDI_SUB[level=="Subnat"& country=="India"]

HDI_SUB[Indian_states, on = c(region="v024"), 
        DHS_CountryCode_mod := region]

# compute hdi for the regions used
HDI_INDIA <- HDI_SUB[,.(hdi=weighted.mean(hdi, pop)), 
                     by=.(DHS_CountryCode_mod, year, iso_code)]


################################################################################
######################### Fusion of the two data.tables ########################

HDI_NAT  <-  HDI_NAT[,  .(DHS_CountryCode_mod, year, hdi)]
HDI_INDIA <- HDI_INDIA[,.(DHS_CountryCode_mod, year, hdi)]

HDI <- rbindlist(list(HDI_NAT, HDI_INDIA), fill = F)

# check all countries are found in HDI 
if(!all(datasets[, DHS_CountryCode_mod ] %in% HDI[, DHS_CountryCode_mod])){
  stop("countries missing!!!") }

# dcast then create NA columns for years not available
HDI <- dcast(HDI, DHS_CountryCode_mod ~ year , value.var = "hdi")
HDI[, setdiff(TARGET_YEARS, names(HDI)) := NA_real_]

# melt back
HDI <- melt(HDI, id.vars = c("DHS_CountryCode_mod"),
            measure.vars = TARGET_YEARS,
            variable.name = "year", value.name = "hdi")

HDI[, SurveyYear:=as.numeric(as.character(year))]

setkey(HDI, DHS_CountryCode_mod, SurveyYear)


# impute missing values
HDI[, hdi_imp := is.na(hdi)]
HDI[, hdi:=imputeTS::na_kalman(hdi), by=.(DHS_CountryCode_mod)]

if(is.character(datasets[, SurveyYear])){
  # if no quick load, then SurveyYear is a character variable
   datasets[, SurveyYear:=as.numeric(SurveyYear)]}

HDI[datasets, on= .(DHS_CountryCode_mod, SurveyYear), 
    SurveyId_mod := SurveyId_mod ]
setcolorder(HDI, "SurveyId_mod")


# plot(HDI[DHS_CountryCode_mod=="BD", .(as.numeric(as.character(year)), hdi)], pch=20)
# points(HDI[DHS_CountryCode_mod=="ZM", .(as.numeric(as.character(year)), hdi)], pch=20,col="red")

DEV_TABLE = HDI[!is.na(SurveyId_mod) ]

DEV_TABLE[datasets, on=.(SurveyId_mod), 
          `:=`(Region = Region,
               Region_code = Region_code,
               Region_col = Region_col)]


################################################################################
############################  DHS-derived dev index  ###########################

# Crude measure of fertility
DEV_DHS <- dtb[, .(MB=weighted.mean(as.numeric(bord), sample_weight)),
               by=SurveyId_mod]
DEV_TABLE[DEV_DHS, on=.(SurveyId_mod), mean_bord:=MB]
rm(DEV_DHS)

# Urbanization
DEV_DHS <- dtb[,.(URBANIZATION=weighted.mean(residence=="urban", sample_weight)),
               by=SurveyId_mod]
DEV_TABLE[DEV_DHS, on=.(SurveyId_mod), urbanization:=URBANIZATION]
rm(DEV_DHS)

# access to electricity
DEV_DHS <- dtb[elec_g %in% c("y", "n"), 
               .(ELEC=weighted.mean(elec_g=="y", sample_weight)),
               by=SurveyId_mod]
DEV_TABLE[DEV_DHS, on=.(SurveyId_mod), pct_elec:=ELEC]
rm(DEV_DHS)

# rural access to electricity
DEV_DHS <- dtb[residence == "rural" & elec_g %in% c("y", "n"), 
               .(ELEC_RURAL=weighted.mean(elec_g=="y", sample_weight)),
               by=SurveyId_mod]
DEV_TABLE[DEV_DHS, on=.(SurveyId_mod), pct_elec_rural:=ELEC_RURAL]
rm(DEV_DHS)

# summary(DEV_TABLE)


