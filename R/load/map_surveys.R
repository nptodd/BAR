
# load map of countries
World <- sf::st_read("../data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp")
# remove Antartica
World <- World %>% dplyr::filter(!grepl("anta", NAME_EN, ignore.case = T))

# number of surveys per country
Nsurveys <- unique(datasets, by = "SurveyId_original")[,.(N_dhs=.N), by=iso3]

## check : only small islands are missing from World; they would not be visible anyway
datasets[!iso3 %in% World$ISO_A3_EH]

World <- dplyr::left_join(World, Nsurveys, by=c("ISO_A3_EH"="iso3") ) 
World <- World %>% dplyr::mutate(N_dhs=ifelse(is.na(N_dhs), 0, N_dhs))
                            
## check
# sum(World$N_dhs)
# datasets[,.N, by=SurveyId_original][,.N]


breaks_N_dhs <- 0:max(World$N_dhs)
pal_N_dhs <- c("darkgray", sf::sf.colors(max(World$N_dhs)) )

pdf("../ms/figs_sup/FigS1.pdf", width = 8, height = 6)
plot(World["N_dhs"], border =  "transparent", col=pal_N_dhs[World$N_dhs+1], main="")
par(xpd=TRUE)
legend("bottomleft", title="", legend = seq_len(max(World$N_dhs)),  horiz=T, 
       col=pal_N_dhs[-1], bty="n", pch=15, pt.cex=2)
dev.off()

# checks
# datasets[CountryName=="Mexico"]
# datasets[CountryName=="Egypt"]
# datasets[CountryName=="Bolivia"]

rm(Nsurveys)
rm(World)
