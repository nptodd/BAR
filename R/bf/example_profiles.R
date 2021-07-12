
bkgd_pts_no_hc_l = list(bf_any   = dtb[hormonal_contracep_cur==F, 
                                       .( mean = weighted.mean(still_bf_any, sample_weight)),
                                       keyby=.(SurveyId_mod, age_interview)  ],
                        
                        ameno    = dtb[hormonal_contracep_cur==F,
                                       .( mean = weighted.mean(still_ameno, sample_weight)),
                                       keyby=.(SurveyId_mod, age_interview)  ],
                        
                        bf_exclu = dtb[hormonal_contracep_cur==F,
                                       .( mean = weighted.mean(still_bf_exclu, sample_weight)),
                                       keyby=.(SurveyId_mod, age_interview)  ] )

####################################### FIG S3 #######################################

# Bangladesh
pdf("../ms/figs_sup/FigS3A.pdf", width = 18, height = 7)
par(mfrow=c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
ctry_prof_f("BD", 
            subset_a = "hormonal_contracep_cur==F",
            bkgd_pts_a = bkgd_pts_no_hc_l,
            to_file_a = F, txt_cex_a= 1.5, lwd_a=3, 
            add_leg1 = F, add_leg2 = F, add_leg3 = T, 
            detail_leg_a=0,
            title_a = "Bangladesh")
dev.off()


# Nepal
pdf("../ms/figs_sup/FigS3B.pdf", width = 18, height = 7)
par(mfrow=c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
ctry_prof_f("NP", 
            subset_a = "hormonal_contracep_cur==F",
            bkgd_pts_a = bkgd_pts_no_hc_l, 
            to_file_a = F, txt_cex_a= 1.5, lwd_a=3, 
            add_leg1 = F, add_leg2 = F, add_leg3 = T, 
            detail_leg_a=0, 
            title_a = "Nepal")
dev.off()

# India, central zone
pdf("../ms/figs_sup/FigS3C.pdf", width = 18, height = 7)
par(mfrow=c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
ctry_prof_f("IAc", 
            subset_a = "hormonal_contracep_cur==F",
            bkgd_pts_a = bkgd_pts_no_hc_l, 
            to_file_a = F, txt_cex_a= 1.5, lwd_a=3, 
            add_leg1 = F, add_leg2 = F, add_leg3 = T, 
            detail_leg_a=0,
            title_a= "India, Central Zone")
dev.off()

# Philippines
pdf("../ms/figs_sup/FigS3D.pdf", width = 18, height = 7)
par(mfrow=c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
ctry_prof_f("PH", 
            subset_a = "hormonal_contracep_cur==F",
            bkgd_pts_a = bkgd_pts_no_hc_l, 
            to_file_a = F, txt_cex_a= 1.5, lwd_a=3, 
            add_leg1 = F, add_leg2 = F, add_leg3 = T, 
            detail_leg_a=0, 
            title_a = "Philippines")
dev.off()


####################################### FIG S4 #######################################

# Chad
pdf("../ms/figs_sup/figS4A.pdf", width = 18, height = 7)
par(mfrow=c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
ctry_prof_f("TD", 
            subset_a = "hormonal_contracep_cur==F",
            bkgd_pts_a = bkgd_pts_no_hc_l,
            to_file_a = F, txt_cex_a= 1.5, lwd_a=3, 
            add_leg1 = F, add_leg2 = F, add_leg3 = T, 
            detail_leg_a=0,
            title_a = "Chad")
dev.off()

# Ethiopia
pdf("../ms/figs_sup/figS4B.pdf", width = 18, height = 7)
par(mfrow=c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
ctry_prof_f("ET", 
            subset_a = "hormonal_contracep_cur==F",
            bkgd_pts_a = bkgd_pts_no_hc_l, 
            to_file_a = F, txt_cex_a= 1.5, lwd_a=3, 
            add_leg1 = F, add_leg2 = F, add_leg3 = T, 
            detail_leg_a=0,
            title_a = "Ethiopia")
dev.off()

# Niger
pdf("../ms/figs_sup/figS4C.pdf", width = 18, height = 7)
par(mfrow=c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
ctry_prof_f("NI", 
            subset_a = "hormonal_contracep_cur==F",
            bkgd_pts_a = bkgd_pts_no_hc_l, 
            to_file_a = F, txt_cex_a= 1.5, lwd_a=3, 
            add_leg1 = F, add_leg2 = F, add_leg3 = T, 
            detail_leg_a=0,
            title_a = "Niger")
dev.off()


# Mali
pdf("../ms/figs_sup/figS4D.pdf", width = 18, height = 7)
par(mfrow=c(1, 3), mar= c(6, 6, 1, 1) + 0.1)
ctry_prof_f("ML", 
            subset_a = "hormonal_contracep_cur==F",
            bkgd_pts_a = bkgd_pts_no_hc_l, 
            to_file_a = F, txt_cex_a= 1.5, lwd_a=3, 
            add_leg1 = F, add_leg2 = F, add_leg3 = T, 
            detail_leg_a=0,
            title_a = "Mali")
dev.off()


rm(bkgd_pts_no_hc_l)






