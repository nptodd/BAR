ctry_prof_f <- function(DHS_CountryCode_mod_a, 
                        ameno_var_a = "still_ameno",
                        subset_a= "T",
                        
                        bkgd_pts_a=NULL,
                        to_file_a=T,      # print to a file ?
                        directory_a=NULL, # if to_file_a, which directory_a
                        add_leg1=T, add_leg2=T, add_leg3=T,
                        
                        datasets_a= datasets, # to retrieve info on Region
                        
                        title_a=NULL,
                        ...){
  
  if(to_file_a){
    path <- paste0("../figs/", directory_a, "/", DHS_CountryCode_mod_a,".pdf")
    pdf(path, width = 14, height = 5)
    par(mfrow = c( 1, 3), mar= c(6, 6, 1, 1) + 0.1)
  }
  
  # file_path <- paste0("../data/final_data/dtb_cntry/", DHS_CountryCode_mod_a, ".csv")
  
  A <- plot_ctry_f(DHS_CountryCode_mod_a = DHS_CountryCode_mod_a, 
                   var_a = "still_bf_any", 
                   bkgd_pts_a = bkgd_pts_a$bf_any, 
                   subset_a = subset_a,
                   add_leg_a = add_leg1, 
                   ...)
  
  B <- plot_ctry_f(DHS_CountryCode_mod_a = DHS_CountryCode_mod_a, 
                   var_a = "still_bf_exclu", 
                   bkgd_pts_a = bkgd_pts_a$bf_exclu, 
                   subset_a = subset_a,
                   add_leg_a = add_leg2, 
                   add_info_a = "reduced", 
                   ...)
  
  if(!is.null(title_a)){title(title_a, cex.main=3, line = -1.5)}
  
  C <- plot_ctry_f(DHS_CountryCode_mod_a = DHS_CountryCode_mod_a, 
                   var_a = ameno_var_a, 
                   bkgd_pts_a = bkgd_pts_a$ameno, 
                   subset_a = subset_a,
                   add_leg_a = add_leg3,
                   ...)
  
  setnames(A, c("mean", "median"), c("BANY", "BANYmd"))
  A[B, on=.(SurveyId_mod), `:=`(BEX = mean, BEXmd = median)]
  A[C, on=.(SurveyId_mod), `:=`(A = mean, Amd = median)]
  
  A[,DBP:= BPF_f(BANY) - A]
  A[,DBP_prop := BPF_f(BANY)/A]
  A[,DBPmd:=BPF_f(BANYmd)- Amd]
  A[,DBPmd_prop := BPF_f(BANYmd)/Amd]
  
  A[datasets_a, on=.(SurveyId_mod), `:=`(Region = Region,
                                         Region_code = Region_code,
                                         Region_col  = Region_col)]
  
  if(to_file_a){ dev.off() }
  
  return(A)
}

