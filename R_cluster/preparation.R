####################################################################################
####################################################################################
################################  SEND DATA & CODE  ################################ 

# -- CONNECT TO HOST FOR FILE TRANSFER-- 
S <- ssh::ssh_connect(host = "nicolas.todd@transfer.gwdg.de",
                      keyfile = "~/.ssh/id-rsa")


# -- create project folder --
ssh::ssh_exec_wait(S, 'mkdir BAR')
# -- Upload data --
ssh::ssh_exec_wait(S, 'mkdir -p BAR/data/final_data')
ssh::scp_upload(S, '../data/final_data/datasets.csv', 'BAR/data/final_data')
ssh::scp_upload(S, '../data/final_data/dtb.csv', 'BAR/data/final_data')
ssh::scp_upload(S, '../data/final_data/dtb_cntry', 'BAR/data/final_data')
# -- Upload code --
ssh::scp_upload(S, '../R', 'BAR')
ssh::scp_upload(S, '../R_cluster', 'BAR')
# -- create results folder --
ssh::ssh_exec_wait(S, 'mkdir -p BAR/results_cluster/out_files')

# go to the GWDG cluster and run job array


####################################################################################
####################################################################################
################################## RECEIVE RESULTS #################################

# -- CONNECT TO HOST FOR FILE TRANSFER-- 
S <- ssh::ssh_connect(host = "nicolas.todd@transfer.gwdg.de",
                      keyfile = "~/.ssh/id-rsa")

# remove existing results ?
# if(dir.exists("../results_cluster/")){unlink("../results_cluster/", recursive = TRUE)}
# # download back results
ssh::scp_download(S, 'BAR/results_cluster', '..' )


write_results_f = function(folder_a){
  
  folder_res <- paste0("../results_cluster/", folder_a)
  file_out <- paste0( "../results_cluster/", toupper(folder_a),".csv")
  
  path_results <- list.files(folder_res, pattern = "\\.csv$", full.names = T)
  res_list <- lapply(path_results, data.table::fread)
  ESTIMATES <- data.table::rbindlist(res_list, use.names = T)
  
  ESTIMATES <- ESTIMATES[N_used>=70]
  
  # correct BEX that are truly zero : set to "very low value with high confidence"
  ESTIMATES[BEX<1e-6, `:=`(BEX=0.1, BEX_se=0.05)]
  
  if("group" %in% names(ESTIMATES)){  # variables for meta-regression
    
    ESTIMATES[, urban:= as.integer(startsWith(group, "u"))]
    ESTIMATES[, elec:=  as.integer(substr(group,2,2)=="y")]
    
    if(startsWith(folder_a, "energy4")){
      ESTIMATES[, water:= as.integer(substr(group,3,3)=="y")]
      ESTIMATES[, educ:= as.integer(substr(group,4,4)=="y")]
    }
    
    if(endsWith(folder_a, "parity")){
      ESTIMATES[, par1:= as.integer(endsWith(group,"1"))]
      ESTIMATES[, par2:= as.integer(endsWith(group,"2"))]
      ESTIMATES[, par4:= as.integer(endsWith(group,"4"))]
    }
    
  } # end if("group" %in% names(ESTIMATES))
  
  data.table::fwrite(ESTIMATES, file_out)
  
  return(0)
}

write_results_f("country")
write_results_f("energy4_parity")



