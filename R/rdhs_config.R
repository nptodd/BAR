
set_rdhs_config(email = my_email, # CHANGE
                project = my_project, # CHANGE
                config_path = "rdhs.json",
                global = FALSE,
                cache_path = path_to_data,
                timeout =90,
                verbose_download = T,
                data_frame = 'data.table::as.data.table')

# useless .gitignore file created by set_rdhs_config 
file.remove(".gitignore")
