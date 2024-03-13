

for (region in regions){
  
  cur_inputs_path <- file.path(inputs_path, region) 
  
  files_list <- list.files(cur_inputs_path, full.names = T)
  
  index <- which(regions == region)
  
  for (cur_file in files_list) {
    
    new_file <- str_replace(cur_file, str_c('_', index), '')
    file.rename(cur_file, new_file)
  }
  
  
}
