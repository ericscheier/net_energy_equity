download_egrid <- function(){
  url <- "https://www.epa.gov/sites/production/files/2020-03/egrid2018_data_v2.xlsx"
  temp <- tempfile()
  download.file(url,temp,mode="wb", method="libcurl")
  mode = 'wb'
  path <- temp
  path %>%
    excel_sheets() %>%
    set_names() %>% 
    map(read_then_csv, path = path)
}

get_state_egrid <- function(){
  relevant_file_path <- "data/ST18.csv"
  if(!file.exists(relevant_file_path)){
    download_egrid()
  }
  state_egrid <- read_csv(relevant_file_path, skip=2)
  
  return(state_egrid)
}