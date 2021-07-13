require(rmarkdown)

file_name <- paste0('net_energy_equity_',format(Sys.time(),"%Y%m%d%H%M%S"),'.pdf')

rmarkdown::render("net_energy_equity.Rmd",
                  output_file=file_name)