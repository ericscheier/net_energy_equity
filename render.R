require(rmarkdown)

file_name <- paste0('net_energy_equity_final_review',format(Sys.time(),"%Y%m%d%H%M%S"),'.pdf')

rmarkdown::render("net_energy_equity_final_review.Rmd",
                  output_file=file_name)