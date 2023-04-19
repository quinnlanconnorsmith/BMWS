library(wdnr.fmdb)

surveys <- get_fmdb_surveys(wbic = 805400, year = 2020:2022)
efforts <- get_fmdb_efforts(wbic = 805400, year = 2020:2022)
fishraw <- get_fmdb_fishraw(wbic = 805400, year = 2022)

mendota_cpe <- calc_cpe(surveys, efforts, fishraw)

browseVignettes(package = "wdnr.fmdb")

