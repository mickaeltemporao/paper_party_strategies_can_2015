##!/usr/bin/Rscript
# AUTHOR:   Mickael Temporão
# FILE:     /Users/home/party-strategies/0_data-management.R
# ROLE:     Data Management of Party Stragies Project
# CREATED:  2016-05-03 09:39:19
# MODIFIED: 2016-05-05 10:39:28

data_path <- '~/dropbox/MEDW data/Party Strategies/original datasets from basecamp/'
election <- 'PTY_CAN_Quebec_09_04_2012'

# TV debates
Deb <- readstata13::read.dta13(
  paste0(data_path, 'TV debates/', election, '_Debates_recoded.dta')
  )

# TV-ads
Ads <- readstata13::read.dta13(
  paste0(data_path, 'TV-ads/', election, '_TVads.xls_recoded.dta')
  )

# Manifestos
Man <- readstata13::read.dta13(
  paste0(data_path, 'Manifestos/', election, '_Platforms.dta')
  )

# Expert Surveys
ExS <- readstata13::read.dta13(
  paste0(data_path, 'TV debates/', election, '_Debates_recoded.dta')
  )

# TODO: Social Media Data (Fb&Tw)
Fb  <- readstata13::read.dta13(
  paste0(data_path, 'Social Media/', election, '_Debates_recoded.dta')
  )

Tw  <- readstata13::read.dta13(
  paste0(data_path, 'Social Media/', election, '_Debates_recoded.dta')
  )
