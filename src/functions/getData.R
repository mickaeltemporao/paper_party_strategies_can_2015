#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     getData.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-12-07 08:40:56
# Modified:     2016-12-07 08:41:01
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
getData <- function (x) {
  require(dplyr)
  d <- data.frame(readxl::read_excel(x))
  names(d) <- gsub(".", "", names(d), fixed = TRUE)
  names(d) <- tolower(names(d))
  d <- d %>%
    select(date,
           actorparty,
           objectparty,
           direction,
           language,
           tone,
           spotlength,
           adid
    ) %>% return
}

