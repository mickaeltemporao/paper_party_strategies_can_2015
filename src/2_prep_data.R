#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-05-14 10:59:16
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

# Creating a list the data to be used filtered by UpperCase first letter
df_list <- lapply(ls(pattern="^[A-Z]"), get)
names(df_list) <- ls(pattern="^[A-Z]")

# Convert all variable names to lower case for merge
df_list <- lapply(df_list,
  function(i) {
    y <- data.frame(i)
    names(y) <- gsub(" ", "", names(y), fixed = TRUE)
    names(y) <- tolower(names(y))
    return(y)
  }
)

# Convert df_list to a global data.frame with all data types
Data <- data.table::rbindlist(df_list, fill=T)
# Create type of data variable in Global data.frame
Data$type_source <- rep(names(df_list), sapply(df_list, nrow))

# Clean workspace
rm(list=setdiff(ls(), "Data"))
gc()
