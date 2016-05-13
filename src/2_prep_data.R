#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        TODO: (add title)
# Filename:     test.R
# Description:  TODO: (write me)
# Version:      0.0.0.000
# Created:      2016-05-05 10:41:06
# Modified:     2016-05-12 15:19:01
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

#Get first letter
df_list <- lapply(ls(pattern="^[A-Z]"), get)

# Convert all variable names to lower case for merge
df_list <- lapply(df_list,
  function(i) {
    y <- data.frame(i)
    names(y) <- tolower(names(y))
    return(y)
  }
)

# Convert list to data.frames in GlobalEnv
list2env(df_list ,.GlobalEnv)
