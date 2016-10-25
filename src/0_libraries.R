#!/usr/bin/env Rscript
# ------------------------------------------------------------------------------
# Title:        Libraries.R
# Filename:     0_libraries.R
# Description:  Installs and loads requirred libraries
# Version:      0.0.0.000
# Created:      2016-05-09 11:14:49
# Modified:     2016-10-25 06:47:43
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------
list_packages <- c(
  "devtools",
  "dplyr",
  "ggplot2",
  "ggthemes",
  "readstata13",
  "readxl",
  "stringr"
)

to_install <- list_packages[!(list_packages %in% installed.packages()[,"Package"])]
if(length(to_install)) install.packages(to_install)

lapply(list_packages, require, character.only = TRUE)
