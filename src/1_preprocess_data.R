#!/usr/bin/Rscript
# AUTHOR:   Mickael Temporão
# FILE:     1_preprocess_data.R
# ROLE:     TODO (some explanation)
# CREATED:  2016-05-05 10:41:06
# MODIFIED: 2016-05-05 11:33:43

names(Deb)
dim(Deb)
for (i in names(Deb)) {
  print(plot(Deb[,i]))
  cat ("Press [enter] to continue")
  line <- readline()
}

