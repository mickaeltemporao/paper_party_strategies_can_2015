# ------------------------------------------------------------------------------
# Title:        Update script
# Filename:     update.command
# Description:  Script that updates to most recent changes in git folder
# Version:      0.0.0.000
# Created:      2016-05-17 07:09:22
# Modified:     2016-05-17 07:09:22
# Author:       Mickael Temporão < mickael.temporao.1 at ulaval.ca >
# ------------------------------------------------------------------------------
# Copyright (C) 2016 Mickael Temporão
# Licensed under the GPL-2 < https://www.gnu.org/licenses/gpl-2.0.txt >
# ------------------------------------------------------------------------------

# Change this file's permissions to chmod +x update.command
cd "$(dirname "$0")"
git pull
