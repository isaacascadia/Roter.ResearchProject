# 
# 
#===== General ================================================================
# 
# This file contains code that pertains to the following aspects of the 
# sex-weather project:
#
# 1. Libraries
# 2. File Management
# 3. Global Variables
# 4. Run Custom Codes
# 
# 
R.version.string
# "R version 3.6.2 (2019-12-12)"

sessionInfo()  # for more info

#==== 1. Libraries==============================================================
# libraries needed for downloading gtrends and weather data

library(dplyr)
library(gtools)
library(ggplot2)
library(vegan)


# if you don't have the library yet, please install it! Either with the script 
# below, or with the "Install Packages..." option under the 'Tools' menu

# install.packages("dplyr")
# install.packages("gtools")
# install.packages("ggplot2")
# install.packages("vegan")


#======== 2. File Management ===================================================

# working directory
wd <- getwd()

# folders for storing data outputs and figures
# store names of the folders in an object
output.folder.names <- c("data.raw", "data.output", "figures")
# and make the folders if they don't exist yet. 
for(i in 1:length(output.folder.names)){ 
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])
  }

# path to raw data folder
path.data.raw <- paste(wd,"/",output.folder.names[1],"/", sep = "")

# path to data output folder
path.data.output <- paste(wd,"/",output.folder.names[2],"/", sep = "")

# path to figures folder
path.figures <- paste(wd,"/",output.folder.names[3],"/", sep = "")


#===== 3. Global Variables =====================================================

set.seed(seed = 4)


#============================ 4. Run Custom Codes ==============================

# # Run some script
source()

#============================ End of Main ======================================


