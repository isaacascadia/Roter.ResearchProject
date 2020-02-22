# 
# 
#===== General ================================================================
# 
# This file contains code that pertains to the following aspects of  
# Isaac's exploration of Jaccard's index and principal component analysis:
#
# 1. Libraries
# 2. File Management
# 3. Global Variables
# 4. Run Custom Code 
# 
# cluster analysis to come?


R.version.string
# "R version 3.6.2 (2019-12-12)"

sessionInfo()  # for more info

#============================ 1. Libraries =====================================
# libraries needed for downloading gtrends and weather data

library(dplyr)    # %>% 
library(gtools)   # combinations()
library(ggplot2)  # ggplot()
library(vegan)    # vegdist()



# if you don't have the library yet, please install it! Either with the script 
# below, or with the "Install Packages..." option under the 'Tools' menu

# install.packages("dplyr")
# install.packages("gtools")
# install.packages("ggplot2")
# install.packages("vegan")


#============================ 2. File Management ===============================

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


#============================ 3. Global Variables ==============================

# none yet


#============================ 4. Run Custom Codes ==============================

# # Run script files
source(paste(getwd(), "/2.data.simulation.R", sep = ""))
source(paste(getwd(), "/3.jaccard.R", sep = ""))
source(paste(getwd(), "/4.pca.R", sep = ""))


#============================ End of Main ======================================


