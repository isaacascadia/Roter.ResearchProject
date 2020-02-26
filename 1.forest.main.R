# 
# 
#===== General ================================================================
# 
# This file contains code that pertains to the following aspects of  
# Isaac's exploration of Jaccard's index and principal component analysis:
#
# 1. Libraries
# 2. File Management
# 3. Run Custom Code 
# 


# What version of R is being used?
R.version.string
# "R version 3.6.2 (2019-12-12)"

# Want more info?
sessionInfo()
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 18363)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] pvclust_2.2-0     dendextend_1.13.3 vegan_2.5-6       lattice_0.20-38  
# [5] permute_0.9-5     pheatmap_1.0.12   ggplot2_3.2.1     gtools_3.8.1     
# [9] dplyr_0.8.4      
# 
# loaded via a namespace (and not attached):
# [1] Rcpp_1.0.3         pillar_1.4.3      compiler_3.6.2     RColorBrewer_1.1-2
# [5] viridis_0.5.1      tools_3.6.2       viridisLite_0.3.0    lifecycle_0.1.0   
# [9] tibble_2.1.3       gtable_0.3.0        nlme_3.1-142       mgcv_1.8-31       
# [13] pkgconfig_2.0.3    rlang_0.4.3        Matrix_1.2-18      rstudioapi_0.11   
# [17] parallel_3.6.2     gridExtra_2.3      withr_2.1.2        cluster_2.1.0     
# [21] grid_3.6.2         tidyselect_0.2.5   glue_1.3.1         R6_2.4.1          
# [25] purrr_0.3.3        magrittr_1.5       scales_1.1.0       MASS_7.3-51.4     
# [29] splines_3.6.2      assertthat_0.2.1   colorspace_1.4-1   lazyeval_0.2.2    
# [33] munsell_0.5.0      crayon_1.3.4  


#============================ 1. Libraries =====================================
# libraries needed for various syntactical, statistical, and visual purposes

library(dplyr)        # %>% - chaining operator
library(gtools)       # combinations() - find all combinations for a number set
library(ggplot2)      # ggplot() - make nice graphs
library(pheatmap)     # pheatmap() - make pretty heatmaps
library(vegan)        # vegdist() - make distance matrix with Jaccard distance



# if you are missing any of the packages, please use this code install them! 

# install.packages("dplyr")
# install.packages("gtools")
# install.packages("ggplot2")
# install.packages("pheatmap")
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



#============================ 3. Run Script Files ==============================

# Run functions script to population global environment with custom functions
source(paste(getwd(), "/2.functions.R", sep = ""))

# Run simulation and testing script to populate folders with raw and output 
# data and figures from all trials performed
source(paste(getwd(), "/3.simulation.testing.R", sep = ""))


#============================ End of Main ======================================


