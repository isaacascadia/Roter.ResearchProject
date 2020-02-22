

#============================ pca function =====================================

# more .csv data saving to come!

fpca <- function(samps.vars, pca.name){  # pca.name is for plot/pdf titles
  
# expects samples to be rows, and variables to be columns (transpose if needed)
  pca <- prcomp(samps.vars, scale = TRUE)  # returns x (PCs)

# begin saving of pca plots
  pdf(paste(path.figures, gsub(" ", "", pca.name), ".figures.pdf", sep = ""),
      width = 5, height = 5) 
  
# What does the plot of our first two principal components look like?
  plot(pca$x[,1], pca$x[,2], xlab = "PC 1", ylab = "PC 2",
       main = paste("PCs of", pca.name, sep = " "))
  
# what is the variation covered by each PC?
  pca.var <- pca$sdev^2
  
# what percent of the total variation is covered by each PC?
  pca.var.per <- round(pca.var/sum(pca.var) * 100, 1)
  
# Scree plot of variation distribution among PCs
  barplot(pca.var.per, main = paste("Scree Plot of", pca.name, sep = " "), 
          xlab = "Principal Component", ylab = "Percent Variation",
          names.arg = colnames(pca$x), cex.names = 0.9, las = 1)

   dev.off()  # finish saving plots
  
# loading scores goodness
loading.scores <- pca$rotation[,1]                    # display loading scores
var.scores <- abs(loading.scores)                     # loading score magnitudes
var.score.ranked <- sort(var.scores, decreasing = T)  # loading scores in order

# determine appropriate number of loading scores to display
if(ncol(samps.vars) <= 5){top.vars <- names(var.score.ranked[1:3])}
if(ncol(samps.vars) >= 6 && ncol(samps.vars) <= 10){top.vars <- 
  names(var.score.ranked[1:5])}
if(ncol(samps.vars) >= 11){top.vars <- names(var.score.ranked[1:10])}

# save loading scores as .csv
write.csv(pca$rotation, paste(path.data.output, gsub(" ", "", pca.name), 
                              ".loading.csv", sep = ""))

print("most important variables") # names of most important variables

return(pca$rotation[top.vars,1])  # names and scores of most important variables

  
}  # end of pca function




#============================ first pca trial, one population ==================

fpca(pca.test.data, "pca.test")




#============================ second trial, 3 populations ======================
# tree 3 mean varies, remainder are the same

fpca(study1, "study 1")




#============================ third trial, 3 variables =======================
# tree 3 mean varies, remainder are the same

fpca(study2, "study 2")












