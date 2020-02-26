
# This script file is a place for me to mess around and experiment. 
# Some things are functional but were made obsolete or deemed unnecessary.
# Many other things don't work! Annotation isn't highly prioritized here as 
# this is essentially a scrap pile.

# Contents:
# First automated simulation
# other data simulation stuff
# prep simulated dataframe for PCA function
# JI calculation function
# JI function with heatmap
# JI distance function
# Jaccard works in progress
# ggplot pca visualization
# first pca trial 
# dendrogram stuff
# dendrogram stuff for integration into JI function


#================== first automated simulation =================================

# enter species means and sds for three different populations
alp.means <- c(50, 100, 150, 300, 10, 275)
alp.sd <- rep(x = 12, times = 6)
sub.means <- c(100, 50, 300, 150, 200, 30)
sub.sd <- rep(x = 12, times = 6)
val.means <- c(200, 10, 50, 200, 100, 180)
val.sd <- rep(x = 12, times = 6)

# make a matrix to put all the data into
simul.data <- matrix(nrow = 90, ncol = 6)

# give each observation a name according to its population
rownames(simul.data) <- c(paste("alpine", 1:30, sep = ""),
                          paste("subalp", 1:30, sep = ""),
                          paste("valley", 1:30, sep = ""))

# name all the variables
colnames(simul.data) <- paste("tree", 1:6, sep = "")

# simulate the data!
for(i in 1:6){
  alp.values <- rnorm(n = 30, m = alp.means[i], sd = alp.sd[i])
  sub.values <- rnorm(n = 30, m = sub.means[i], sd = sub.sd[i])
  val.values <- rnorm(n = 30, m = val.means[i], sd = val.sd[i])
  
  simul.data[,i] <- c(alp.values, sub.values, val.values)
}

# what do we have?
head(simul.data)





#================== other data simulation stuff ================================

# mostly messing around with finding a more efficient way to simulate data

test.abund <- data.frame(trees1 = rep(NA, 20), 
                         trees2 = rep(NA, 20), 
                         trees3 = rep(NA, 20), 
                         trees4 = rep(NA, 20))

for(i in 1:nrow(alp.abund)){
  
  test.abund[,i] <- c(tree.abund, rnorm(n.trees[i], mean.trees[i], sd.trees[i]))
  
}

alpine.auto <- matrix(nrow = 11, ncol = 3)
colnames(alpine.auto) <- paste("tree", 1:3, sep = "")
rownames(data.matrix) <- paste("plot", 1:11, sep = "")

for(i in 1:nrow(alpine.auto)) {
  
  tree1 <- rnorm(n = 46, mean = 180, sd = 12)
  tree2 <- rnorm(n = 46, mean = 170, sd = 12)
  tree3 <- rnorm(n = 46, mean = 150, sd = 12)
  
  data.matrix[i,] <- c(tree1, tree2, tree3)
}

head(data.matrix)

alpine.auto <- c(c(1:11), c(1:11), c(1:11))

data.matrix()




#================== prep simulated dataframe for pca function ==================

# function to add plot numbers to data matrix and save csvs 
fpca.ready <- function(plots.matrix, csv.name){  # adds plot nums to a matrix 
 
  # add plot numbers to matrix rows
  rownames(plots.matrix) <- paste("plot", 1:nrow(plots.matrix), sep = "")
  
  # save output locally
  write.csv(plots.matrix, paste(path.data.raw, csv.name, 
                                ".data.csv", sep = ""))
  # show the user what they've got
  return(plots.matrix)
}






#================== jaccard's index calculation function =======================
# function for calculating jaccard's similarity for a presence/absence dataset
fjaccard <- function(binary.dataframe, csv.name){
  
  # how many combinations are there
  combs <- combinations(n = nrow(binary.dataframe), r = 2)
  
  #  create an empty dataframe to enter comparisons and JI scores into
  j.sim <- data.frame("comparison" = rep(NA, nrow(combs)), 
                      "JI" = rep(NA, nrow(combs)))
  
  # loop definition, repeats for every possible combination of samples in datset
  for(i in 1:nrow(combs)){
    
    # write names of paired, compared samples into output dataset
    j.sim$comparison[i] <- paste(as.character(combs[i,1]), 
                                 as.character(combs[i,2])) %>%
      gsub(pattern = " ", replacement = "-")
    
    # which rows (plots) are being compared?
    n1 <- combs[i,1]
    n2 <- combs[i,2]
    
    trees.i <- which(binary.dataframe[n1,] == 1)
    trees.j <- which(binary.dataframe[n2,] == 1)
    
    # calculate jaccard index for pair
    j.sim$JI[i] <- length(intersect(trees.i, trees.j)) / 
      unique(c(trees.i, trees.j)) %>% length()
    
    
  }  # end of jaccard index loop
  
  # save data output as a .csv
  write.csv(j.sim, paste(path.data.output, csv.name, 
                         ".jaccard.csv", sep = ""))
  
  # display output dataframe
  return(j.sim)
  
}  # end jaccard function



#================== jaccard's index calculation and heatmap  function ==========

# function for calculating jaccard's similarity for a presence/absence dataset
# and outputting a heatmap
fjaccard <- function(binary.dataframe, ji.name){
  
  n.plots <- nrow(binary.dataframe)
  
  # how many combinations are there
  combs <- combinations(n = nrow(binary.dataframe), r = 2)
  
  combs <- rbind(combs, matrix(rep(x = 1:n.plots, 2), 
                               nrow = n.plots, ncol = 2))
  
  
  
  # create an empty matrix
  j.output <- matrix(data = NA, 
                     nrow = n.plots, 
                     ncol = n.plots)
  
  # row and column names
  rownames(j.output) <- paste("plot", 1:n.plots, sep = "")
  colnames(j.output) <- paste("plot", 1:n.plots, sep = "")
  
  
  
  # loop definition, repeats for every possible combination of samples in datset
  for(i in 1:nrow(combs)){
    
    
    # which rows (plots) are being compared?
    n1 <- combs[i,1]
    n2 <- combs[i,2]
    
    trees.i <- which(binary.dataframe[n1,] == 1)
    trees.j <- which(binary.dataframe[n2,] == 1)
    
    # calculate jaccard index for pair
    j.output[n1, n2] <- length(intersect(trees.i, trees.j)) / 
      unique(c(trees.i, trees.j)) %>% length()
    
    j.output[n2, n1] <- length(intersect(trees.i, trees.j)) / 
      unique(c(trees.i, trees.j)) %>% length()
    
    
    
  }  # end of jaccard index loop
  
  
  heatmap <- pheatmap(j.output, main =  paste(ji.name, "JI scores"), 
                      legend_labels = c("JI Score"), 
                      cluster_rows = F, cluster_cols = F)
  
  # begin saving of pca plots
  pdf(paste(path.figures, gsub(" ", "", ji.name), ".heatmap.pdf", sep = ""),
      width = 5, height = 5) 
  
  # save principal component plot and scree plot
  heatmap
  
  dev.off()  # finish saving plots
  
  # save data output as a .csv
  write.csv(j.output, paste(path.data.output, gsub(" ", "", ji.name), 
                            ".jaccard.csv", sep = ""))
  
  # display output dataframe
  return(j.output)
  
}  # end jaccard function


#================== jaccard's distance and heatmap function ====================

# function for calculating jaccard's distance for a presence/absence dataset
# and outputting a heatmap
fjaccard.dist <- function(binary.matrix, ji.name){
  
  n.plots <- nrow(binary.matrix)
  
  # how many combinations are there
  combs <- combinations(n = nrow(binary.matrix), r = 2)
  
  combs <- rbind(combs, matrix(rep(x = 1:n.plots, 2), 
                               nrow = n.plots, ncol = 2))
  
  # create an empty matrix
  j.dist.output <- matrix(data = NA, 
                          nrow = n.plots, 
                          ncol = n.plots)
  
  # row and column names
  rownames(j.output) <- rownames(binary.matrix)
  colnames(j.output) <- rownames(binary.matrix)
  
  
  
  # loop definition, repeats for every possible combination of samples in datset
  for(i in 1:nrow(combs)){
    
    
    # which rows (plots) are being compared?
    n1 <- combs[i,1]
    n2 <- combs[i,2]
    
    trees.i <- which(binary.matrix[n1,] == 1)
    trees.j <- which(binary.matrix[n2,] == 1)
    
    # calculate jaccard index for pair
    j.dist.output[n1, n2] <- 1 - length(intersect(trees.i, trees.j)) / 
      unique(c(trees.i, trees.j)) %>% length()
    
    j.dist.output[n2, n1] <- 1 - length(intersect(trees.i, trees.j)) / 
      unique(c(trees.i, trees.j)) %>% length()
    
    
    
  }  # end of jaccard index loop
  
  # display heatmap to user in RStudio graphic window
  pheatmap(j.dist.output, main =  paste(ji.name, "JI scores"), 
           legend_labels = c("JI Score"), 
           cluster_rows = F, cluster_cols = F)
  
  # begin saving of heatmap
  pdf(paste(path.figures, gsub(" ", "", ji.name), ".heatmap.pdf", sep = ""),
      width = 5, height = 5) 
  
  # display heatmap to be saved
  pheatmap(j.dist.output, main =  paste(ji.name, "JI scores"), 
           legend_labels = c("JI Score"), 
           cluster_rows = F, cluster_cols = F)
  
  dev.off()  # finish saving plots
  
  # save data output as a .csv
  write.csv(j.dist.output, paste(path.data.output, gsub(" ", "", ji.name), 
                                 ".jaccard.csv", sep = ""))
  
  # display output dataframe
  return(j.dist.output)
  
}  # end jaccard function





#============================ Jaccard works in progress ========================

# Jaccard distance based on Bray-Curtis dissimilarity for quantities (vegan)


# INCOMPLETE

bdist <- rep(NA, nrow(test.data))

for(i in 1:ncol(test.data)){
  
  bdist[i] <- abs(test.data[1,i] - test.data[2,i+1]) / 
    test.data[1,i] + test.data[2,i+1]
  
}  # end b-c dissimilarity

# check results
bdist

# bray curtis distance to jaccard conversion
abs(test.data[1,2] - test.data[2,2]) / test.data[1,2] + test.data[2,2]





#============================ ggplot pca visualization =========================

# rearranging dataset into ggplot format
study <- data.frame(Sample = rownames(pca$x),
                    x = pca$x[,1],
                    y = pca$x[,2])


# what does it look like?
study


# fancy plot
ggplot(data = as.data.frame(trial4), aes(x = x, y = y, label = Sample)) + 
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) + 
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) + 
  theme_bw() +
  ggtitle("proper fancy graph")



#============================ first pca trial ==================================

fpca(pca.test.data, "pca.test")

# expects samples to be rows, and variables to be columns (transpose if needec)
forest.pca <- prcomp(data.matrix(pca.test.data), scale=TRUE)  # returns x (PCs)

# What does the plot of our first two principal components look like?
plot(forest.pca$x[,1], forest.pca$x[,2], xlab = "PC 1", ylab = "PC 2",
     main = paste("PCs of", deparse(substitute(pca.test.data))))

# what is the variation covered by each PC?
forest.pca.var <- forest.pca$sdev^2

# what percent of the total variation is covered by each PC?
forest.pca.var.per <- round(forest.pca.var/sum(forest.pca.var)*100, 1)

# Scree plot of variation distribution among PCs
barplot(forest.pca.var.per, main = "Scree Plot", las = 1,
        xlab = "Principal Component", ylab = "Percent Variation")






#================== dendrogram stuff ===========================================

# jaccard's using the vegan package into an object for h.clustering
vd <- vegdist(binary1, method = "jaccard", upper = F)

# turning dist object into a hierarchical cluster using UPGMA
hc <- hclust(vd, method = "average")

# plotting the hclust/dendrogram
plot(hc, hang = -1, cex = 0.6, ylab = "Jaccard distance", xlab = "poop")

# using plot.dendrogram()
hcd <- as.dendrogram(hc)


plot(hcd, type = "rectangle", leaflab = "perpendicular", 
     nodePar = list(lab.cex = 0.7, pch = NA), 
     xlab = "Plots", ylab = "Jaccard distance")



# messing around with bootstrapping
set.seed(1234)
result <- pvclust(presence1 %>% data.matrix() %>% t(), 
                  method.dist = "binary",
                  method.hclust = "average",
                  nboot = 1000)

plot(result)


# install.packages("dendextend")
# install.packages("pvclust")
library(dendextend)
library(pvclust)


#================== dendro integration into Jaccard function ===================


# jaccard's using the vegan package into an object for h.clustering
vd <- vegdist(binary1, method = "jaccard", upper = F)

# turning dist object into a hierarchical cluster using UPGMA
hc <- hclust(vd, method = "average")

# using plot.dendrogram()
hcd <- as.dendrogram(hc)

plot(hcd, type = "rectangle", leaflab = "perpendicular", 
     nodePar = list(lab.cex = 0.7, pch = NA), 
     xlab = "Plots", ylab = "Jaccard distance", cex.lab = 1.05, las = 1,
     main = "Dendrogram of binary 1")




