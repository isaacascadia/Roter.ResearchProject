#============================ automated simulation exploration =================
# data simulation variables
trees <- c("tree1", "tree2", "tree3", "tree4")
n.trees <- rep(20, 4)
mean.trees <- c(180, 170, 150, 120)
sd.trees <- c(12, 12, 12, 12)



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



# jaccard's using the vegan package
vegdist(test.data, method = "jaccard", upper = F)




#============================ ggplot pca visualization =========================

# rearranging dataset into ggplot format
study <- data.frame(Sample = rownames(pca$x),
                    x = pca$x[,1],
                    y = pca$x[,2])


# what does it look like?
study


# fancy plot
ggplot(data = study, aes(x = x, y = y, label = Sample)) + 
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



