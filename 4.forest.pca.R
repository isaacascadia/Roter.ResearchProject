# expects samples to be rows, and variables to be columns (transposition)
forest.pca <- prcomp(data.matrix(test.data), scale=TRUE)  # returns x (PCs)

# What does the plot of our first two principal components look like?
plot(forest.pca$x[,1], forest.pca$x[,2])

# what is the variation covered by each PC?
forest.pca.var <- forest.pca$sdev^2

# what percent of the total variation is covered by each PC?
forest.pca.var.per <- round(forest.pca.var/sum(forest.pca.var)*100, 1)

# Scree plot of variation distribution among PCs
barplot(forest.pca.var.per, main = "Scree Plot", las = 1,
        xlab = "Principal Component", ylab = "Percent Variation")



#============================ ggplot visualization ============================

# rearranging dataset into ggplot format
forest.pca.data <- data.frame(sample = rownames(pca$x),
                              x = pca$x[,1],
                              y = pca$x[,2])

# what does it look like?
forest.pca.data


