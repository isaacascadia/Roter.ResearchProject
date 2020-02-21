

#============================ pca function =====================================
# add argument for plot name

fpca <- function(samps.vars){  # function definition
# add plot numbers
  rownames(samps.vars) <- paste("plot", 1:nrow(samps.vars), sep = "")
  
# expects samples to be rows, and variables to be columns (transpose if needed)
  pca <- prcomp(samps.vars, scale = TRUE)  # returns x (PCs)
  
# What does the plot of our first two principal components look like?
  plot(pca$x[,1], pca$x[,2], xlab = "PC 1", ylab = "PC 2",
       main = "PCs")
  
# what is the variation covered by each PC?
  pca.var <- pca$sdev^2
  
# what percent of the total variation is covered by each PC?
  pca.var.per <- round(pca.var/sum(pca.var) * 100, 1)
  
# Scree plot of variation distribution among PCs
  barplot(pca.var.per, main = "Scree Plot", las = 1,
          xlab = "Principal Component", ylab = "Percent Variation",
          names.arg = colnames(pca$x), cex.names = 0.9)
  
loading.scores <- pca$rotation[,1]
var.scores <- abs(loading.scores)  # get the magnitudes
var.score.ranked <- sort(var.scores, decreasing = T)


if(ncol(matrix) <= 5){top.vars <- names(var.score.ranked[1:3])}
if(ncol(matrix) >= 6 && ncol(matrix) <= 10){top.vars <- 
  names(var.score.ranked[1:5])}
if(ncol(matrix) >= 11){top.vars <- names(var.score.ranked[1:10])}


print("most important variables") # names of most important variables

return(pca$rotation[top.vars,1])  # names and scores of most important variables

  
}  # end of pca function

fpca(study2)


#============================ first pca attempt, one population ================

# expects samples to be rows, and variables to be columns (transpose if needec)
forest.pca <- prcomp(data.matrix(test.data), scale=TRUE)  # returns x (PCs)

# What does the plot of our first two principal components look like?
plot(forest.pca$x[,1], forest.pca$x[,2], xlab = "PC 1", ylab = "PC 2",
     main = paste("PCs of", deparse(substitute(test.data))))

# what is the variation covered by each PC?
forest.pca.var <- forest.pca$sdev^2

# what percent of the total variation is covered by each PC?
forest.pca.var.per <- round(forest.pca.var/sum(forest.pca.var)*100, 1)

# Scree plot of variation distribution among PCs
barplot(forest.pca.var.per, main = "Scree Plot", las = 1,
        xlab = "Principal Component", ylab = "Percent Variation")



#============================ second attempt, 3 populations ====================
# tree 3 mean varies, remainder are the same

study1 <- rbind(alpine1, subalp1, valley1)

# calculating the PCs
pca2 <- prcomp(data.matrix(study1), scale = TRUE)  # returns x (PCs)

# What does the plot of our first two principal components look like?
plot(pca2$x[,1], pca2$x[,2], xlab = "PC 1", ylab = "PC 2",
     main = paste("PCs of", deparse(substitute(study1))))


# what is the variation covered by each PC?
pca2.var <- pca2$sdev^2

# what percent of the total variation is covered by each PC?
pca2.var.per <- round(pca2.var/sum(pca2.var)*100, 1)

# Scree plot of variation distribution among PCs
barplot(pca2.var.per, main = "Scree Plot", las = 1,
        xlab = "Principal Component", ylab = "Percent Variation")




#============================ third attempt, 3 variables =======================
# tree 3 mean varies, remainder are the same

study.matrix <- rbind(alpine2, subalp2, valley2) %>% 
  data.matrix()

rownames(study.matrix) <- paste("plot", 1:nrow(study), sep = "")


# calculating the PCs
pca <- prcomp(study.matrix, scale=TRUE)  # returns x (PCs)

# What does the plot of our first two principal components look like?
plot(pca$x[,1], pca$x[,2], xlab = "PC 1", ylab = "PC 2",
     main = paste("PCs of", deparse(substitute(study1))))


# what is the variation covered by each PC?
pca.var <- pca$sdev^2

# what percent of the total variation is covered by each PC?
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)

# Scree plot of variation distribution among PCs
barplot(pca.var.per, main = "Scree Plot", las = 1,
        xlab = "Principal Component", ylab = "Percent Variation")




#============================ ggplot visualization =============================

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













