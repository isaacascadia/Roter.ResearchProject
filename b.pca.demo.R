
# simulating three different populations across 4 variables
alpine1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 30, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 20, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 100, sd = 12), 
                      stringsAsFactors = F)

subalp1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 250, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 70, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 50, sd = 12), 
                      stringsAsFactors = F)

valley1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 20, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 300, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 250, sd = 12), 
                      stringsAsFactors = F)


# converting individual populations into whole dataset as a matrix
study1 <- rbind(alpine1, subalp1, valley1) %>% 
  data.matrix()

study1


# add plot numbers
rownames(study1) <- paste("plot", 1:nrow(study1), sep = "")





# expects samples to be rows, and variables to be columns (transpose if needed)
pca <- prcomp(study1, scale = TRUE)  # returns x (PCs)

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

# how much does each variable influence PC?
loading.scores <- pca$rotation[,1]
loading.scores

# what is the magnitude of the variable influence?

var.scores <- abs(loading.scores)  # get the magnitudes
var.scores

# what are the magnitudes in order?
var.score.ranked <- sort(var.scores, decreasing = T)
var.score.ranked

# what are variables with the highest loading scores?
if(ncol(study1) <= 5){top.vars <- names(var.score.ranked[1:3])}
if(ncol(study1) >= 6 && ncol(matrix) <= 10){top.vars <- 
  names(var.score.ranked[1:5])}
if(ncol(study1) >= 11){top.vars <- names(var.score.ranked[1:10])}

top.vars



pca$rotation[top.vars,1]  # names and scores of most important variables
