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





#============================ initial data simulation ==========================
test.data <- data.frame("tree1" = rnorm(n = 46, mean = 180, sd = 12),
                        "tree2" = rnorm(n = 46, mean = 170, sd = 12),
                        "tree3" = rnorm(n = 46, mean = 150, sd = 12),
                        "tree4" = rnorm(n = 46, mean = 120, sd = 12),
                        "tree6" = rnorm(n = 46, mean = 130, sd = 12),
                        "tree6" = rnorm(n = 46, mean = 140, sd = 12),
                        stringsAsFactors = F)


#============================ trial 2 (different populations) ==================

# fewer variables (4), mean varies in 2
alpine1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 30, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 20, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 150, sd = 12), stringsAsFactors = F)

subalp1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 250, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 70, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 150, sd = 12), stringsAsFactors = F)

valley1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 20, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 300, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 150, sd = 12), stringsAsFactors = F)


#============================ trial 3 ==========================================

# fewer variables (3), two have variance
alpine2 <- data.frame(tree1 = rnorm(n = 11, mean = 150, sd = 12),
                      tree2 = rnorm(n = 11, mean = 30, sd = 12),
                      tree3 = rnorm(n = 11, mean = 20, sd = 12), stringsAsFactors = F)

subalp2 <- data.frame(tree1 = rnorm(n = 11, mean = 150, sd = 12),
                      tree2 = rnorm(n = 11, mean = 250, sd = 12),
                      tree3 = rnorm(n = 11, mean = 70, sd = 12), stringsAsFactors = F)

valley2 <- data.frame(tree1 = rnorm(n = 11, mean = 150, sd = 12),
                      tree2 = rnorm(n = 11, mean = 20, sd = 12),
                      tree3 = rnorm(n = 11, mean = 300, sd = 12), stringsAsFactors = F)

study2 <- rbind(alpine2, subalp2, valley2) %>% 
  data.matrix()




#============================ binary data for jaccard's ========================

test.pres <- data.frame("tree1" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree2" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree3" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree4" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree5" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree6" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        stringsAsFactors = F)



