
# function to add plot numbers to data matrix and save csvs 
fpca.ready <- function(plots.matrix, csv.name){  # adds plot nums to a matrix 
  rownames(plots.matrix) <- paste("plot", 1:nrow(plots.matrix), sep = "")
  write.csv(plots.matrix, paste(path.data.raw, csv.name, 
                                ".data.csv", sep = ""))
  return(plots.matrix)
}


#============================ initial data simulation ==========================
pca.test.data <- data.frame("tree1" = rnorm(n = 46, mean = 180, sd = 12),
                            "tree2" = rnorm(n = 46, mean = 170, sd = 12),
                            "tree3" = rnorm(n = 46, mean = 150, sd = 12),
                            "tree4" = rnorm(n = 46, mean = 120, sd = 12),
                            "tree6" = rnorm(n = 46, mean = 130, sd = 12),
                            "tree6" = rnorm(n = 46, mean = 140, sd = 12),
                            stringsAsFactors = F)

write.csv(pca.test.data, paste(path.data.raw, "pca.test.data.csv", sep = ""))


#============================ trial 2 (different populations) ==================

# fewer variables (4), mean varies in 2
alpine1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 30, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 20, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 150, sd = 12),
                      stringsAsFactors = F)

subalp1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 250, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 70, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 150, sd = 12), 
                      stringsAsFactors = F)

valley1 <- data.frame("tree1" = rnorm(n = 11, mean = 150, sd = 12),
                      "tree2" = rnorm(n = 11, mean = 20, sd = 12),
                      "tree3" = rnorm(n = 11, mean = 300, sd = 12),
                      "tree4" = rnorm(n = 11, mean = 150, sd = 12), 
                      stringsAsFactors = F)

study1 <- rbind(alpine1, subalp1, valley1) %>% 
  data.matrix() %>% fpca.ready(csv.name = "study1")



#============================ trial 3 ==========================================

# fewer variables (3), two have variance
alpine2 <- data.frame(tree1 = rnorm(n = 11, mean = 150, sd = 12),
                      tree2 = rnorm(n = 11, mean = 30, sd = 12),
                      tree3 = rnorm(n = 11, mean = 20, sd = 12), 
                      stringsAsFactors = F)

subalp2 <- data.frame(tree1 = rnorm(n = 11, mean = 150, sd = 12),
                      tree2 = rnorm(n = 11, mean = 250, sd = 12),
                      tree3 = rnorm(n = 11, mean = 70, sd = 12), 
                      stringsAsFactors = F)

valley2 <- data.frame(tree1 = rnorm(n = 11, mean = 150, sd = 12),
                      tree2 = rnorm(n = 11, mean = 20, sd = 12),
                      tree3 = rnorm(n = 11, mean = 300, sd = 12), 
                      stringsAsFactors = F)

study2 <- rbind(alpine2, subalp2, valley2) %>% 
  data.matrix() %>% fpca.ready(csv.name = "study2")





#============================ binary data for jaccard's ========================

presence1 <- data.frame("tree1" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree2" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree3" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree4" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree5" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree6" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        stringsAsFactors = F)

write.csv(presence1, paste(path.data.raw, "presence1.data.csv", sep = ""))


