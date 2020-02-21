# data Simulation variables
trees <- c("tree1", "tree2", "tree3", "tree4")
n.trees <- rep(20, 4)
mean.trees <- c(180, 170, 150, 120)
sd.trees <- c(12, 12, 12, 12)



alp.abund <- dataframe(trees[1] = rep(NA, 20),
                       trees[2] = rep(NA, 20),
                       trees[3] = rep(NA, 20),
                       trees[4] = rep(NA, 20))

for(i in 1:length(trees)){
  
  alp.abund[,i] <- c(tree.abund, rnorm(n.trees[i], mean.trees[i], sd.trees[i]))

}



# simulating data manually
test.data <- data.frame("tree1" = rnorm(n = 46, mean = 150, sd = 12),
                        "tree2" = rnorm(n = 46, mean = 150, sd = 8),
                        "tree3" = rnorm(n = 46, mean = 150, sd = 6),
                        "tree4" = rnorm(n = 46, mean = 150, sd = 100),
                        "tree5" = rnorm(n = 46, mean = 150, sd = 100),
                        "tree6" = rnorm(n = 46, mean = 150, sd = 8),
                        stringsAsFactors = F)



test.pres <- data.frame("tree1" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree2" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree3" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree4" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree5" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree6" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        stringsAsFactors = F)




hist(x = test.data$tree1, col = "darkgreen")



