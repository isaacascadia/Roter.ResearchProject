# Data Simulation variables
trees <- c("tree1", "tree2", "tree3", "tree4")
n.trees <- c(20, 20, 20, 20)
mean.trees <- c(180, 170, 150, 120)
sd.trees <- c(12, 12, 12, 12)



tree.abund <- rep(NA, sum(n.trees))

for(i in 1:length(trees)){
  
  tree.abund <- c(tree.abund, rnorm(n.trees[i], mean.trees[i], sd.trees[i]))
  }



# simulating data manually
test.data <- data.frame("tree1" = floor(rnorm(n = 46, mean = 180, sd = 12)),
                        "tree2" = rnorm(n = 46, mean = 170, sd = 12),
                        "tree3" = rnorm(n = 46, mean = 150, sd = 12),
                        "tree4" = rnorm(n = 46, mean = 120, sd = 12),
                        "tree6" = rnorm(n = 46, mean = 130, sd = 12),
                        "tree6" = rnorm(n = 46, mean = 140, sd = 12),
                        stringsAsFactors = F)

hist(x = test.data$tree1, col = "darkgreen")


test.pres <- data.frame("tree1" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree2" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree3" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree4" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree5" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        "tree6" = round(rnorm(n = 46, mean = 0.5, sd = 0.25)),
                        stringsAsFactors = F)





