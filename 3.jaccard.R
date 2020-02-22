
fjaccard <- function(binary.dataframe, csv.name){

# how many combinations are there
  combs <- combinations(n = nrow(binary.dataframe), r = 2)
  
  
  j.sim <- data.frame("comparison" = rep(NA, nrow(combs)), 
                      "JI" = rep(NA, nrow(combs)))
  
# loop definition
  for(i in 1:nrow(combs)){

# write name of compared pair
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
  
  write.csv(j.sim, paste(path.data.output, csv.name, 
                                ".jaccard.csv", sep = ""))
  return(j.sim)
  
}  # end jaccard function



fjaccard(presence1, "presence1")

similarities <- fjaccard(presence1, "presence1")

sort(similarities$JI)









