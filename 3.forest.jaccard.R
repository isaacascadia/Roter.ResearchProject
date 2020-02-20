
fjaccard <- function(binary.dataframe){

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
    
    trees.i <- which(test.pres[n1,] == 1)
    trees.j <- which(test.pres[n2,] == 1)
    
# calculate jaccard index for pair
    j.sim$JI[i] <- length(intersect(trees.i, trees.j)) / 
      unique(c(trees.i, trees.j)) %>% length()
    
    
  }  # end of jaccard index loop
  
  return(j.sim)
  
}  # end jaccard function



fjaccard(test.pres)

similarities <- fjaccard(test.pres)

sort(similarities$JI)










# Jaccard distance based on Bray-Curtis dissimilarity for quantities (vegan)


# INCOMPLETE

bdist <- rep(NA, nrows(test.data))

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



