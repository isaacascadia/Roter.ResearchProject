#============================ prep dataframe for pca function ==================

# function to add plot numbers to data matrix and save csvs 
fpca.ready <- function(plots.matrix, csv.name){  # adds plot nums to a matrix 
  rownames(plots.matrix) <- paste("plot", 1:nrow(plots.matrix), sep = "")
  write.csv(plots.matrix, paste(path.data.raw, csv.name, 
                                ".data.csv", sep = ""))
  return(plots.matrix)
}



#================== data simulation function ===================================

# defining the function fsimulate()
fsimulate <- function(name, n.vars,              # dataset name - files/figs
                      alp.n, alp.m, alp.sd,   # pop 1 sample characteristics
                      sub.n, sub.m, sub.sd,   # pop 2 sample characteristics
                      val.n, val.m, val.sd){  # pop 3 sample characteristics
  
  simul.data <- matrix(nrow = (alp.n + sub.n + val.n), ncol = n.vars)
  rownames(simul.data) <- c(paste("alp", 1:alp.n, sep = ""),
                            paste("sub", 1:sub.n, sep = ""),
                            paste("val", 1:val.n, sep = ""))
  colnames(simul.data) <- paste("species", 1:n.vars, sep = "")
  
  seeds <- 1:n.vars * 1234
  
  for(i in 1:n.vars){
    set.seed(seeds[i])
    alp.values <- rnorm(n = alp.n, m = alp.m[i], sd = alp.sd[i])
    set.seed(seeds[i] + 1)
    sub.values <- rnorm(n = sub.n, m = sub.m[i], sd = sub.sd[i])
    set.seed(seeds[i] + 2)
    val.values <- rnorm(n = val.n, m = val.m[i], sd = val.sd[i])
    
    simul.data[,i] <- c(alp.values, sub.values, val.values)
  } # end of data simulation loop
  
  # saving simulated data
  write.csv(simul.data, paste(path.data.raw, name, 
                              ".data.csv", sep = ""))
  # return simulated data
  return(simul.data)
  
} # end of simulation function





#================== jaccard's similarity and heatmap function ==================

# function for calculating jaccard's similarity for a presence/absence dataset
# and outputting a heatmap

fjaccard <- function(binary.dataframe, ji.name){
  

  n.plots <- nrow(binary.dataframe)
  
  # how many combinations are there
  combs <- combinations(n = nrow(binary.dataframe), r = 2)
  
  combs <- rbind(combs, matrix(rep(x = 1:n.plots, 2), 
                               nrow = n.plots, ncol = 2))
  
  # create an empty matrix
  j.output <- matrix(data = NA, 
                     nrow = n.plots, 
                     ncol = n.plots)
  
  # row and column names
  rownames(j.output) <- paste("plot", 1:n.plots, sep = "")
  colnames(j.output) <- paste("plot", 1:n.plots, sep = "")
  
  
  # loop definition, repeats for every possible combination of samples in datset
  for(i in 1:nrow(combs)){
    
    
    # which rows (plots) are being compared?
    n1 <- combs[i,1]
    n2 <- combs[i,2]
    
    trees.i <- which(binary.dataframe[n1,] == 1)
    trees.j <- which(binary.dataframe[n2,] == 1)
    
    if(length(c(trees.i, trees.j)) == 0){
      j.output[n1, n2] <- 0
      j.output[n2, n1] <- 0
    } else {  # end if, beginning else
      
      # calculate jaccard index for pair
      j.output[n1, n2] <- length(intersect(trees.i, trees.j)) / 
        unique(c(trees.i, trees.j)) %>% length()
      
      j.output[n2, n1] <- length(intersect(trees.i, trees.j)) / 
        unique(c(trees.i, trees.j)) %>% length()
    }  # end else
    
    
  }  # end of jaccard index loop
  
  
  if(dev.cur() > 1){
    dev.off()
  }
  
  # begin saving of heatmap
  pdf(paste(path.figures, gsub(" ", "", ji.name), ".heatmap.pdf", sep = ""),
      width = 5, height = 5)
  
  # display heatmap to be saved
  pheatmap(j.output, main =  paste(ji.name, "JI scores"), 
           legend_labels = c("JI Score"), 
           cluster_rows = F, cluster_cols = F)
  
  dev.off()  # finish saving plots
  
  # save data output as a .csv
  write.csv(j.output, paste(path.data.output, gsub(" ", "", ji.name), 
                            ".jaccard.csv", sep = ""))
  
  # display output dataframe
  return(j.output)
  
}  # end jaccard function


#================== jaccard's distance and heatmap function ====================

# function for calculating jaccard's distance for a presence/absence dataset
# and outputting a heatmap
fjaccard.dist <- function(binary.dataframe, ji.name){
  
  n.plots <- nrow(binary.dataframe)
  
  # how many combinations are there
  combs <- combinations(n = nrow(binary.dataframe), r = 2)
  
  combs <- rbind(combs, matrix(rep(x = 1:n.plots, 2), 
                               nrow = n.plots, ncol = 2))
  
  # create an empty matrix
  j.dist.output <- matrix(data = NA, 
                     nrow = n.plots, 
                     ncol = n.plots)
  
  # row and column names
  rownames(j.dist.output) <- paste("plot", 1:n.plots, sep = "")
  colnames(j.dist.output) <- paste("plot", 1:n.plots, sep = "")
  
  
  
  # loop definition, repeats for every possible combination of samples in datset
  for(i in 1:nrow(combs)){
    
    
    # which rows (plots) are being compared?
    n1 <- combs[i,1]
    n2 <- combs[i,2]
    
    trees.i <- which(binary.dataframe[n1,] == 1)
    trees.j <- which(binary.dataframe[n2,] == 1)
    
    # calculate jaccard index for pair
    j.dist.output[n1, n2] <- 1 - length(intersect(trees.i, trees.j)) / 
      unique(c(trees.i, trees.j)) %>% length()
    
    j.dist.output[n2, n1] <- 1 - length(intersect(trees.i, trees.j)) / 
      unique(c(trees.i, trees.j)) %>% length()
    
    
    
  }  # end of jaccard index loop
  
  # display heatmap to user in RStudio graphic window
  pheatmap(j.dist.output, main =  paste(ji.name, "JI scores"), 
           legend_labels = c("JI Score"), 
           cluster_rows = F, cluster_cols = F)
  
  # begin saving of heatmap
  pdf(paste(path.figures, gsub(" ", "", ji.name), ".heatmap.pdf", sep = ""),
      width = 5, height = 5) 
  
  # display heatmap to be saved
  pheatmap(j.dist.output, main =  paste(ji.name, "JI scores"), 
           legend_labels = c("JI Score"), 
           cluster_rows = F, cluster_cols = F)
  
  dev.off()  # finish saving plots
  
  # save data output as a .csv
  write.csv(j.dist.output, paste(path.data.output, gsub(" ", "", ji.name), 
                            ".jaccard.csv", sep = ""))
  
  # display output dataframe
  return(j.dist.output)
  
}  # end jaccard function


#============================ pca function =====================================

# what does this function do?



# samps.vars is a matrix class object
fpca <- function(samps.vars, pca.name){  # pca.name: char type > plot/pdf titles
  
  # expects samples to be rows, and variables to be columns 
  pca <- prcomp(samps.vars, scale = TRUE)  # returns x (PCs)
  
  # what is the variation covered by each PC?
  pca.var <- pca$sdev^2
  
  # what percent of the total variation is covered by each PC?
  pca.var.per <- round(pca.var/sum(pca.var) * 100, 1)
  
  
  # begin saving of pca plots
  pdf(paste(path.figures, gsub(" ", "", pca.name), ".pca.plots.pdf", sep = ""),
      width = 5, height = 5) 
  
  # What does the plot of our first two principal components look like?
  plot(pca$x[,1], pca$x[,2], 
       xlab = paste("PC1 - ", pca.var.per[1], "%", sep=""), 
       ylab = paste("PC2 - ", pca.var.per[2], "%", sep=""),
       main = paste("PCs of", pca.name, sep = " "))
  
  
  
  # Scree plot of variation distribution among PCs
  barplot(pca.var.per, 
          main = paste("Scree Plot of", pca.name, sep = " "), 
          xlab = "Principal Component", cex.names = 0.9, las = 1,
          ylab = "Percent Variation", names.arg = colnames(pca$x),
          ylim = c(0, max(pca.var.per) + 10))   
  
  dev.off()  # finish saving plots
  
  
  # display pc plots in RStudio graphics window
  # What does the plot of our first two principal components look like?
  plot(pca$x[,1], pca$x[,2], 
       xlab = paste("PC1 - ", pca.var.per[1], "%", sep=""), 
       ylab = paste("PC2 - ", pca.var.per[2], "%", sep=""),
       main = paste("PCs of", pca.name, sep = " "))
  
  # Scree plot of variation distribution among PCs
  barplot(pca.var.per, 
          main = paste("Scree Plot of", pca.name, sep = " "), 
          xlab = "Principal Component", cex.names = 0.9, las = 1,
          ylab = "Percent Variation", names.arg = colnames(pca$x),
          ylim = c(0, max(pca.var.per) + 10))  
  
  
  # loading scores goodness
  loading.scores <- pca$rotation[,1]                    # display loading scores
  var.scores <- abs(loading.scores)                     # loading magnitudes
  var.score.ranked <- sort(var.scores, decreasing = T)  # ordered loading scores
  
  # determine appropriate number of loading scores to display
  if(ncol(samps.vars) <= 5){top.vars <- 
    names(var.score.ranked[1:ncol(samps.vars)])}
  if(ncol(samps.vars) >= 6 && ncol(samps.vars) <= 10){top.vars <- 
    names(var.score.ranked[1:5])}
  if(ncol(samps.vars) >= 11){top.vars <- names(var.score.ranked[1:10])}
  
  # save loading scores as .csv
  write.csv(pca$rotation, paste(path.data.output, gsub(" ", "", pca.name), 
                                ".loading.csv", sep = ""))
  
  # save principle component values as .csv
  write.csv(pca$x, paste(path.data.output, gsub(" ", "", pca.name), 
                         ".pc.values.csv", sep = ""))
  

  print("highest loading scores")   # names of most important variables
  
  return(pca$rotation[top.vars,1])  # names, scores of most important variables
  
  
}  # end of pca function
