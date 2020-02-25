


#================== binary data for jaccard's ==================================

binary1 <- fsimulate(name = "binary1", n.vars = 6,
                     alp.n = 7, alp.m = rep(x = 0.5, times = 6), 
                     alp.sd = rep(0.25, 6),
                     sub.n = 7, sub.m = rep(0.5, 6), sub.sd = rep(0.25, 6),
                     val.n = 7, val.m = rep(0.5, 6), val.sd = rep(0.25, 6)) %>% 
  round()

fjaccard(binary1, "binary 1")




#================== binary data for jaccard's ==================================

binary2 <- fsimulate(name = "binary2", n.vars = 6,
                     alp.n = 7, alp.m = rep(x = 0.8, times = 6), 
                     alp.sd = rep(0.25, 6),
                     sub.n = 7, sub.m = rep(0.5, 6), sub.sd = rep(0.25, 6),
                     val.n = 7, val.m = rep(0.7, 6), val.sd = rep(0.25, 6)) %>% 
  round()

fjaccard(binary2, "binary 2")



#================== first pca data simulation and trial ========================

# simulating one population - consistent proportions of species means 
# across all areas. Low SDs.

trial1 <- fsimulate(name = "trial1", n.vars = 6,
                    alp.n = 46, alp.m = rep(135, 6), 
                    alp.sd = rep(1, 6), 
                    sub.n = 46, sub.m = rep(135, 6), 
                    sub.sd = rep(1, 6),
                    val.n = 46, val.m = rep(135, 6), 
                    val.sd = rep(1, 6))
fpca(trial1, "trial 1")

# [1] "highest loading scores"
# species2   species6   species1   species3   species5 
# -0.6227149  0.6045992  0.3813914 -0.2408017  0.1791551 

# loading scores - species 2 and 6 are most influential, but this is random
# scree plot - first two PCs together only capture ~40% of total variation
# PC scatterplot - widely distributed points, some clustering toward center
# no differences to see here, folks!

#================== trial 2 (different populations) ============================

# three populations, first variable varies hugely between populations, 
# while the remainder stay consistent


trial2 <- fsimulate(name = "trial2", n.vars = 6,
                    alp.n = 46, alp.m = c(1, 135, 135, 135, 135, 135), 
                    alp.sd = rep(1, 6), 
                    sub.n = 46, sub.m = c(30000, 135, 135, 135, 135, 135), 
                    sub.sd = rep(1, 6),
                    val.n = 46, val.m = c(70000, 135, 135, 135, 135, 135), 
                    val.sd = rep(1, 6))

fpca(trial2, "trial 2")

# [1] "highest loading scores"
# species6   species2   species1   species5   species3 
# 0.6691136 -0.5377409  0.3907840  0.2336353 -0.2313142 

# loading scores - species 6 and 2 are most influential, even though species 1 
#   varies hugely between populations
# scree plot - first two PCs together only capture ~42% of total variation,
#   only slightly more than with all variables consistent
# PC scatterplot - widely distributed points, some clustering toward center
# still no differences to see here, folks!

#================== trial 3 ====================================================

# two variables vary hugely, remainder are consistent

trial3 <- fsimulate(name = "trial3", n.vars = 6,
                    alp.n = 46, alp.m = c(1, 70000, 135, 135, 135, 135), 
                    alp.sd = rep(1, 6), 
                    sub.n = 46, sub.m = c(30000, 30000, 135, 135, 135, 135), 
                    sub.sd = rep(1, 6),
                    val.n = 46, val.m = c(70000, 1, 135, 135, 135, 135), 
                    val.sd = rep(1, 6))

fpca(trial3, "trial 3")

# [1] "highest loading scores"
# species1   species2   species6   species4   species3 
# 0.6799005 -0.6780242  0.2120718  0.1420278 -0.0880981 

# loading scores - species 1 and 2 now most influential, reflecting variation
# scree plot - first two PCs together  capture ~53% of total variation,
#   only slightly more than with all variables consistent
#   PC1 captures ~2x more than others > eigenvector can now capture variation.
# scatterplot - 3 distinct populations, separated along PC1, still lots of 
#   variation along PC2, including some distinct clusters within populations
# we've got different populations, folks!

#================== trial 4 ====================================================

# one variable varies hugely, but there are only 3 variables

trial4 <- fsimulate(name = "trial4", n.vars = 3,
                    alp.n = 46, alp.m = c(1, 135, 135), 
                    alp.sd = rep(1, 6), 
                    sub.n = 46, sub.m = c(30000, 135, 135), 
                    sub.sd = rep(1, 6),
                    val.n = 46, val.m = c(70000, 135, 135), 
                    val.sd = rep(1, 6))

fpca(trial4, "trial 4")

# [1] "highest loading scores"
# species3   species1   species2 
# 0.7530434 -0.5561007  0.3516783

# loading scores - species 3 is most influential, despite sp1 varying the most 
# scree plot - first two PCs together  capture ~66% of total variation,
#   so they describe it fairly well, but PC1 isn't really any better 
# scatter plot - widely distributed points, loosely clustered toward center
# no differences to see here, folks.



#================== trial 5 only 1/6 varying variables =========================


# one variable varies hugely, but there are only 2 variables

trial5 <- fsimulate(name = "trial5", n.vars = 2,
                    alp.n = 46, alp.m = c(1, 135), 
                    alp.sd = rep(1, 6), 
                    sub.n = 46, sub.m = c(30000, 135), 
                    sub.sd = rep(1, 6),
                    val.n = 46, val.m = c(70000, 135), 
                    val.sd = rep(1, 6))

fpca(trial5, "trial 5")

# [1] "highest loading scores"
# species2  species1 
# 0.7071068 0.7071068

# loading scores - both species are identically influential 
# scree plot - each PC captures about 50% variation, PC1 only slightly more
# scatter plot - 3 distinct lines of points, with only minor clustering within
#   
# no differences to see here, folks.


#================== trial 6 1/6 varying variables, more variance ===============


trial6 <- fsimulate(name = "trial6", 
                    alp.means = c(0, 100, 100, 100, 100, 100),
                    alp.sd = rep(x = 8, times = 6),
                    sub.means = c(200, 100, 100, 100, 100, 100),
                    sub.sd = rep(x = 8, times = 6),
                    val.means = c(400, 100, 100, 100, 100, 100),
                    val.sd = rep(x = 8, times = 6),
                    samps = 90, 
                    vars = 6)

fpca(trial6, "trial6")


#================== trial 4 (now with data simulation function !) ==============


trial7 <- fsimulate(name = "trial7", 
                    alp.means = c(50, 100, 150, 300, 10, 275),
                    alp.sd = rep(x = 12, times = 6),
                    sub.means = c(100, 50, 300, 150, 200, 30),
                    sub.sd = rep(x = 12, times = 6),
                    val.means = c(200, 10, 50, 200, 100, 180),
                    val.sd = rep(x = 12, times = 6),
                    samps = 90, 
                    vars = 6)

fpca(trial7, "trial 7")










