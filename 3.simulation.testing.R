
# this file contains each dataset simulated using the fsimulate() function and 
# the Jaccard-based cluster analysis or PCA performed for that dataset, as well
# as notes on the results.


#================== jaccard index 1 ============================================

# initial binary dataset. all 3 populations have identical mean presences for 
# all species

binary1 <- fsimulate(name = "binary1", n.vars = 6,
                     alp.n = 7, alp.m = rep(x = 0.5, times = 6), 
                     alp.sd = rep(0.25, 6),
                     sub.n = 7, sub.m = rep(0.5, 6), sub.sd = rep(0.25, 6),
                     val.n = 7, val.m = rep(0.5, 6), val.sd = rep(0.25, 6)) %>% 
  round()

fjaccard(binary1, "binary 1")


# as expected, no clear results in heatmap or dendrogram


#================== jaccard index 2 ============================================

# similar to binary 1, except alp.m is higher, differentiating this population

binary2 <- fsimulate(name = "binary2", n.vars = 6,
                     alp.n = 7, alp.m = rep(0.9, 6), alp.sd = rep(0.25, 6),
                     sub.n = 7, sub.m = rep(0.5, 6), sub.sd = rep(0.25, 6),
                     val.n = 7, val.m = rep(0.5, 6), val.sd = rep(0.25, 6)) %>% 
  round()

fjaccard(binary2, "binary 2")

# clear clustering in heatmap, all comparisons between plots 1:7 are much more 
# red, while the remainder are fairly stochastic.
# dendrogram also clusters all alp plots 



#================== jaccard index 3 ============================================

# three distinct populations, still 6 variables

binary3 <- fsimulate(name = "binary3", n.vars = 6,
                     alp.n = 7, alp.m = c(0.9, 0.9, 0.5, 0.5, 0.5, 0.5), 
                     alp.sd = rep(0.25, 6),
                     sub.n = 7, sub.m = c(0.5, 0.5, 0.9, 0.9, 0.5, 0.5), 
                     sub.sd = rep(0.25, 6),
                     val.n = 7, val.m = c(0.5, 0.5, 0.5, 0.5, 0.9, 0.9), 
                     val.sd = rep(0.25, 6)) %>% 
  round()

fjaccard(binary3, "binary 3")

# heatmap: smaller (3 plot) clusters for alp and sub, possible but indistinct
# clustering for valley
# dendrogram: sub well clustered, valley clustered but mixed with alpine 



#================== jaccard index 4 ============================================

# three distinct populations, now 30 variables

binary4 <- fsimulate(name = "binary4", n.vars = 30,
                     alp.n = 7, alp.m = rep(c(0.9, 0.5, 0.5), each = 10), 
                     alp.sd = rep(0.25, 30),
                     sub.n = 7, sub.m = rep(c(0.5, 0.9, 0.5), each = 10), 
                     sub.sd = rep(0.25, 30),
                     val.n = 7, val.m = rep(c(0.9, 0.5, 0.9), each = 10), 
                     val.sd = rep(0.25, 30)) %>% 
  round()

fjaccard(binary4, "binary 4")

# heatmap: clear similarity clustering for valley, weak but distinct for sub,
# weak and only slightly distinct for alpine
# dendrogram: sub branches off at first node, alp and val more interrelated but
# still distinct in final clustering, except alp7 is in val, reflects heatmap



#================== jaccard index 5 ============================================

# three distinct populations, 30 variables, very low standard deviation

binary5 <- fsimulate(name = "binary5", n.vars = 30,
                     alp.n = 7, alp.m = rep(c(0.9, 0.5, 0.5), each = 10), 
                     alp.sd = rep(0.01, 30),
                     sub.n = 7, sub.m = rep(c(0.5, 0.9, 0.5), each = 10), 
                     sub.sd = rep(0.01, 30),
                     val.n = 7, val.m = rep(c(0.9, 0.5, 0.9), each = 10), 
                     val.sd = rep(0.01, 30)) %>% 
  round()

fjaccard(binary5, "binary 5")

# heatmap: distinct similarity clusters for all three groups, val is most 
# distinct, alp is least
# dendrogram: sub still branches off at first node, val and alp more clearly
# distinguished but alp3 is still mixed in with sub


#================== PCA data simulation and trial 1 ============================

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

#================== PCA trial 2 ================================================

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



#================== trial 5 ====================================================


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



#================== trial 6 ====================================================

# 6 variables, 2 vary in each group, rest are consistent

trial6 <- fsimulate(name = "trial6", n.vars = 6,
                    alp.n = 46, alp.m = rep(c(150, 135, 135), each = 2), 
                    alp.sd = rep(1, 6), 
                    sub.n = 46, sub.m = rep(c(135, 150, 135), each = 2), 
                    sub.sd = rep(1, 6),
                    val.n = 46, val.m = rep(c(135, 135, 150), each = 2), 
                    val.sd = rep(1, 6))

fpca(trial6, "trial 6")

# [1] "highest loading scores"
# species4   species3   species1   species2   species5 
# 0.5716909  0.5706700 -0.3449294 -0.3402583 -0.2415189

# loading scores - species 3 and 4 were most influential, varying species means
# for subalpine
# scatterplot - 3 clear clusters, 2 vary along PC2, one diverges along PC1
# scree plot - first 2 PCs capture 99% of variation

# lots of difference observable


#================== trial 7 ====================================================

# 6 variables, 1 varies in each group, rest are consistent

trial7 <- fsimulate(name = "trial7", n.vars = 6,
                    alp.n = 46, alp.m = c(150, 135, 135, 135, 135, 135), 
                    alp.sd = rep(1, 6), 
                    sub.n = 46, sub.m = c(135, 150, 135, 135, 135, 135), 
                    sub.sd = rep(1, 6),
                    val.n = 46, val.m = c(135, 135, 150, 135, 135, 135), 
                    val.sd = rep(1, 6))

fpca(trial7, "trial 7")

# [1] "highest loading scores"
# species3   species2   species6   species1   species4 
# 0.7094337 -0.5166924  0.3889796 -0.1871015  0.1785395

# loading scores - species 3 was most influential, divergent mean for val
# scatterplot - three distinct groups, each covers PC1 range by about 50%
# scree plot - first two PCs capture ~50%, PC3:6 each capture ~15%

# still difference is observable

#================== trial 8 ====================================================

# 90 variables, 3 distinct populations, each has 30 distinct species means, 
# but distinct species means are only 2.5 off from remainder of means

trial8 <- fsimulate(name = "trial8", n.vars = 90,
                    alp.n = 46, alp.m = rep(c(137.5, 135, 135), each = 30), 
                    alp.sd = rep(1, 90), 
                    sub.n = 46, sub.m = rep(c(135, 137.5, 135), each = 30), 
                    sub.sd = rep(1, 90),
                    val.n = 46, val.m = rep(c(135, 135, 137.5), each = 30), 
                    val.sd = rep(1, 90))

fpca(trial8, "trial8")


# [1] "highest loading scores"
# species42 species51 species38 species32 species54 species35 species59
# 0.1511003 0.1493604 0.1470872 0.1425704 0.1421839 0.1411947 0.1407716

# loading scores - top 10 highest loading scores pretty consistent, but all 
# between 32 and 59, the species varying in subalpine
# scatterplot - three distinct groups, two are consistent along PC2, all quite
# distinct along PC1
# scree plot - first two PCs capture ~60%, remainder are negligible but add up

# difference is clear









