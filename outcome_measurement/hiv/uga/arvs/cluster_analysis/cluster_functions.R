# Functions for conducting a k-means cluster analysis
# 10/11/19
# Caitlin O'Brien-Carelli

# ----------------------
# function to create an elbow plots

elbow_fun = function(x, y, z) {
  withinss = map_dbl(y:z, function(k) {
  model = kmeans(x = x, center = k)
  model$tot.withinss
}) 
  
elbow_df = data.frame(
  k = y:z, 
  tot_withinss = withinss)

return(elbow_df) }
#---------------------------
# function to calculate silhouette widths 
# this functions takes three arguments:
# x = a matrix on which you want to select clusters
# y - z = the range of clusters (usually 2:10)

sil_fun = function(x, y, z) {
    sil_width = map_dbl(y:z, function(k) {
    model = pam(x = x, k = k)
    model$silinfo$avg.width
  }) 
  sil_df = data.frame(
    k = 2:10, 
    sil_width = sil_width)
    return(sil_df)
}

# test_elbow = elbow_fun(test_k, 2, 10)
# test_df = sil_fun(test_k, 2, 10)

















