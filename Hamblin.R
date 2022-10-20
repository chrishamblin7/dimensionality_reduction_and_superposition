#Chris Hamblin
#Stats 2070 midterm R code

##PLEASE READ the report.rmd file, nothing in this document will really make sense 
##without the contextual information contained in the report.


## PACKAGES:
if(!require("pacman")) {install.packages("pacman"); require("pacman")} 
p_load("rstudioapi", ## setting working dir 
       "MASS", # for mvrnorm() -- multi variate normal dist
       "smacof", ## MDS
       "corrplot", ## for corrplot(function) 
       "rgl", ## for plot3D() and text3D()
       "colorspace",
       "ggplot2",
       "gridExtra",
       "reshape2", ## turning a dist object into a dataframe 
       "patchwork",
       "RcppCNPy",
       "umap",
       "Rtsne",
       "reticulate",
       "viridis",
       "RColorBrewer",
       "nat")

current_path <- getActiveDocumentContext()$path    ## gets path for current R script
setwd(dirname(current_path))                       ## sets dir to R script path

##SEED

set.seed(2)




Noisify <- function(data) {
  noise_amount <- mean(data)/(10^6) # small amount of noise relative to the data magnitude
  
  
  if (is.vector(data)) {
    noise <- runif(length(data), -noise_amount, noise_amount)
    noisified <- data + noise
  } else {
    length <- dim(data)[1] * dim(data)[2]
    noise <- matrix(runif(length, -0.0001, 0.00001), dim(data)[1])
    noisified <- data + noise
  }
  return(noisified)
}



#MDS of high dimensional unit vectors

par(mar=c(2,2,2,2),pty="s")


#plotting layout
layout_m <- matrix(c(1:14), 
nrow = 2, ncol = 7, byrow = FALSE)

layout(layout_m)

#generate mds of more and more unit vectors in 2D,
#then look at resultant configuration and stress
dimensions = c(3,4,8,20,40,60)
for (i in 1:length(dimensions)) {
  d <- dimensions[i]
  print(d)
  data <-  diag(d) #30 distinct features, each with their own dimension
  distances <-  dist(diag(d)) #gives all off-diagonal distance as sqrt(2), what we'd expect for 2 orthogonal unit vectors.
  mdsFit <- mds(distances)
  ###IF YOU GET ERROR
  #'Lapack routine dgesv: system is exactly singular: U[2,2] = 0'
  #use comment code below instead
  #noisy_distances <- Noisify(distances)
  #mdsFit <- mds(noisy_distances, type = "interval", ndim = 2) 
  
  if (i == 1){  #axis labels
    plot(mdsFit$conf, main = paste("Original dims:", d), geom = "point",)
    plot(mdsFit, plot.type = "Shepard", col = 1:2)
  }
  else { #dont repeat axis labels
    plot(mdsFit$conf, 
         main = d,
         xlab = '',
         ylab = '',
         geom = "point",)
    plot(mdsFit, plot.type = "Shepard", col = 1:2, 
         main= '',
         xlab = '',
         ylab = '',
         )   
  }

}


## Stress increases as we try to pack in more orthogonal directions
## And plot tends towards some sort of homogenous circle. 
## Note for plot for 4 points, small noise is dominating the distances I think?
## Was expecting a more symmetrical arrangement, 
## like the equalateral triangle in 3 . . .


#We can do the same exact experiment with other 
#common dimensionality techniques: tSNE and UMAP
#These nonlinear methods should introduce some sort of biased prior,
#This could probably be worked out analytically, but lets just see what happens

#lets consider the problem for the high dimensional case (60 dims), under the
#presumption that the homogenous circle MDS solution is optimal. Well check what
# happens across multiple hyper-parameter settings


## UMAP

#plotting layout
layout_m <- matrix(c(1:18), 
nrow = 3, ncol = 6, byrow = FALSE)
layout(layout_m)



umap_config <- umap.defaults
n_neighbors_list <- c(2,5,10,20,30,40)
min_dist_list <- c(.05,.1,.5)
data <-  diag(60) #60 distinct features, each with their own dimension
#we dont need distance matrix or added noise this time, umap with take the
#original data


for (n_neighbors in n_neighbors_list) {
  for (min_dist in min_dist_list) {
    umap_config$n_neighbors <- n_neighbors
    umap_config$min_dist <- min_dist
    
    umapFit <- umap(data, config = umap_config)
    #umaps[i] <- umapFit
    plot(umapFit$layout,
         main = paste(n_neighbors,min_dist, sep=" : "),
         xlab='',
         ylab='')
  }
  
}

## Seems like as we go towards the bottom right, with 
# a large number of neighbors and large minimum neighbors. 
#This makes sense as any neighborhood structure is artificial
# in the uniform data, and a large min distance also prevents clumping
# Lets see how this scales with dimensionality as before
umap_config <- umap.defaults

#plotting layout
layout_m <- matrix(c(1:10), 
nrow = 2, ncol = 5, byrow=TRUE)
layout(layout_m)

#generate UMAP of more and more unit vectors in 2D,
#then look at resultant configuration and stress
dimensions = c(3,4,6,10,15,20,40,60,100,200)

for (i in 1:length(dimensions)) {
  d <- dimensions[i]
  print(d)
  data <-  diag(d) 
  
  umap_config <- umap.defaults
  if (umap_config$n_neighbors>d) {
    umap_config$n_neighbors <- d-1
  }
  umapFit <- umap(data,config=umap_config)

  plot(umapFit$layout,
       main = d,
       xlab='',
       ylab='')
  
}


# seems like pretty similar solutions to MDS, more sensible fits in the low dimensional
#case, but that could just be due to the added noise.


#t-SNE

#Finally lets check biases introduced by t-SNE in the same manner

#hyper-param sweep

#plotting layout
layout_m <- matrix(c(1:15), 
nrow = 3, ncol = 5, byrow = TRUE)
layout(layout_m)


perplexity_list <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)

data <-  diag(60) #60 distinct features, each with their own dimension
#we dont need distance matrix or added noise this time, umap with take the
#original data

for (perplexity in perplexity_list) {
  
  tsneFit <- Rtsne(data,perplexity=perplexity)
  plot(tsneFit$Y,
       main = perplexity,
       xlab='',
       ylab='')

  
}

#again perhaps this is what would except, small perplexity 
#(small local neighborhoods) leads to more artifacts on 
#symmetrical data. But with large neighborhoods we approach
#the optimal MDS solution

#and as before across dimensions:
#plotting layout
layout_m <- matrix(c(1:10), 
  nrow = 2, ncol = 5, byrow=TRUE)



layout(layout_m)

#generate mds of more and more unit vectors in 2D,
#then look at resultant configuration and stress
dimensions = c(3,4,6,10,15,20,40,60,100,200)

for (i in 1:length(dimensions)) {
  d <- dimensions[i]
  print(d)
  data <-  diag(d) 
  
  perplexity = floor(d - 1) / 3
  
  tsneFit <- Rtsne(data,perplexity=perplexity)
  plot(tsneFit$Y,
       main = d,
       xlab='',
       ylab='')
  
  
}


#at 20 and 40 t-SNE introduces some wild artifacts,
#theres a big effect of the ratio of dimensions and perplexity
#maybe we should stay away from t-SNE for the remainder of
#the experiments, as we want to isolate the look of artifacts
#coming from superposition, not from what ever weird geometries
#t-SNE is coming up with




##Superposition data

#This data was generated using the colab notebook:
#latent_data_generation.ipynb

#load data
load("all_data.Rdata")


# This data takes 60 dimensional basis vectors, and makes a neural network that
#compresses them as per the superposition paper. We have data for 
#6 different embeddings sizes, under the names below;

embedding_names <- c("60to3.npy",
                     "60to5.npy",
                     "60to10.npy",
                     "60to20.npy",
                     "60to30.npy",
                     "60to40.npy"
                     )


#For each embedding size we also have 6 'sparsities' from an exponential 
#distibution: 1.0000,0.5493,0.3017,0.1657,0.0910,0.0500
#See the report to understand what this metric means

# so for each of the mappings above, we get a tensor with dimensions;
# 6 (sparsities) x 60 (original dimensions) x n (embedding dimensions)

dim(all_data$'60to10.npy')



#we can play the same game as before, the 'correct' projection of 
#the original features should be a homogenous circle. But we can 
#see how a network compressing the symmetrical orthogonal data 
#with superposition might lead to the appearance of a latent space
# with structure given conventional visualization techniques

layout_m <- matrix(c(1:36), 
                   nrow = 6, ncol = 6, byrow = TRUE)
layout(layout_m)



sparsities <- c(1.0,.55,.30,.17,.09,.05)
embedding_ds <- c(3,5,10,20,30,40)


for (i in 1:length(embedding_ds)) {
  d <- embedding_ds[i]
  print(d)
  for (j in 1:length(sparsities)) {
    s <- sparsities[j]
    data <- all_data[[embedding_names[i]]][j,,]
    distances <-  dist(data)
    mdsFit <- mds(distances, type = "interval", ndim = 2) 
    plot(mdsFit$conf, 
         main = paste(d,s,sep=' : '),
         xlab = '',
         ylab = '',
         geom="point")
  }
}




##with variable feature importance (from exponential curve,like sparsity)

embedding_names <- c("60to3_withimportance.npy",
                     "60to5_withimportance.npy",
                     "60to10_withimportance.npy",
                     "60to20_withimportance.npy",
                     "60to30_withimportance.npy",
                     "60to40_withimportance.npy")



#lets first check our wtw plots, that they look like those
#in the original paper;
par(mar=c(1,1,1,1))

layout_m <- matrix(c(1:36), 
                   nrow = 6, ncol = 6, byrow = TRUE)
layout(layout_m)


for (i in 1:length(embedding_ds)) {
  d <- embedding_ds[i]
  print(d)
  for (j in 1:length(sparsities)) {
    s <- sparsities[j]
    W <- all_data[[embedding_names[i]]][j,,]
    image(flip(W%*%t(W),flipdim="Y"), axes=FALSE, main=paste(d,s,sep=' : '),zlim=c(-4,4),  col = rev(brewer.pal(n=11,name = "RdBu")))
  }
}


#they look similar, although 60 in 40 dimensions seems pretty
#easy for the model, with superposition only of the unimportant
#features


#Lets plot the MDS embeddings


par(mar=c(1.5,1.5,1.5,1.5),pty="s")

for (i in 1:length(embedding_ds)) {
  d <- embedding_ds[i]
  #print(d)
  for (j in 1:length(sparsities)) {
    s <- sparsities[j]
    data <- all_data[[embedding_names[i]]][j,,]
    distances <-  dist(data)
    mdsFit <- mds(distances, type = "interval", ndim = 2) 

    plot(mdsFit$conf, 
         main = paste(d,s,sep=' : '),
         xlab = '',
         ylab = '',
         geom = "point",
         col = inferno(60),
         cex=.3)
  }
}

### Looks like at low embedding dimensionality and 
#at low sparsity we see a signature of 'dropped' features
#with unimportant features going to 0. However,
#There is no clear signature of superposition, perhaps
# in 2 dimensions, superposition geometries project symmetrically

#The 0 embeddings at the could potentially be hiding the interesting geometry,
#as every represented data point must be configured to 
#preserve its distance from all the points at the origin
#Lets do the same thing, but exclude the unrepresented points
#at the origin from the configuration:

for (i in 1:length(embedding_ds)) {
  d <- embedding_ds[i]
  #print(d)
  for (j in 1:length(sparsities)) {
    s <- sparsities[j]
    data <- all_data[[embedding_names[i]]][j,,]
    thresh <- mean(abs(data[1,]))/20
    rep_indices <- c()
    for (k in 1:nrow(data)) {
      if (mean(abs(data[k,])) > thresh) {
        rep_indices <- c(rep_indices,k)
      }
    }
    data <- data[rep_indices,]
    distances <-  dist(data)
    mdsFit <- mds(distances, type = "interval", ndim = 2) 
    
    plot(mdsFit$conf, 
         main = paste(d,s,sep=' : '),
         xlab = '',
         ylab = '',
         geom = "point",
         col = inferno(60),
         cex=.3)
  }
}




#with umap?


#with all points
for (i in 1:length(embedding_ds)) {
  d <- embedding_ds[i]
  print(d)
  for (j in 1:length(sparsities)) {
    s <- sparsities[j]
    data <- all_data[[embedding_names[i]]][j,,]
    
    umap_config <- umap.defaults
    umap_config$min_dist <-  .0001
    umapFit <- umap(data,config=umap_config)

    plot(umapFit$layout, 
         main = paste(d,s,sep=' : '),
         xlab = '',
         ylab = '',
         geom = "point",
         col = inferno(60))
  }
}



#with nonzero points only


for (i in 1:length(embedding_ds)) {
  d <- embedding_ds[i]
  print(d)
  for (j in 1:length(sparsities)) {
    s <- sparsities[j]
    data <- all_data[[embedding_names[i]]][j,,]
    thresh <- mean(abs(data[1,]))/20
    rep_indices <- c()
    for (k in 1:nrow(data)) {
      if (mean(abs(data[k,])) > thresh) {
        rep_indices <- c(rep_indices,k)
      }
    }
    data <- data[rep_indices,]
    umap_config <- umap.defaults
    umap_config$n_neighbors <- min(nrow(data)-1,umap_config$n_neighbors)
    umapFit <- umap(data,config=umap_config)
    
    plot(umapFit$layout, 
         main = paste(d,s,sep=' : '),
         xlab = '',
         ylab = '',
         geom = "point",
         col = inferno(60))
  }
}






