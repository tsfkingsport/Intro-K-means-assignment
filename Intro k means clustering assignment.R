# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle.data","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine2 <- wine[,-1]
wine2 <-scale(wine2)

# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine2)

# Exercise 2:
#   * How many clusters does this method suggest?
#   3 based on the "elbow" of the graph

#   * Why does this method work? What's the intuition behind it?
#   It looks to see how many clusters you need for more clusters to start
#   giving diminishing returns on the sum of squares

#   * Look at the code for wssplot() and figure out how it works
#  It creates a function with a default number of clusters and default seed
#to make it easier to reproduce. My knowledge on the statistics is a bit weak
#so I don't know the formula being used from memory.  

#The *sum(apply(data,2,var)) confuses me, as far as I can tell it means to 
#take the sum of all of the numbers created by taking the variance of all 
#of the columns. I say columns due to the number 2 in the middle of apply.
#I do not know why the for section has 2:nc. I know this means iterate from
#the second to the number nc but I don't know why someone would pick the second
#as opposed to the first or what different it would make. I know it makes a 
#difference I am just not sure what the difference is. 

#set.seed is fairly obvious, set the seed number for easy reproducability
#After that the I am confused on line 30.  After that is just plotting 
#the graph

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine2, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
#         3

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

 fit.km <- kmeans(wine2, 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$cluster
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster)
table(wine$Type)

# I do not remember how to evaluate clustering models effectively. I remember doing
#confusion squares but I do not think that applies to k-means specifically, or maybe I'm wrong


# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
library(cluster)
clusplot(fit.km)

#I tried multiple things in the clusplot function and I am doing something wrong
#but I do not know what. 
