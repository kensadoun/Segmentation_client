



library(tidyverse)
library(ggplot2)
library(dplyr)
library(cluster)
library(purrr)


cdata <- read.csv ("C:/Users/asus/Desktop/pr/Mall_Customers.csv")
head(cdata)

dim(cdata)
str(cdata)
names(cdata)

summary(cdata$Age)
sd(cdata$Age)

summary(cdata$Annual.Income..k..)
sd(cdata$Annual.Income..k..)

summary(cdata$Spending.Score..1.100.)
sd(cdata$Spending.Score..1.100.)

a=table(cdata$Gender)
barplot(a, main="display gender comparison", ylab = "count", xlab = "gender",col= rainbow(2), legend= rownames(a))

age_pie <- ggplot(cdata, aes(x= "", fill = cut(Age, breaks = seq(0, 100, by=10)))) +
  geom_bar(width=1, color="white")+
  coord_polar("y", start=0)+
  labs(title = "   Age distribution", fill ="age")+
  theme_void()
  print(age_pie)
  
  
ggplot (cdata, aes(x = Annual.Income..k..)) + 
  geom_histogram(binwidth = 8, fill="pink", color ="black", alpha = 0.7)
  labs(title = " Annual income distibution",
       x ="annual income", 
       y ="frequency") +
    theme_minimal()
  
ggplot(cdata, aes(x = Annual.Income..k.., fill = "Annual Income")) +
  geom_density(alpha = 0.5) +
  geom_density(aes(x = Spending.Score..1.100., fill = "Spending Score"), alpha = 0.5) +
  labs(title = "Density plot of Annual Income and Spending Score",
       x = "Values",
       y = "Density") +
  scale_fill_manual(values = c("Annual Income" = "blue", "Spending Score" = "red")) +
  theme_minimal()
  

#________________

ggplot(cdata, aes( x =Spending.Score..1.100. , y = Annual.Income..k.. , color = Age)) +
  geom_point() +
  labs(title = "Scatter point of age, income and spending", 
       x = "Spending" ,
       y = " Annual income" ,
       color = "Age") +
  theme_minimal()


#___________________

#kmeans_algo 
#optimal_cluster_elbow_method 

set.seed(123)
iss <- function(k) {
  kmeans(cdata[, 3:5],k,iter.max = 1000, nstart = 1000, algorithm = "Lloyd")$tot.withinss
}
 k.values <- 1:10 
 
 iss_values <- map_dbl(k.values, iss)

plot (k.values, iss_values, 
      type = "b" , pch = 19, frame =FALSE,
      xlab = "number of cluster k", 
      ylab = "total intra-cluster sum of squares")



#_______________________





variables <- cdata [, c("Spending.Score..1.100.", "Annual.Income..k..")]
scaled_var <- scale (variables)

k <- 5
kmeans_result <- kmeans(scaled_var, centers = k)

cat("cluster centers (age, spending score, annual income):\n")
print(kmeans_result$centers)

cat("\nAffectation of the cluster for each observation:\n")
print(kmeans_result$cluster)


cdata$Cluster <- kmeans_result$cluster

ggplot( cdata, aes(x = Annual.Income..k.. , y = Spending.Score..1.100., color = factor(Cluster))) +
  geom_point() +
  labs (title = "Scatter plot of the cluster", 
        x = "Annual income",
        y = "Spending Score") +
  
  theme_minimal()



