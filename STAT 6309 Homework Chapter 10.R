# Question 7

library(ISLR)
set.seed(1)
dsc = scale(USArrests)
d1 = dist(dsc)^2
d2 = as.dist(1 - cor(t(dsc)))
d = d2/d1
summary(d)

# Question 8

# part a

# standard deviation of prcomp output
out = prcomp(USArrests, scale = TRUE)
var = out$sdev^2
pve = var/sum(var)
sum(var) #4
print(pve)
# 0.62006039 0.24744129 0.08914080 0.04335752

# part b

# Principle component loadings
loading = out$rotation
USArrests2 = scale(USArrests)
sum_var = sum(apply(as.matrix(USArrests2)^2, 2, sum))
apply((as.matrix(USArrests2) %*% loading)^2, 2, sum) / sum_var
# 0.62006039 0.24744129 0.08914080 0.04335752 

# Question 9
set.seed(2)

# part a
hc = hclust(dist(USArrests), method = "complete")
plot(hc)

# part b
cutree(hc, 3)

# part c
data = scale(USArrests)
hc_sd = hclust(dist(data), method = "complete")
plot(hc_sd)

# part d
cutree(hc_sd, 3)
table(cutree(hc, 3), cutree(hc_sd, 3))

# Question 10

set.seed(2)

# part a
x = matrix(rnorm(20 * 3 * 50, mean = 0, sd = 0.001), ncol = 50)
x[1:20, 2] = 1
x[21:40, 1] = 2
x[21:40, 2] = 2
x[41:60, 1] = 1
labels = c(rep(1, 20), rep(2, 20), rep(3, 20))

# part b
out = prcomp(x)
plot(out$x[,1:2], col = 1:3, xlab = "z1", ylab = "z2", pch = 15)

# part c
k = kmeans(x, 3, nstart = 20) #with 3
table(labels, k$cluster)

# part d
k = kmeans(x, 2, nstart = 20) #with 2
table(labels, k$cluster)

# part e
k = kmeans(x, 4, nstart = 20) #with 4
table(labels, k$cluster)

# part f
k = kmeans(out$x[, 1:2], 3, nstart = 20)
table(labels, k$cluster)

# part g
k = kmeans(scale(x), 3, nstart = 20)
table(labels, k$cluster)

# Question 11

# part a
genes = read.csv("Documents/SMU/2019-20/Spring/STAT 6309/Homework/Homework Chapter 10/Ch10Ex11.csv", header = FALSE)

# part b

# type 1
hc_complete = hclust(as.dist(1 - cor(genes)), method = "complete")
plot(hc_complete)

# type 2
hc_single = hclust(as.dist(1 - cor(genes)), method = "single")
plot(hc_single)

# type 3
hc_average = hclust(as.dist(1 - cor(genes)), method = "average")
plot(hc_average)

# part c
out = prcomp(t(genes))
head(out$rotation)

# find the abs value
load = apply(out$rotation, 1, sum)
index = order(abs(load), decreasing = TRUE)
index[1:10]
# 865  68 911 428 624  11 524 803 980 822

