#This script plays with null models as a way to learn functions and
  #practice loops.


#Let's start by optimizing the code for simple ploting

data(iris)
#head(iris)

setosa <- subset(iris, Species == "setosa")
plot(setosa$Sepal.Length ~ setosa$Petal.Length, las = 1,
     xlab = "Sepal length", ylab = "Petal length")
versicolor <- subset(iris, Species == "versicolor")
plot(versicolor$Sepal.Length ~ versicolor$Petal.Length, las = 1,
     xlab = "Sepal length", ylab = "Petal length")
virginica <- subset(iris, Species == "virginica")
plot(virginica$Sepal.Length ~ virginica$Petal.Length, las = 1,
     xlab = "Sepal length", ylab = "Petal length")

#Better
species <- c("setosa", "versicolor", "virginica")
#species <- unique(iris$Species)

for (i in species) {
  temp <- subset(iris, Species == i)
  plot(temp$Sepal.Length ~ temp$Petal.Length, las = 1,
       xlab = "Sepal length", ylab = "Petal length")
}

#Even better
plot_species <- function(sp, data) {
  temp <- subset(data, Species == sp)
  plot(temp$Sepal.Length ~ temp$Petal.Length, las = 1,
       xlab = "Sepal length", ylab = "Petal length")
  }
lapply(species, plot_species, data = iris)
#or
purrr::map(species, plot_species, data = iris)

#Or if data is heavy
library(future.apply)
plan(multisession)
future_lapply(species, plot_species, data = iris)


#A simple null model used to test correlations----

#The data
abundance <- c(1,3,4,7,8,13)
body_size <- c(9,6,3,3,1,1)
plot(abundance ~ body_size)
(corr <- cor(abundance,body_size))
cor.test(abundance,body_size)

#The null model:
#idea: Brake all processes except for the process of interest and compare the
#observed with the expected data.

#In this case we want to test if this correlation may be due to randomness.
#We brake then the "body_size" process and left only randomness. use `sample()`

cor(sample(body_size, size = length(body_size), replace = FALSE), abundance)

#we do it 1000 times
cor_dis <- c()
for (k in 1:1000){
  cor_dis[k] <- cor(sample(body_size, size = length(body_size), replace = FALSE), abundance)
}

#plot
hist(cor_dis)
lines(c(corr, corr), c(0,200), col = "red")

#test p-value
(p <- pnorm(corr, mean = mean(cor_dis), sd = sd(cor_dis)))
#Or using the real distribution directly
length(cor_dis[which(cor_dis < corr)])/1000

#Another example----
#We observe a pattern: How uneven are abundance distributions?
abundance <- c(1,3,4,7,8,13)
#calculate pielou's evenees (shannon/logarithm of number of species)
#Shannon The proportion of species i relative to the total number of species
#(pi) is calculated, and then multiplied by the natural logarithm of
#this proportion (lnpi). The resulting product is summed across species,
#and multiplied by -1:
J <- function(v){
  p <- v/sum(v)
  S <- -sum(p * log(p))
  S/log(length(v))
}
eve <- J(abundance)
#is this eveness higher than expected?
#calculate evenness of a random assembly
#tip: we need to sample 36 individuals and assign them a species randomly
#then, we can group per species.
rand <- sample(c(1:6), sum(abundance), replace = TRUE)
abundance2 <- table(rand)
#abundance2 <- hist(rand, breaks = seq(0,6,1))$counts #this does the same, plots store data!
#Calculate J of the simulated community
J2 <- J(abundance2)
# now make it 1000 times
out <- c()
for(i in 1:100){
  rand <- sample(c(1:6), sum(abundance), replace = TRUE)
  abundance2 <- hist(rand, breaks = seq(0,6,1))$counts
  J2 <- J(abundance2)
  out <- c(out, J2)
}
#calculate p-value
hist(out)
lines(c(eve, eve), c(0,20), col = "red")
(p <- pnorm(eve, mean = mean(out, na.rm = TRUE), sd = sd(out, na.rm = TRUE)))
#Or using the real distribution directly
length(out[which(out < eve)])/1000

#functionalize it
null_function <- function(x, iterations = 1000){
  # make a loop
  n <- length(x)
  eve <- J(x)
  out_bs <- c()
  for(i in 1:iterations){
    rand <- sample(c(1:n), sum(x), replace = TRUE)
    x2 <- table(rand)
    J2 <- J(x2)
    out_bs <- c(out_bs, J2)
  }
  #And test significance
  out <- length(out_bs[which(out_bs < eve)])/iterations
  print(paste("The eveness value observed is ",
              round(eve, digits = 2), "and the probablility of ocurring by chance is ",
              round(out, digits = 2)))
}

null_function(c(1,2,3,4,5,6,7,8,9))
null_function(round(runif(100, 30,100)))

#make a package!


#null model 2.
#We want to test now if body size is driving the evenness patterns.
#we create a null model where body_size is the only responsible of the
#observed pattern
abundance <- c(1,3,4,7,8,13)
body_size <- c(9,6,5,3,2,1)
#Do one iteration
sample(c(1:6), 36, replace = TRUE, prob = body_size)
# make a loop
out_bs <- c()
for(i in 1:1000){
  rand <- sample(c(1:6), 36, replace = TRUE, prob = body_size)
  abundance2 <- hist(rand, breaks = seq(0,6,1))$counts
  J2 <- J(abundance2)
  out_bs <- c(out_bs, J2)
}
#And test significance
hist(out_bs)
lines(c(eve, eve), c(0,400), col = "red")
(p <- pnorm(eve, mean = mean(out_bs, na.rm = TRUE), sd = sd(out_bs, na.rm = TRUE)))
(p <- 1- pnorm(eve, mean = mean(out_bs, na.rm = TRUE), sd = sd(out_bs, na.rm = TRUE)))
#we remove NA's as in function J, log(richness) is NA when a species abundance is 0.
#basically we ignore those cases, as we are only interested in communities with all species
#having positive abundances.
#Or using the real distribution directly
length(out_bs[which(out_bs < eve)])/1000

#Functionalize it

null_function2 <- function(x, cond, iterations = 1000){
  # make a loop
  n <- length(x)
  eve <- J(x)
  out_bs <- c()
  for(i in 1:iterations){
    rand <- sample(c(1:n), sum(x), replace = TRUE, prob = cond)
    abundance2 <- table(rand)
    J2 <- J(abundance2)
    out_bs <- c(out_bs, J2)
  }
  #And test significance
  out <- length(out_bs[which(out_bs < eve)])/iterations
  print(paste("The eveness value observed is ",
              round(eve, digits = 2), "and the probablility of ocurring by the condition is ",
              round(out, digits = 2)))
}

null_function2(x = abundance, cond = body_size)

