library(tidyverse)
renv::snapshot()
data(iris)
head(iris)
summary(iris)
str(iris)
library(GGally)

#plots
ggpairs(iris, aes(color = Species))

#remove nas and duplicates
clean.data <- iris %>% drop_na() %>% unique()
summary(clean.data)

#aggregate to summarise each variable
iris %>% group_by(Species) %>% summarize_all(mean)

#random sampling - identify which rows fromthe dataset should be taken
take <- sample(seq(nrow(iris)), size = 15)
iris[take, ] #all the rows from take & all cols from iris

#scatterplot matrix using a subset of iris
set.seed(1000) #ensure consistent random sampling
s <- iris %>% slice_sample(n = 15)
ggpairs(s, aes(color = Species))

#3D scatterplot of iris
# library(plotly) # I don't load the package because it's namespace clashes with select in dplyr.
plotly::plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, z = ~Sepal.Width,
                size = ~Petal.Width, color = ~Species, type="scatter3d")

#PCA
pc <- iris %>% select(-Species) %>% as.matrix() %>% prcomp()
summary(pc)
plot(pc, type = "line") #line graph
str(pc)

#reduce 4 variables to two pc
iris_projected <- as_tibble(pc$x) %>% add_column(Species = iris$Species)
ggplot(iris_projected, aes(x = PC1, y = PC2, color = Species)) + 
  geom_point()

#scree plot - displays relative contribution of each pc to the total variance
library(factoextra)
fviz_pca(pc)
fviz_pca_var(pc)
d <- iris %>% select(-Species) %>% dist() #calculate Euclidean distances b/w each pair of obvs
fit <- cmdscale(d, k = 2) #k = no. components
colnames(fit) <- c("comp1", "comp2")
fit <- as_tibble(fit) %>% add_column(Species = iris$Species)

ggplot(fit, aes(x = comp1, y = comp2, color = Species)) + geom_point()
ggplot(iris, aes(x = Petal.Width)) + geom_histogram(binwidth = .2)

#package for analysing and visualising data for transactions
#i.e. shopping based w/ milk, butter, bread or a set of co-occurring items in a data set
#good for identifying patterns and trends
library(arules) 
iris %>% pull(Petal.Width) %>% discretize(method = "interval", breaks = 3) #divides data into 3 discrete intervals
#plot
ggplot(iris, aes(Petal.Width)) + geom_histogram(binwidth = .2) +
  geom_vline(xintercept =
               iris %>% pull(Petal.Width) %>% discretize(method = "interval", breaks = 3, onlycuts = TRUE),
             color = "blue") +
  labs(title = "Discretization: interval", subtitle = "Blue lines are boundaries")

#Standardise data
#interates through each column, if the col is numeric it is transformed using the scale function (all values are between -1 and 1)
scale_numeric <- function(x) x %>% mutate_if(is.numeric, function(y) as.vector(scale(y)))

iris.scaled <- iris %>% scale_numeric()
iris.scaled
summary(iris.scaled)

#Distances for binary data
b <- rbind(
  c(0,0,0,1,1,1,1,0,0,1),
  c(0,0,1,1,1,0,0,1,0,0)
)
b

b_logical <- apply(b, MARGIN = 2, as.logical)
b_logical

dist(b, method = "manhattan")
dist(b, method = "euclidean")^2
dist(b, method = "binary")

people <- tibble(
  height = c(      160,    185,    170),
  weight = c(       52,     90,     75),
  sex    = c( "female", "male", "male")
)
people

library(proxy)
library(caret)
data_dummy <- dummyVars(~., people) %>% predict(people) #makes dummy variables and attaches to original data frame
data_dummy

weight_matrix <- matrix(c(1, 1, 1/2, 1/2), ncol = 4, nrow = nrow(data_dummy), byrow = TRUE)
data_dummy_scaled <- scale(data_dummy) * weight_matrix

d_dummy <- dist(data_dummy_scaled)
d_dummy

#Correlation
cc <- iris %>% select(-Species) %>% cor()
cc
ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  geom_point() +
  geom_smooth(method = "lm")
with(iris, cor(Petal.Length, Petal.Width)) #Pearson's correlation coefficient

#Grouping
iris %>% group_by(Species) %>% summarize(across(Sepal.Length, mean))
iris %>% group_by(Species) %>% summarize_all(mean)

#Boxplot
ggplot(iris, aes(Species, Sepal.Length)) + 
  geom_boxplot()

#Data matrix
iris_matrix <- iris %>% select(-Species) %>% as.matrix() #convert selected columns into a matrix
head(iris_matrix)
iris_long <- as_tibble(iris_matrix) %>% mutate(id = row_number()) %>% pivot_longer(1:4) #convert into tibble, add id column then change cols to rows
head(iris_long)
ggplot(iris_long,
       aes(x = name, y = id, fill = value)) + geom_tile()

#Correlation matrix
cm1 <- iris %>% select(-Species) %>% as.matrix %>% cor() #converts col to matrix then calcs correlation b/w the cols
cm1
library(ggcorrplot)
ggcorrplot(cm1)

#Parallel coordinates plot
library(GGally)
ggparcoord(iris, columns = 1:4, groupColumn = 5) #col = cols 1-4 of data set, group Col = 5th col

#test commit history


