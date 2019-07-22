## Code for linear regression
library(e1071)
library(neuralnet)
library(nnet)
library(fpc)
library(mclust)
library(ggplot)
library(flexclut)
library(lattice)
library(mvtnorm)
library(party)
library(rpart)
# read the imdb dataset

imdbData = read.csv("C:/Users/Bipin/Desktop/Movie-User-Rating-and-Gross-Income-Prediction-master/movie_metadata.csv")

# Get information about the dataset

str(imdbData)
imdbData$title_year = as.factor(imdbData$title_year)

# separate the numeric data columns from the Factor(string) data columns

num = sapply(imdbData, is.numeric)  # num contains numeric column indices
fact = sapply(imdbData, is.factor)  # fact contains the factor column indices
imdb_numeric = imdbData[, num]      # the numeric data
imdb_factor = imdbData[, fact]      # the factor(textual) data

# plot the histogram of the imdb_scores

hist(imdb_numeric$imdb_score,breaks = 20, col = "green") # Most of the scores lie in the range[5.5, 7.5]


# remove rows that have missing values

imdb_numeric_new <- na.omit(imdb_numeric) 

# scale the data

dat <- data.frame(lapply(imdb_numeric_new, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE))))

dat$imdb_score = imdb_numeric_new$imdb_score / 10

#split the data in to trainset and test set

dat = data.frame(dat)
index <- 1:nrow(dat)
testindex <- sample(index, trunc(length(index)*1/4))
testset <- dat[testindex,]
trainset <- dat[-testindex,]

# create a model using linear regression

lm1 = lm(imdb_score ~., data = trainset)

# Apply the model on the testset and get the predictted values

prediction = predict(lm1, testset[,-13])

# Calculate the Mean Square Error Values

mse <- mean((testset$imdb_score - prediction)^2)
print(mse)