## Code for Artificial Neural Networks

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

hist(imdb_numeric$imdb_score,breaks = 40, col = "yellow") # Most of the scores lie in the range[5.5, 7.5]


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

# create the formula for neural networks

myform <- as.formula(paste0('imdb_score ~ ',paste(names(trainset[!names(trainset) %in% 'imdb_score']), collapse = ' + ')))

# train the neural network with 4 hidden layers using the trainset

net.sqrt <- neuralnet(myform, trainset, hidden = 4)

#plot the obtained neural network

plot(net.sqrt)

# Apply the Neural Network model on the testset

comp <- compute(net.sqrt, testset[,-13])

# Calculate the Mean Squared Error

mse <- mean((testset$imdb_score - comp$net.result)^2)
print(mse)