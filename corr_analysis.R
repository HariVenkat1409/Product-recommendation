## Code for Correaltion Analysis

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

hist(imdb_numeric$imdb_score,breaks = 10, col = "blue") # Most of the scores lie in the range[5.5, 7.5]

#correlation analysis is performed on the numeric data and the output is displayed on the screen
cor(na.omit(imdb_numeric), use="complete.obs", method="pearson")
