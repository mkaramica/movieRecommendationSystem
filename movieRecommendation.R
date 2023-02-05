
#-------------------------------------------------------------------------------
### Part #1: Data Wrangling: Create edx and final_holdout_test sets 
#    (As described in the course)
#   Then the edx dataset will consequently be divided into train_set 
#   and validation_set
#-------------------------------------------------------------------------------


# Install required package if not installed:
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")

#Load requierd libraries:
library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

# Download the zipped file if it does not exist in the working directory:
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)


# Unzip the rating and movies files if they do not exist:----------------------
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)
# -----------------------------------------------------------------------------


# Extract data from ratings_file and store them into a data frame:
ratings <- as.data.frame(str_split(read_lines(ratings_file), 
                                   fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)

# Add columns names to ratings data frame:
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))


# Extract data from movies file and store them into a data frame:
movies <- as.data.frame(str_split(read_lines(movies_file), 
                                  fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)

# Add columns names to ratings data frame:
colnames(movies) <- c("movieId", "title", "genres")

# convert movieId column from string to integer:
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

# Combine ratings and movies into a new data frame: movielens
movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 

test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, p = 0.1, list = FALSE)

edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into 'edx' set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

# Up to here, We have divided the whole dataset ('movielens') into two sets:
#   1- 'edx' (For training and regularization)
#   2- 'final_holdout_test' (for final testing)


# The 'edx' dataset should be divided into 'train_set' and 'validation_set' 
# where the former is used to train the model while the latter is engaged for 
# tuning of the hyper parameter (here lambda), 
# which is the regularization factor.


# Divide 'edx' into train_set & 'validation_set' 
validation_index <- createDataPartition(y = edx$rating, 
                                        times = 1, p = 0.1, list = FALSE)


train_set <- edx[-validation_index,]
temp <- edx[validation_index,]

# Make sure userId and movieId in 'validation_set' are also in 'train_set'
validation_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from validation set back into train set
removed <- anti_join(temp, validation_set)
train_set <- rbind(train_set, removed)


# Keep required variables in memory and delete the rest of them:
rm(list = ls()[!(ls() %in% 
          c('edx','final_holdout_test', 'train_set', 'validation_set'))])

#-------------------------------------------------------------------------------
### Part #2: Inspect MovieLens Database:
#     1- Overview;
#     2- Movies;
#     3- Users;
#     4- Ratings;
#     5- Genres;
#-------------------------------------------------------------------------------

# Overview
# Show the number of rows in each dataset:
n_train <- print(nrow(train_set))
n_validation <- print(nrow(validation_set))
n_test <- print(nrow(final_holdout_test))
nTotal <- n_train + n_validation + n_test 
nList <- c(n_train,n_validation,n_test, nTotal)
nNames <- c("Train Set", "Validation Set", "Test Set", "Total (MovieLens)")
dataframeOverView <- data.frame(nNames, nList, rep(ncol(edx),4))
colnames(dataframeOverView) <- c("Data Frame", "No. Rows", "No. Columns")
head(dataframeOverView)
names(edx)


# Movies
movies <- edx %>% group_by(movieId, title, genres) %>% dplyr::summarize( nRatings = n())
nMovies <- nrow(movies)

minMovieRatings <- min(movies$nRatings)
maxMovieRating <- max(movies$nRatings)

sum(movies$nRatings == minMovieRatings)
sum(movies$nRatings == maxMovieRating)

movies %>% filter(nRatings == maxMovieRating ) %>% pull(movieId)
movies %>% filter(nRatings == maxMovieRating ) %>% pull(title)

mean(movies$nRatings)
median(movies$nRatings)


movies %>% 
  ggplot(aes(nRatings)) + 
  geom_histogram( bins=50, color = "blue") +
  scale_x_log10() + 
  ggtitle("Distribtution of the Number of Ratings Received by Movies") +
  xlab("Number of Ratings Received by a Movie") +
  ylab("Number of Movies")


# Users
users <- edx %>% group_by(userId) %>% dplyr::summarize( nRatings = n())
nUsers <- nrow(users)
nUsers

minUserRatings <- min(users$nRatings)
minUserRatings

maxUserRating <- max(users$nRatings)
maxUserRating

sum(users$nRatings == minUserRatings)
sum(users$nRatings == maxUserRating)


mean(users$nRatings)
median(users$nRatings)

users %>% 
  ggplot(aes(nRatings)) + 
  geom_histogram( bins=50, color = "blue") +
  scale_x_log10() + 
  ggtitle("Distribtution of the Number of Ratings given by Users") +
  xlab("Number of Ratings Given by a User") +
  ylab("Number of Users")

# Ratings
mean(edx$rating)
median(edx$rating)
sd(edx$rating)

edx %>% group_by(rating) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(x=rating, y=n)) +
  geom_bar(stat="identity", fill="blue") +
  ggtitle("Distribtution of Ratings in Database") +
  xlab("Rating") +
  ylab("Number of Ratings")


# Genres

# Number of different genres (Combinations of basic genres)
nrow(edx %>% group_by(genres) %>% summarize(n=n()))

# option #1: Run the follwoing script to get the list of genres--------------
# genresCol <- edx$genres
# genreSet <- c()
# 
# 
# for (iRow in 1:length(genresCol)) {
#   splitted <- unlist(strsplit(as.character(genresCol[iRow]),"\\|"))
#   for (iSplitted in 1:length(splitted)) {
#     genreSet <- append(genreSet, splitted[iSplitted])
#   }
#   genreSet <- unique(genreSet)
# }
# genreSet <- sort(genreSet)
#---------------------------------------------------------------------------

# option #2: Use the pre-found genres to save the runtime;
genreSet <- c( "(no genres listed)", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "Film-Noir", "Horror", "IMAX", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western" )

genreMoviesCount <- rep(0, length(genreSet))
genreRatingCount <- rep(0, length(genreSet))

for (iGenre in 1:length(genreSet)) {
  genreMoviesCount[iGenre] <- nrow(movies %>%
                                     filter(str_detect(genres, genreSet[iGenre])))
  
  genreRatingCount[iGenre] <- nrow(edx %>%
                                     filter(str_detect(genres, genreSet[iGenre])))
}


names(genreMoviesCount) <- genreSet
names(genreRatingCount) <- genreSet

genreMoviesCount <- sort(genreMoviesCount, decreasing = T)
genreRatingCount <- sort(genreRatingCount, decreasing = T)

# Plot margins from 4 sides:
par(mar = c(8, 5, 0, 0)) 

# X-Axis margins:
par(mgp = c(4, 0.5, 0))
barplot(genreMoviesCount, names.arg = names(genreMoviesCount), 
        xlab = "Basic Genres", ylab = "Number of Movies", las = 2, col="blue")



# Plot margins from 4 sides:
par(mar = c(8, 5, 0, 0)) 

# X-Axis margins:
par(mgp = c(4, 0.5, 0))

barplot(genreRatingCount, names.arg = names(genreRatingCount), 
        xlab = "Basic Genres", ylab = "Number of Ratings", las = 2, col="blue")
#-------------------------------------------------------------------------------

# Keep required variables in memory and delete the rest of them:
rm(list = ls()[!(ls() %in% 
          c('edx','final_holdout_test', 'train_set', 'validation_set'))])

#-------------------------------------------------------------------------------
### Part #3: Define appropriate functions including:
#     1- Building the model;
#     2- Predicting the user rating of a given dataset with a given model;
#     3- Calculating RMSE for a predicted set of result;
#     4- Calculating RMSE for a given database with a given built model;
#     5- Calculating RMSE of the validation set for a given lambda;
#-------------------------------------------------------------------------------


buildLinearModel <- function(givenDataBase, lambda=0) {
#   This function takes a dataset and a regularization factor, 
#   lambda (with default value of zero) and build a linear model.
#   It uses the regularization concept to reduce the over-fitting 
#   of the model on the training set. The optimal value of lambda 
#   is computed by considering the RMSE of the different models
#   applied on the validation_set.
   
  
  
  # Calculate the average of all given ratings:
  ratingAVG <- mean(givenDataBase$rating)
  
  # Calculate the movie bias of each movie and store it in a new data frame
  # called movies_df:
  movies_df <- givenDataBase %>% 
    group_by(movieId) %>% 
    dplyr::summarize( movieBias = sum(rating - ratingAVG)/(n() + lambda) )
  
  # Calculate the user bias of each user and store it in a new data frame
  # called users_df:
  users_df <- givenDataBase %>% 
    left_join(movies_df, by='movieId') %>%
    group_by(userId) %>%
    dplyr::summarize( userBias = sum(rating - ratingAVG - movieBias)/
                        (n() + lambda) )
  
  # Calculate the genre bias of each genre and store it in a new data frame
  # called genres_df:
  genres_df <- givenDataBase %>% 
    left_join(movies_df, by='movieId') %>%
    left_join(users_df, by='userId') %>%
    group_by(genres) %>% 
    dplyr::summarize(genreBias = 
                       sum(rating - ratingAVG - movieBias - userBias) / 
                (n() + lambda) )
  
  # Combine all the calculated data frames into a list and return it:
  builtModel <- list(ratingAVG = ratingAVG, movies_df = movies_df, 
                     users_df = users_df, genres_df = genres_df)
  
  return(builtModel)
  
}

doPredictionWithModel <- function(givenDataBase, givenModel, option) {
#   This function takes a pre-built model as well as a dataset, 
#   and calculates the predicted rating for dataset based on the
#   given linear model. 
#   It limits the calculated ratings in range (0.5,5), as the 
#   actual rating system does.
#   The input model is compatible with the output format of 
#   the 'buildLinearModel' function.
  
# The option indicates how much details are considered in the calculation:
#    option =1: The rating avg only
#    option =2: The rating avg + movies bias
#    option =3: The rating avg + movies bias + users' bias
#    option =4,5: The rating avg + movies bias + users' bias + genres bias
#       hint! option 5 is used for considering regularization parameter.  

  # Define the lower and upper limits of the calculated ratings:  
  rateMin <- 0.5
  rateMax <- 5
  
  # Unpacking the model into its components:
  ratingAVG <- givenModel$ratingAVG
  movies_df <- givenModel$movies_df
  users_df <- givenModel$users_df
  genres_df<- givenModel$genres_df
  
  # Considering options for calculation:
  
  movieBiascoeff <- 0
  userBiascoeff <- 0
  genreBiascoeff <- 0
  
  if (option > 1 ) movieBiascoeff <- 1
  if (option > 2 ) userBiascoeff <- 1
  if (option > 3 ) genreBiascoeff <- 1
  
  
  # Calculate the predicted ratings:
  predicted_ratings <- givenDataBase %>% 
    left_join(movies_df, by='movieId') %>%
    left_join(users_df, by='userId') %>%
    left_join(genres_df, by = 'genres') %>%
    mutate(pred = ratingAVG + 
             movieBiascoeff*movieBias + 
             userBiascoeff*userBias + 
             genreBiascoeff*genreBias) %>%
    mutate(pred = ifelse(pred < rateMin, rateMin, 
                         ifelse(pred > rateMax, rateMax, pred))) %>%
    pull(pred)
  
  return (predicted_ratings)
}

calcRMSE <- function(true_ratings, predicted_ratings){
#   This function calculates the Root-mean-square error (RMSE) 
#   for the calculated ratings vs. the true ratings.
  
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

calcPredictionError <- function(givenDataBase, givenModel, option) {
#   This function returns the RMSE for a given database 
#   when its actual ratings are compared with those 
#   predicted by the given model.

  predicted <- doPredictionWithModel(givenDataBase, givenModel, option)
  err <- calcRMSE(predicted, givenDataBase$rating)
  
  return(err)
}

regularizationErr <- function(lambda) {
#   This function takes the regularization parameter (lambda) 
#   and calculates the RMSE of predicting ratings for the 
#   validation set on a model achieved by the train set if 
#   the given lambda is considered.
  
  currentModel <- buildLinearModel(train_set, lambda = lambda)
  err <- calcPredictionError(validation_set,currentModel, option=4)
  return(err)
}



#-------------------------------------------------------------------------------
### Part #4: Obtain the recommendation system based on the linear model.

# For options #1 to #4, the results are computed by assuming lambda=0.

#   The following steps will be performed for option #5:

#   1- Find the optimized value for regularization parameter (lambda)
#       1-1- Guess a lambda
#       1-2- Build a model by the train set and lambda
#       1-3- Calculate the RMSE of the validation set by the resulted model
#       1-4- Modify the value of lambda and go back to step 1-2 until reaching 
#            a satisfactory result.

#   2- Build a model based on the optimized lambda and edx database 
#   3- Calculate the predicted ratings of the final_holdout_test based on the 
#      resulted model in step 2

#    4- Report the RMSE of the predicted ratings compared to the true ratings
#-------------------------------------------------------------------------------

# Build the linear model without regularization:
model_NoRegul <- buildLinearModel(edx, lambda=0)
#-------------------------------------------------------------------------------

# Option #1: considering average of ratings only -------------------------------

# on edx dataset:
predicted_edx_option1 <- doPredictionWithModel(edx, model_NoRegul, option=1)
err_edx_option1 <- calcRMSE(edx$rating, predicted_edx_option1)

#on final_holdout_test:
predicted_test_option1 <- doPredictionWithModel(final_holdout_test, 
                                                model_NoRegul, option=1)

err_test_option1 <- calcRMSE(final_holdout_test$rating, predicted_test_option1)
#-------------------------------------------------------------------------------

# Option #2: considering average of ratings and bias of (movies)----------------

# on edx dataset:
predicted_edx_option2 <- doPredictionWithModel(edx, model_NoRegul, option=2)
err_edx_option2 <- calcRMSE(edx$rating, predicted_edx_option2)

#on final_holdout_test:
predicted_test_option2 <- doPredictionWithModel(final_holdout_test, 
                                                model_NoRegul, option=2)

err_test_option2 <- calcRMSE(final_holdout_test$rating, predicted_test_option2)
#-------------------------------------------------------------------------------

# Option #3: considering average of ratings and bias of (movies, users)---------

# on edx dataset:
predicted_edx_option3 <- doPredictionWithModel(edx, model_NoRegul, option=3)
err_edx_option3 <- calcRMSE(edx$rating, predicted_edx_option3)

#on final_holdout_test:
predicted_test_option3 <- doPredictionWithModel(final_holdout_test, 
                                                model_NoRegul, option=3)

err_test_option3 <- calcRMSE(final_holdout_test$rating, predicted_test_option3)
#-------------------------------------------------------------------------------

# Option #4: considering average of ratings and bias of (movies, users, genres)-

# on edx dataset:
predicted_edx_option4 <- doPredictionWithModel(edx, model_NoRegul, option=4)
err_edx_option4 <- calcRMSE(edx$rating, predicted_edx_option4)

#on final_holdout_test:
predicted_test_option4 <- doPredictionWithModel(final_holdout_test, 
                                                model_NoRegul, option=4)

err_test_option4 <- calcRMSE(final_holdout_test$rating, predicted_test_option4)
#-------------------------------------------------------------------------------

# Option #5 (Final Model): considering option #4 with regularization------------

# The lambda is investigated over the range of (0, 10):
lambdaRange <- c(0, 10)

# Find the optimized lambda that minimizes the 'regularizationErr' function:
bestRegularization <- optimize(regularizationErr, lambdaRange)

# Extract the optimized lambda:
lambda_opt <- unlist(bestRegularization["minimum"])
print(lambda_opt)   # lambda_opt = 4.10247 

# Build the predicting model by edx dataset and the optimized lambda:
finalModel <- buildLinearModel(edx, lambda = lambda_opt)

# on edx dataset:
predicted_edx_option5 <- doPredictionWithModel(edx, finalModel, option=5)
err_edx_option5 <- calcRMSE(edx$rating, predicted_edx_option5)

#on final_holdout_test:
predicted_test_option5 <- doPredictionWithModel(final_holdout_test, 
                                                finalModel, option=5)

err_test_option5 <- calcRMSE(final_holdout_test$rating, predicted_test_option5)



# Calculate quantiles for 'edx' and 'final holdout test' datasets:
portions <- seq(0,1,0.005)

# edx dataset
quantile_edx_actual <- quantile(edx$rating,portions)
quantile_edx_option1 <- quantile(predicted_edx_option1,portions)
quantile_edx_option2 <- quantile(predicted_edx_option2,portions)
quantile_edx_option3 <- quantile(predicted_edx_option3,portions)
quantile_edx_option4 <- quantile(predicted_edx_option4,portions)
quantile_edx_option5 <- quantile(predicted_edx_option5,portions)

# final holdout test
quantile_test_actual <- quantile(final_holdout_test$rating,portions)
quantile_test_option1 <- quantile(predicted_test_option1,portions)
quantile_test_option2 <- quantile(predicted_test_option2,portions)
quantile_test_option3 <- quantile(predicted_test_option3,portions)
quantile_test_option4 <- quantile(predicted_test_option4,portions)
quantile_test_option5 <- quantile(predicted_test_option5,portions)


graphTitle_edx <- "Predicted Ratings vs. Actual Rating for 'edx' Dataset"

plot(portions, quantile_edx_actual, type="o", col="black", 
     pch="o", lty=1, ylim=c(0,5), xlab="Portion", ylab="Rating" )
lines(portions, quantile_edx_option1, col="black",lty=2, lwd=2)
lines(portions, quantile_edx_option2, col="brown",lty=2, lwd=2)
lines(portions, quantile_edx_option3, col="green",lty=2, lwd=10)
lines(portions, quantile_edx_option4, col="red",lty=2, lwd=5)
lines(portions, quantile_edx_option5, col="blue",lty=2, lwd=2)

legend(0.5,3,legend=c("Actual","Option #1","Option #2", "Option #3", 
                      "Option #4", "Option #5"), 
       col=c("black", "black", "brown", "green","red", "blue"),
       pch=c("o","","","","",""),
       lty=c(1,2,3,4,5,6), ncol=1, lwd=c(1,2,2,10,5,2))

title(graphTitle_edx)



graphTitle_test <- "Predicted Ratings vs. Actual Rating for 'final hold out test'"

plot(portions, quantile_test_actual, type="o", col="black", 
     pch="o", lty=1, ylim=c(0,5), xlab="Portion", ylab="Rating" )
lines(portions, quantile_test_option1, col="black",lty=2, lwd=2)
lines(portions, quantile_test_option2, col="brown",lty=2, lwd=2)
lines(portions, quantile_test_option3, col="green",lty=2, lwd=10)
lines(portions, quantile_test_option4, col="red",lty=2, lwd=5)
lines(portions, quantile_test_option5, col="blue",lty=2, lwd=2)

legend(0.5,3,legend=c("Actual","Option #1","Option #2", "Option #3", 
                      "Option #4", "Option #5"), 
       col=c("black", "black", "brown", "green","red", "blue"),
       pch=c("o","","","","",""),
       lty=c(1,2,3,4,5,6), ncol=1, lwd=c(1,2,2,10,5,2))

title(graphTitle_test)




