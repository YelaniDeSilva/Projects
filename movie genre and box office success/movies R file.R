#importing the csv file
movies <- read.csv("movies.csv",header = TRUE ,sep = ",")

#importing the ggplot2 library 
library(ggplot2)

#uni variate analysis
#drawing a bar chart
ggplot(movies) + aes(Imdb_genre) + geom_bar() + labs(x="IMDB_genre",y="count",title="movies by genre") + theme_minimal()

#creating a sample from the data set to increase the clarity of the visualizations 
set.seed(2)
sampleMovies<-movies[sample(nrow(movies),40),]

#creating a histogram
ggplot(sampleMovies) + geom_histogram(aes(IMDB.Rating),binwidth = 0.4,fill = "#69b3a2") + labs(x="IMDB.Rating",y="count",title="Ratings of movies") + theme_minimal()

#creating a box plot
ggplot(sampleMovies) + geom_boxplot(aes(x = Box.Office.Collection)) + labs(title="box office collection") + theme_minimal()

#creating a frequency polygon
ggplot(sampleMovies) + geom_freqpoly(aes(IMDB.Rating),binwidth = 0.4) + labs(x="IMDB.Rating",y="count",title="distribution of the imdb ratings") + theme_minimal()

#creating a density plot
ggplot(sampleMovies) + ( aes(x=Box.Office.Collection)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + labs(x="box office collection",y="count",title="distribution of the box office collection") + theme_minimal()

#creating a density plot
ggplot(sampleMovies) + ( aes(x=IMDB.Rating)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) + labs(x="imdb rating",y="count",title="distribution of the imdb ratings") + theme_minimal()

#bivariate analysis
#creating a scatter plot for the variables IMDb rating and box office collection
ggplot(sampleMovies) + aes(IMDB.Rating, Box.Office.Collection) + geom_point(alpha = 0.5) + labs(x="imdb rating",y="box office collection",title="imdb ratings in relation to box office collections") + theme_minimal()

#creating a scatter plot
ggplot(movies) + aes(IMDB.Rating, Box.Office.Collection) + geom_point(alpha = 0.5) + labs(x="imdb rating",y="box office collection",title="imdb ratings in relation to box office collections") + theme_minimal()

#creating a scatter plot using the geom_jitter function
ggplot(movies) + aes(IMDB.Rating, Box.Office.Collection) + geom_jitter() +
  geom_smooth(method = "lm") +  geom_point(alpha = 0.5) + labs(x="imdb rating",y="box office collection",title="imdb ratings in relation to box office collections") + theme_minimal()

#importing the library ggpubr to calculate the correlation between variables
library(ggpubr)

#using stat_cor function to find correlation and using a scatter plot
ggplot(movies) + aes(Year, Votes) + geom_point(alpha = 0.5) + stat_cor(method = "pearson") + labs(x="year",y="votes",title="year in relation to votes") + theme_minimal() 

#creating a scatter plot
ggplot(movies) + geom_point(alpha = 0.5) + aes(IMDB.Rating, Votes) + stat_cor(method = "pearson") + theme_minimal()

#creating a scatter plot
ggplot(movies) + aes(IMDB.Rating, Box.Office.Collection) + geom_point(alpha = 0.5) + stat_cor(method = "pearson") + theme_minimal()

#creating a scatter plot
ggplot(movies) + aes(Year, IMDB.Rating) + geom_point(alpha = 0.5) + stat_cor(method = "pearson") + theme_minimal()

#multivariate analysis
#creating a scatter plot
ggplot(movies) + aes(IMDB.Rating,Votes, colour = metascore, shape = Imdb_genre) + geom_jitter() + theme_minimal()

#creating a scatter plot
ggplot(movies) + aes(Box.Office.Collection,IMDB.Rating ,colour = Imdb_genre) + geom_point(alpha = 0.5) + theme_minimal()

#creating a line chart
ggplot(movies) + aes(Box.Office.Collection, IMDB.Rating, color = Imdb_genre) + geom_line() + theme_minimal()

#creating a box plot
ggplot(movies) + aes(Imdb_genre,Box.Office.Collection) + geom_boxplot() + theme_minimal()

#creating a scatter plot
ggplot(movies) + aes(Box.Office.Collection,IMDB.Rating ,colour = Imdb_genre) + geom_point(alpha = 0.5) + facet_wrap(~ Imdb_genre, nrow = 2)

#Loading the library dplyr to find the box office collection per genre
library(dplyr)

#creating the sample of 5movies for each genre
set.seed(2) 
#Creating the object average box office per genre from the movies dataset and finds the average for all the randomly selected movies.
average_box_office_per_genre <- movies %>%
  group_by(Imdb_genre) %>%
  sample_n(5, replace = FALSE) %>% # Randomly sample 5 movies per genre
  summarise(Average_Collection = mean(Box.Office.Collection, na.rm = TRUE))

print(average_box_office_per_genre)

# Create the bar chart
ggplot(average_box_office_per_genre) + 
  geom_bar(aes(x =Imdb_genre , y = Average_Collection), stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Average Box Office Collection by Genre",
       x = "Genre",
       y = "Average Box Office Collection") + theme_minimal()
  



  
