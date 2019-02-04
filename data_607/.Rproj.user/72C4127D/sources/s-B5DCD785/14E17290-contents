#https://rstudio-pubs-static.s3.amazonaws.com/125760_358e4a6802c94fa29e2a9ab49f45df94.html
#https://rstudio-pubs-static.s3.amazonaws.com/125760_358e4a6802c94fa29e2a9ab49f45df94.html
#https://vincentarelbundock.github.io/Rdatasets/datasets.html
#https://vincentarelbundock.github.io/Rdatasets/doc/carData/MplsStops.html


#https://www.kaggle.com/uciml/mushroom-classification#mushrooms.csv

#https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data

mashroom_url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
#mashroom_url <- "data/mushrooms.csv"
df_mashroom <- read.table(file = mashroom_url, header = TRUE, sep = ",")
head(df_mashroom)
summary(df_mashroom)
NROW(df_mashroom)
NROW(na.omit(df_mashroom))

#create a data frame with only the required columns
#two data frames are created since there is a column break
mashroom01 <- df_mashroom[,1:4]
mashroom02 <- df_mashroom[,6]
#the two are combined into one data frame
mashroom00 <- cbind(mashroom01, mashroom02)
head(mashroom00)


#carUrl <- "https://vincentarelbundock.github.io/Rdatasets/csv/carData/Mroz.csv"
#df_cars <- read.table(file = carUrl, header = TRUE, sep = ",")
#head(df_cars)
#summary(df_cars)
#NROW(df_cars)
#NROW(na.omit(df_cars))

#colnames(df_cars)[2] <- "labor-force-participation"
#colnames(df_cars)[3] <- "children-under-5"
#colnames(df_cars)[4] <- "children-6-to-18"
#colnames(df_cars)[6] <- "wife-college-attendance"
#colnames(df_cars)[7] <- "husband-college-attendance"
#colnames(df_cars)[8] <- "log-wage"
#colnames(df_cars)[9] <- "wifes-exclusive-income"

#is.na(df_cars)
#sum(is.na(df_cars$age))
#colSums(is.na(df_cars))
#any(is.na(df_cars$age))
