#install.packages("odbc")
#library(odbc)

con <- dbConnect(odbc(),
                 Driver = "{ODBC Driver 13 for SQL Server}",
                 Server = "jcvd3m5th8.database.windows.net",
                 Database = "FDClientData",
                 UID = rstudioapi::askForPassword("Database uid"),
                 PWD = rstudioapi::askForPassword("Database password"),
                 Port = 1433)

#con <- dbConnect(odbc::odbc(), "mydbalias")

#dbListTables(con)
dbListTables(con, table_name = "MovieR%")

query<-paste0("SELECT ID, [Name] ,[The_Cleanse] ,[Downsizing] ,[Lady_Bird] ,[Get_Out] ,[The_Shape_of_Water] ,[The_Post] FROM [dbo].[MovieReviews]")

moviedata<-dbGetQuery(con,query)
moviedata

install.packages("getPass")
library(getPass)
#data <- dbReadTable(con, "MovieReviews")

#install.packages("RevoScaleR")
#ls("package:RevoScaleR")

#RevoScaleR
#sConnString <- "Driver={ODBC Driver 13 for SQL Server}; Server=tcp:jcvd3m5th8.database.windows.net,1433; Database=PDLPatientService; Uid=monuchacko; Pwd=ZBhIz6QhWhCWYKXQ6LRc; Encrypt=yes; TrustServerCertificate=no; Connection Timeout=30;"
#sQuery <- "select * from SalesLT.ProductDescription"
#sDataSet <- RxOdbcData(sqlQuery=sQuery, connectionString=sConnString)
#sDataFile <- RxXdfData("c:/users/temp/mysqldata.xdf")
#rxImport(sDataSet, sDataFile, overwrite=TRUE)
#rxGetInfo(sDataFile, getVarInfo=TRUE, numRows=50)