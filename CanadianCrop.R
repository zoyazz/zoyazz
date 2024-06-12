#install.packages(c('RSQLite'), repos = 'https://cran.rstudio.com',dependecies=TRUE)
library("RSQLite")
library(DBI)

conn <- dbConnect(RSQLite::SQLite(),"FinalDB.sqlite") #CONNECTION OBJECT 

############# creating Tables ###############################

#crop table
dbExecute(conn, "DROP TABLE IF EXISTS CROP_DATA") #deleting if already created

df1 <- dbExecute(conn, 
                 "CREATE TABLE CROP_DATA (
                                      CD_ID INTEGER NOT NULL,
                                      YEAR DATE NOT NULL,
                                      CROP_TYPE VARCHAR(20) NOT NULL,
                                      GEO VARCHAR(20) NOT NULL, 
                                      SEEDED_AREA INTEGER NOT NULL,
                                      HARVESTED_AREA INTEGER NOT NULL,
                                      PRODUCTION INTEGER NOT NULL,
                                      AVG_YIELD INTEGER NOT NULL,
                                      PRIMARY KEY (CD_ID)
                                      )", 
                 errors=FALSE
)

#Farm Prices table
dbExecute(conn, "DROP TABLE IF EXISTS FARM_PRICES")

df2 <- dbExecute(conn, 
                 "CREATE TABLE FARM_PRICES (
                                      CD_ID INTEGER NOT NULL,
                                      YEAR DATE NOT NULL,
                                      CROP_TYPE VARCHAR(20) NOT NULL,
                                      GEO VARCHAR(20) NOT NULL,
                                    PRICE_PRERMT FLOAT(6) NOT NULL,
PRIMARY KEY (CD_ID)
)",
                 errors = FALSE)
if (df2 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
} 

#Daily FX table
dbExecute(conn, "DROP TABLE IF EXISTS DAILY_FX")

df3 <- dbExecute(conn, 
                 "CREATE TABLE DAILY_FX (
DFX_ID INTEGER NOT NULL,
YEAR DATE NOT NULL,
FXUSDCAD FLOAT(6) NOT NULL,
PRIMARY KEY (DFX_ID)
)",
                 errors = FALSE)
if (df3 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
} 

dbExecute(conn, "DROP TABLE IF EXISTS MONTHLY_FX")

df4 <- dbExecute(conn, 
                 "CREATE TABLE MONTHLY_FX (
DFX_ID INTEGER NOT NULL,
YEAR DATE NOT NULL,
FXUSDCAD FLOAT(8) NOT NULL,
PRIMARY KEY (DFX_ID)
)",
                 errors = FALSE)
if (df4 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
} 

dbListTables(conn)

#Loading data
crop_df <- read.csv("C:/Users/zoyaz/Dropbox/My PC (LAPTOP-1Q5GKF54)/Downloads/data_analysis/Annual_Crop_Data.csv")
farm_df <- read.csv("C:/Users/zoyaz/Dropbox/My PC (LAPTOP-1Q5GKF54)/Downloads/data_analysis/Monthly_Farm_Prices.csv")
daily_df <- read.csv("C:/Users/zoyaz/Dropbox/My PC (LAPTOP-1Q5GKF54)/Downloads/data_analysis/Daily_FX.csv")
monthly_df <- read.csv("C:/Users/zoyaz/Dropbox/My PC (LAPTOP-1Q5GKF54)/Downloads/data_analysis/Monthly_FX.csv")

head(crop_df)
head(farm_df)
head(daily_df)
head(monthly_df)

#Reading data into table
dbWriteTable(conn, "CROP_DATA", crop_df, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "FARM_PRICES", farm_df, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "DAILY_FX", daily_df, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "MONTHLY_FX", monthly_df, overwrite=TRUE, header = TRUE)

dbGetQuery(conn, 'SELECT COUNT(CD_ID) FROM FARM_PRICES') #2678 RECORDS

#provinces we have information from
dbGetQuery(conn, 'SELECT DISTINCT(GEO) FROM FARM_PRICES') #alberta and sakathchewan

#distinct crops we have data on
dbGetQuery(conn, 'SELECT DISTINCT(CROP_TYPE) AS CROP 
           FROM CROP_DATA')

#total Rye in 1968
dbGetQuery(conn, "SELECT SUM(HARVESTED_AREA) AS TOTAL_RYE_1968 FROM CROP_DATA
WHERE CROP_TYPE = 'Rye' AND strftime('%Y',YEAR) = '1968'")

#yearly crop data from 1985 to 2020
dbGetQuery(conn, 'SELECT min(DATE) FIRST_DATE, max(DATE) LAST_DATE FROM FARM_PRICES')

# average price of crops that
dbGetQuery(conn, "SELECT DISTINCT(CROP_TYPE), Avg(PRICE_PRERMT) as AVG_PRICE
FROM FARM_PRICES
WHERE PRICE_PRERMT >= 350")

# the highest yielding year for each type of crop
dbGetQuery(conn, "SELECT CROP_TYPE, MAX(AVG_YIELD) AS MAX_YIELD, YEAR
FROM CROP_DATA 
WHERE GEO='Saskatchewan' 
GROUP BY CROP_TYPE 
ORDER BY MAX_YIELD desc")

# average yield by crop type and city
query <- "
  SELECT CROP_TYPE, GEO, AVG(AVG_YIELD) AS AVG_YIELD
  FROM CROP_DATA
  GROUP BY CROP_TYPE, GEO
  ORDER BY CROP_TYPE, GEO
"

dbGetQuery(conn, query)

dbGetQuery(conn, "SELECT SUM(HARVESTED_AREA) AS TOTAL_HARVESTED_WHEAT, strftime('%Y',YEAR) AS YEAR
FROM CROP_DATA
WHERE YEAR = (SELECT MAX(YEAR) FROM CROP_DATA)
AND GEO = 'Canada' AND CROP_TYPE = 'Wheat'")

# Canola prices in Saskawtchewan for the past 12 months in USD and CAD using inner join
dbGetQuery(conn, "SELECT F.DATE, F.GEO, F.CROP_TYPE, M.FXUSDCAD AS USDtoCAD, F.PRICE_PRERMT AS CAD_PRICE,
(F.PRICE_PRERMT/M.FXUSDCAD) AS USD_PRICE
FROM FARM_PRICES F, MONTHLY_FX M
WHERE strftime('%Y',F.DATE) = strftime('%Y',M.DATE)
AND strftime('%m',F.DATE) = strftime('%m',M.DATE)
AND CROP_TYPE ='Canola' 
AND GEO = 'Saskatchewan' 
ORDER BY F.DATE desc LIMIT 12")

close(conn)
