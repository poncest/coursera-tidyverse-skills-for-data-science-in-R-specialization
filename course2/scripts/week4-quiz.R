
# Question 1 ---
# For this question you will need to read the `excel_data.xlsx` file into R using the readxl package. There are two sheets in this spreadsheet file named "Sheet1" and "Sheet2". Use the readxl package to read in "Sheet2" from the spreadsheet file directly and answer the question below. Recall that the [[ operator can be used to subset a column of tibble or data frame.

# what is the mean of the column labeled "X12" in "Sheet2"? You can use the mean() function to compute the mean of a column. (Choose the closest answer.)

library(readxl)
data <- read_excel("./course2/data/project_data/excel_data.xlsx", sheet = 2)

data %>% 
  summarize(mean(X12))

# Question 2. ----
# Continuing from Question 1 above, use the readxl package to read in both "Sheet1" and "Sheet2" from the excel_data.xlsx file. 

# What is the correlation between column "X5" in "Sheet1" and column "X8" in "Sheet2"? Use the cor() function to compute the correlation between two columns. (Choose the closest answer.)

data1 <- read_excel("./course2/data/project_data/excel_data.xlsx", sheet = 1)

data2 <- read_excel("./course2/data/project_data/excel_data.xlsx", sheet = 2)

cor(data1$X5, data2$X8)

# Question 3. ----

# For this question you will need to read in the database file sqlite_data.db using the RSQLite package. In this database file there is table named "table1". You will need to read that table to answer this question.

# The "ID" column in "table1" serves as and identification number for elements in the database table. What is the correlation between columns "S2" and "S3" for rows with ID equal to 8 only? (Hint: There should be 100 rows where ID = 8.)

library(RSQLite)

## Specify driver
sqlite <- dbDriver("SQLite")

## Connect to Database
db <- dbConnect(sqlite, "./course2/data/project_data/sqlite_data.db")
## List tables in database
dbListTables(db)

## install and load packages
# install.packages("dbplyr")
library(dbplyr)
library(dplyr)

## get the table
data <- 
  tbl(db, "table1") %>% 
  as_tibble(data) %>% 
  filter(ID == 8)

cor(data$S2, data$S3)

# Question 4. ----

# For this question you need to read in "Sheet2" from the excel_data.xlsx file using the readxl package and the data from the table2.json file using the jsonlite package. Then you need to inner join the two tables by their corresponding ID columns to create a new data frame.

# What is the mean of column "J2" in the joined data frame?

library(readxl)
data1 <- read_excel("./course2/data/project_data/excel_data.xlsx", sheet = 2)

# install.packages("jsonlite")
library(jsonlite)

## take JSON object and covert to a data frame
data2 <- fromJSON("./course2/data/project_data/table2.json")

## do inner join
data3 <- inner_join(data1, data2, by = "ID")

mean(data3$J2)

# Question 5. ----

# Continuing from Question 4 above, what is the correlation between column "X2" and column "J4" in the joined data frame?

cor(data3$X2, data3$J4)

