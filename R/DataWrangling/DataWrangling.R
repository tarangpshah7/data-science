library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(anytime)
library(ggplot2)
########################################----PART-1----###################################################

temp_col_names<-c("Num_of_bed","Reg","Vic","PerS","Aus","PerV")

df2011<-read_xlsx("censusdata.xlsx",sheet = '2011', col_names = temp_col_names)
df2016<-read_xlsx("censusdata.xlsx",sheet = '2016', col_names = temp_col_names)

# As there are multiple values in first column which are separated by ‘/’.
# We can not consider it as tidy. A tidy data always contains a single value in a single cell. 

# Drop NA values
df2011 <- drop_na(df2011)
df2016 <- drop_na(df2016)
df2011 <- df2011[-c(109:117),] #Removing duplicate rows


#Creating Separate Datafrmaes for Victoria & Australia
Vic2011 <- data.frame(df2011$Num_of_bed, df2011$Vic)
Vic2016 <- data.frame(df2016$Num_of_bed, df2016$Vic)
Aus2011 <- data.frame(df2011$Num_of_bed, df2011$Aus)
Aus2016 <- data.frame(df2016$Num_of_bed, df2016$Aus)

#Function to transform Regions DataFrame into DesiredFormate
transform_function<- function(df,year)
{
  df <- mutate(df, year = year)
  df <- separate(df,"Reg",into = c('Region','Percentage'), sep = "/",remove = TRUE,convert = TRUE)
  df <- df[,-c(3:7)]
  df <- mutate(df,rownum= rep(1:(nrow(df)/9),each=9))
  df <- spread(df, "Num_of_bed", "Region") 
  col_order<- c("Number of bedrooms","year","None (includes bedsitters)","1 bedroom","2 bedrooms","3 bedrooms",
                "4 or more bedrooms","Number of bedrooms not stated","Average number of bedrooms per dwelling",
                "Average number of people per household")
  df<-df[,col_order]
  cnames_final <- c("region", "year", "br_count_0", "br_count_1", "br_count_2", "br_count_3", 
                    "br_count_4_or_more", "br_count_unstated", "av_per_dwelling", "av_per_household")
  names(df) <- cnames_final 
  
  df <- separate(df, region, c("region", "B"), sep = "([\\(])", covert = TRUE) 
  df <- df[,-2]
  trimws(df$region)
  return(df)
}

#Function to transform Victoria and Australia DataFrame into DesiredFormate
transform_function_overall <- function(df,year)
{
  df <- mutate(df, rownum = rep(1:(nrow(df)/9), each=9))
  df <- df[1:9,]
  df <- mutate(df, year = year)
  df <- df[,-3]
  names(df) <- c("NoB","Vic","Year")
  df <- spread(df, NoB, Vic)
  col_order<- c("Number of bedrooms","Year","None (includes bedsitters)","1 bedroom","2 bedrooms","3 bedrooms",
                "4 or more bedrooms","Number of bedrooms not stated","Average number of bedrooms per dwelling",
                "Average number of people per household")
  df <- df[,col_order]
  cnames_final <- c("region", "year", "br_count_0", "br_count_1", "br_count_2", "br_count_3", 
                    "br_count_4_or_more", "br_count_unstated", "av_per_dwelling", "av_per_household")
  names(df) <- cnames_final 
  return(df)
}

#Transforming the dataframes into desired fomrats
df2011 <- transform_function(df2011,2011)
df2016 <- transform_function(df2016,2016)

Vic2011 <- transform_function_overall(Vic2011,2011)
Vic2016 <- transform_function_overall(Vic2016,2016)
Aus2011 <- transform_function_overall(Aus2011,2011)
Aus2016 <- transform_function_overall(Aus2016,2016)

#Changing Unusual values
df2011[17,6] = 10986
df2016[17,3] = 61

#Adding Missing values
df2011[15,10] = 3.3
df2016[25,7] = 6662

#Combining Region Dataframe
final_df <- rbind(df2011,df2016)

#Arranginf Data Frame
final_df <- final_df[order(final_df$region, final_df$year),]

#Exchanging rows which are not in order
a <- final_df
final_df[1,]  = a[2,]
final_df[2,]  = a[1,]
final_df[15,]  = a[16,]
final_df[16,]  = a[15,]

#Adding raws of Victoria  and Australia Dataframes
final_df <- rbind(final_df,Vic2011,Vic2016,Aus2011,Aus2016)

View(final_df)

write.csv(final_df,"tidy_censusdata.csv", row.names = FALSE)