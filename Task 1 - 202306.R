# Prepare
# Installing and loading required packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("dplyr")
install.packages("ggplot2")

library(tidyverse)
library(lubridate)
library(janitor)
library(dplyr)
library(ggplot2)

# Importing data to data-frame in R
setwd("C:/Users/alfre/Documents/R/DATOS")
data_ciclystic <- read.csv("202306-divvy-tripdata.csv")

# I only have one dataset, so I don't need merming data, but if you need is like that
# data_ciclystic <- rbind(202306-divvy-tripdata.csv,202305-divvy-tripdata.csv,202304-divvy-tripdata.csv, 202303-divvy-tripdata.csv...)

# Process
# The head of data-frame

head(data_ciclystic)

# Removing the empty columns and rows using janitor

data_ciclystic <- janitor::remove_empty(dat = data_ciclystic,which = c("cols"))
data_ciclystic <- janitor::remove_empty(dat = data_ciclystic,which = c("rows"))
dim(data_ciclystic)

# Removing the empty columns and rows using na.omit()
data_ciclystic_clean <- na.omit(data_ciclystic)
dim(data_ciclystic_clean)
head(data_ciclystic_clean)

# Removing the empty columns and rows using filter_all()
data_ciclystic_clean <- data_ciclystic %>%
  filter_all(all_vars(!is.na(.) & . != ""))
head(data_ciclystic_clean)
dim(data_ciclystic_clean)
data_ciclystic <- data_ciclystic_clean

# Converting started_at and ended_at columns to date data type (POSIXct) using lubridate

data_ciclystic$started_at <- lubridate::as_datetime(data_ciclystic$started_at)
data_ciclystic$ended_at <- lubridate::as_datetime(data_ciclystic$ended_at)

# Getting the start time (hour) and end time (hour) components from date-time 

data_ciclystic$start_hr <- lubridate::hour(data_ciclystic$started_at)
data_ciclystic$end_hr <- lubridate::hour(data_ciclystic$ended_at)

# Getting the start date and end date

data_ciclystic$start_date <- lubridate::date(data_ciclystic$started_at)
data_ciclystic$end_date <- lubridate::date(data_ciclystic$ended_at)

# Get the duration of the trip and then change the column format to numeric (min)

data_ciclystic$length_min <- difftime(data_ciclystic$ended_at,data_ciclystic$started_at,units = c("mins"))
data_ciclystic$length_min <- as.numeric(data_ciclystic$length_min)
summary(data_ciclystic)

# Trips with duration less than zero (0)

data_ciclystic_clean <- data_ciclystic %>%
  filter(length_min > 0)
summary(data_ciclystic_clean)

data_ciclystic <- data_ciclystic_clean

# Assign the days of the week to start_date and end_date in other columns (start_day and end_day)

data_ciclystic$start_day <- wday(data_ciclystic$start_date, label = TRUE, abbr = FALSE)
data_ciclystic$end_day <- wday(data_ciclystic$end_date, label = TRUE, abbr = FALSE)
dim(data_ciclystic)
View(data_ciclystic)

# We export the end result as CSV to work on Tableau

write.csv(data_ciclystic, file = "C:/Users/alfre/Documents/R/DATOS/Nueva carpeta/data_ciclystic_202306.csv", row.names = FALSE)

# Others
# Avg and maximum

cat('average duration of rides is : ', mean(data_ciclystic$length_min), "mins")
cat('  maximum duration of rides is : ', round(max(data_ciclystic$length_min)/60/24), "days")

# Cake graph for fun

member <- "member"
member_cant <- sum(grepl(member, data_ciclystic_final$member_casual))

casual <- "casual"
casual_cant <- sum(grepl(casual, data_ciclystic_final$member_casual))


data_members <- data.frame(
  nombres <- c("member", "casual"),
  cantidades <- c(418248, 300408)
)
data_members$porcentaje <- round((data_members$cantidades / sum(data_members$cantidades)) * 100, 1)

grafico_pastel <- ggplot(data_members, aes(x = "", y = cantidades, fill = nombres)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  theme_void()+
  geom_text(aes(label = paste0(porcentaje, "%")), position = position_stack(vjust = 0.5))+
  labs(title="Distribución de tipos de miembros", caption="Total de datos: 5559900")

colores_personalizados <- c("#ff7514", "#84B6f4")
grafico_pastel <- grafico_pastel + scale_fill_manual(values = colores_personalizados)

print(grafico_pastel)