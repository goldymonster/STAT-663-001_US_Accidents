library(dplyr)
library(tidyr)
library(readxl)

#Filter Data Set to Just Virginia 
#Then Export the Virginia Dataset and Close Original Dataset to use New one. 
df<- read.csv("../Data/US_Accidents_March23.csv")

dfva<-filter(df, State == "VA")
write.csv(dfva, file = "../Data/Virginia_Accidents.csv", row.names = FALSE)
dfva<-read.csv("../Data/Virginia_Accidents.csv")
View(dfva)
str(dfva)
head(dfva)
dim(dfva)

Ctydf <- dfva |> 
  group_by(County) |>
  summarize(
    Count = n(),
    Average_Temperature = mean(Temperature.F.),
    Average_Humidity = mean(Humidity...),
    Average_Pressure = mean(Pressure.in.)
  ) 

knitr::kable(
  Ctydf |>
    select(County, Count, Average_Temperature),
  digits = 3
)