library(dplyr)
library(tidyr)
#Filter Data Set to Just Virginia 
#Then Export the Virginia Dataset and Close Original Dataset to use New one. 
df<- read.csv("Data/US_Accidents_March23.csv")

dfva<-filter(df, State == "VA")


write.table(dfva, file = "Data/Virginia_Accidents.csv", sep = ",")

dfva<-read.csv("Data/US_Accidents_March23.csv")
str(dfva)
head(dfva)
dim(dfva)
?group_by()
Ctydf <- dfva |> 
  group_by(County, Severity) |>
  summarize(
    Count = n(),
    Average_Temperature = mean(Temperature.F.),
    Average_Humidity = mean(Humidity...),
    Average_Pressure = mean(Pressure.in.)
  ) 

knitr::kable(
  Ctydf |>
    select(County, Severity, Count, Average_Temperature),
  digits = 3
)

