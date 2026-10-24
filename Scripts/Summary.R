library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
#Filter Data Set to Just Virginia 
#Then Export the Virginia Dataset and Close Original Dataset to use New one. 
df<- read.csv("../Data/US_Accidents_March23.csv")

dfva<-filter(df, State == "VA")
pop <- read_excel("../Data/VA-Intercensal-Estimates_2000-2010.xlsx", skip = 5, col_names = FALSE)
pop<-pop[ ,1:3]
colnames(pop)<- c("FIPS", "County", "Population")
pop <- pop %>% 
  mutate(County = str_remove(County, "County") %>% str_squish())
write.csv(dfva, file = "../Data/Virginia_Accidents.csv", row.names = FALSE)
dfva<-read.csv("../Data/Virginia_Accidents.csv")
dfva <- dfva %>% 
  mutate(County = str_remove(County, "County") %>% str_squish())
dfva<- left_join(dfva, pop, by = "County")
View(pop)
str(dfva)
head(dfva)
dim(dfva)

?group_by()
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

