rm(list=ls())


library(shiny)
library(RColorBrewer)
library(mapproj)
library(MASS)
library(data.table)
library(car)
library(boot)
library(ISLR)
library(maps)
library(class)
library(ggplot2)
library(corrplot)
library(glmnet)
library(pls)
library(leaps)
library(tree)
library(randomForest)
library(splines)
library(gam)
library(lmtest)
library(gbm)
library(corrplot)
library(ggplot2)
library(dplyr)

Gender_Composition <- read.csv(file = here::here("Data/Gender_Composition.csv"))

Gender_Composition <- read.csv("Data/Gender_Composition.csv")
head(Gender_Composition)
summary(Gender_Composition)
str(Gender_Composition)

colSums(is.na(Gender_Composition))

Gender_Composition[Gender_Composition$Gender == "Female", ]   %>%
  tail(10) %>%
  ggplot( aes(x=Year, y=share)) +
  geom_line() +
  geom_point()


Race_and_ethnicity <- read.csv("Data/Race_and_ethnicity.csv")
head(Race_and_ethnicity)
summary(Race_and_ethnicity)
str(Race_and_ethnicity)

colSums(is.na(Race_and_ethnicity))

Race_and_ethnicity_b <- Race_and_ethnicity[Race_and_ethnicity$Race == "Black",]
Race_and_ethnicity_c <- Race_and_ethnicity_b[Race_and_ethnicity_b$Slug.Geography == "united-states",]

Race_and_ethnicity_c[Race_and_ethnicity_c$ID.PUMS.Ethnicity.Parent == "Hispanic" ,]   %>%
  tail(10) %>%
  ggplot( aes(x=Year, y=share)) +
  geom_line() +
  geom_point()

offender_sex_and_race <- read.csv("Data/offender_sex_and_race.csv")



democratic_vs_republican_votes_by_usa_state_2020 <- read.csv("Data/democratic_vs_republican_votes_by_usa_state_2020.csv")
democratic_vs_republican_votes_usa_2016 <- read.csv("Data/democratic_vs_republican_votes_usa_2016.csv")

democratic_vs_republican_votes_usa_2016 <- democratic_vs_republican_votes_usa_2016 %>% 
  rename(
    state = ï..State,
  )

election <- merge(democratic_vs_republican_votes_by_usa_state_2020, democratic_vs_republican_votes_usa_2016, by = "state")

election <- election %>% 
  rename(
    percent_democrat_2016 = percD,
    percent_democrat_2020 = percent_democrat
  )

par(mar=c(1, 5, 1, 1))
barplot(election$percent_democrat_2016 ~ election$usa_state_code,
        horiz = TRUE)


all_states <- map_data("state")
election$region <- tolower(election$usa_state)
totaldf <- merge(all_states, election, by = "region")
# switched to data.table to fix the cut up map issue
# getting sort by region then order 
totaldt <- as.data.table(totaldf)
setkey(totaldt, region, order)
ggplot(data = totaldt, 
       aes(x = long, y = lat, group = group, fill = percent_democrat_2016)) +
  geom_polygon() + coord_map() +
  scale_fill_gradientn("", colours=brewer.pal(9, "GnBu"))

ggplot(data = totaldt, 
       aes(x = long, y = lat, group = group, fill = percent_democrat_2020)) +
  geom_polygon() + coord_map() +
  scale_fill_gradientn("", colours=brewer.pal(9, "GnBu"))
