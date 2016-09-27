library(tidyverse)

#load data
raw_data <- read_csv(file="lab_quiz_week2_data.csv")

#View(raw_data)

raw_data <- read_csv(file="lab_quiz_week2_data.csv" ,na=c("","NA","-999"))

#tell R about categorical variables university and program year
categorical_variables <- select(raw_data, univ, prog_year)
categorical_variables$univ <- as.factor(categorical_variables$univ)
levels(categorical_variables$univ) <- list("Waterloo"=1,"Guelph"=2)
categorical_variables$prog_year <- as.factor(categorical_variables$prog_year)
levels(categorical_variables$prog_year) <- list("First Year"=1,"Second Year"=2,"Third Year"=3,"Fourth Year"=4,"Grad School"=5)

#create scale items
pos_affect_items <- select (raw_data, PA1, PA2, PA3, PA4, PA5)
dep_items <- select (raw_data, D1, D2, D3, D4, D5)
prog_sat_items <- select (raw_data, PS1, PS2, PS3, PS4, PS5)

age <- select(raw_data, age)

#View(pos_affect_items)
#View(dep_items)
#View(prog_sat_items)

#check for out of range values
psych::describe(pos_affect_items)
psych::describe(dep_items)
psych::describe(prog_sat_items)

pos_affect_items
dep_items
prog_sat_items

is_bad_value <- pos_affect_items<1 | pos_affect_items>7
#View(pos_affect_items)
pos_affect_items[is_bad_value] <- NA

is_bad_value <- dep_items<1 | dep_items>4
dep_items[is_bad_value] <- NA

is_bad_value <- prog_sat_items<1 | prog_sat_items>6
prog_sat_items[is_bad_value] <- NA
View(prog_sat_items)

#flip reverse key items
pos_affect_items <- mutate(pos_affect_items,PA1=8-PA1)

#View(pos_affect_items)

dep_items <- mutate(dep_items,D4=5-D4)
dep_items <- mutate(dep_items,D5=5-D5)

#View(dep_items)

prog_sat_items <- mutate(prog_sat_items,PS1=7-PS1)
prog_sat_items <- mutate(prog_sat_items,PS2=7-PS2)

#View(prog_sat_items)

#obtaining scaled scores
pos_affect <- psych::alpha(as.data.frame(pos_affect_items) ,check.keys=FALSE)$scores
dep <- psych::alpha(as.data.frame(dep_items) ,check.keys=FALSE)$scores
prog_sat <- psych::alpha(as.data.frame(prog_sat_items), check.keys=FALSE)$scores

#combine into analytic data
analytic_data <- cbind(categorical_variables,age,pos_affect,dep,prog_sat)

analytic_data

#save data
write_csv(analytic_data,path="quiz1_analytic_data_gomez.csv")
