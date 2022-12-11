library(dplyr)
library(tidyr)
library(sqldf)
library(lubridate)
library(readxl)
library(sqldf)
library(weights)
library(wesanderson)
library(zoo)
library(tsibble)
library(usmap)
library(feasts)

this.dir <- dirname(rstudioapi::getSourceEditorContext()$path)
this.dir
setwd(this.dir)

shooting_data <- read.csv("./data-police-shootings-master/fatal-police-shootings-data.csv")
census_data_orig <- read_excel("./pop-decennial.xls")
race_data <- read.csv("../App-1/race_data.csv")

# Name
# Date yyyy-mm-dd
# Manner of death

# Cleaning data
shooting_data_clean <- 
  shooting_data %>%
  drop_na(age)

summary(shooting_data_clean)
  
max(shooting_data_clean$age)
min(shooting_data_clean$age)

shooting_data_clean %>%
  count(race)

months <- 1:12
names(months) = month.name
months["March"]

shooting_data_clean <- 
  shooting_data_clean %>%
  mutate(race_clean = case_when(race == "W" ~ "White",
                               race == "B" ~ "Black",
                               race == "A" ~ "Asian",
                               race == "H" ~ "Hispanic",
                               race == "O" ~ "Other",
                               race == "N" ~ "Other",
                               is.na(race) ~ "Unknown",
                               TRUE ~ "Unknown"),
         year = as.numeric(format(as.Date(date),"%Y")),
         month = month.name[month(date)],
         date = as.Date(date),
         victim_armed = case_when(armed == "undetermined" ~ "Unknown",
                                             armed == "Unknown" ~ "Yes, object unknown",
                                             armed == "unarmed" ~ "No",
                                              TRUE ~ "Yes"),
         gender_clean = case_when(gender == "M" ~ "Male",
                                  gender == "F" ~ "Female",
                                  TRUE ~ "Unknown"),
         armed_clean = case_when(armed == "air conditioner" ~ "Household object",
         armed == "air pistol" ~ "Airsoft/BB Gun",
         armed == "Airsoft pistol" ~ "Airsoft/BB Gun",
         armed == "ax" ~ "Ax or Knife",
         armed == "ax and machete" ~ "Ax or Knife",
         armed == "barstool" ~ "Household object",
         armed == "baseball bat" ~ "Baseball bat",
         armed == "baseball bat and bottle" ~ "Multiple weapons",
         armed == "baseball bat and fireplace poker" ~ "Baseball bat",
         armed == "baseball bat and knife" ~ "Baseball bat",
         armed == "baton" ~ "Other",
         armed == "BB gun" ~ "Airsoft/BB Gun",
         armed == "BB gun and vehicle" ~ "Multiple weapons",
         armed == "bean-bag gun" ~ "Gun",
         armed == "beer bottle" ~ "Household object",
         armed == "binoculars" ~ "Household object",
         armed == "blunt object" ~ "Other",
         armed == "bottle" ~ "Household object",
         armed == "bow and arrow" ~ "Other",
         armed == "box cutter" ~ "Ax or Knife",
         armed == "brick" ~ "Other",
         armed == "car, knife and mace" ~ "Multiple weapons",
         armed == "carjack" ~ "Household object",
         armed == "chain" ~ "Chainsaw",
         armed == "chain saw" ~ "Chainsaw",
         armed == "chainsaw" ~ "Chainsaw",
         armed == "chair" ~ "Household object",
         armed == "claimed to be armed" ~ "Claimed to be armed",
         armed == "contractor's level" ~ "Household object",
         armed == "cordless drill" ~ "Household object",
         armed == "crossbow" ~ "Other",
         armed == "crowbar" ~ "Household object",
          armed == "fireworks" ~ "Other",
          armed == "flagpole" ~ "Household object",
          armed == "flare gun" ~ "Gun",
          armed == "flashlight" ~ "Household object",
          armed == "garden tool" ~ "Household object",
          armed == "glass shard" ~ "Household object",
          armed == "gun" ~ "Gun",
          armed == "gun and car" ~ "Multiple weapons",
          armed == "gun and knife" ~ "Multiple weapons",
          armed == "gun and machete" ~ "Multiple weapons",
          armed == "gun and sword" ~ "Multiple weapons",
          armed == "gun and vehicle" ~ "Multiple weapons",
          armed == "guns and explosives" ~ "Multiple weapons",
          armed == "hammer" ~ "Household object",
          armed == "hammer and garden tool" ~ "Household object",
          armed == "hand torch" ~ "Household object",
          armed == "hatchet" ~ "Ax or Knife",
          armed == "hatchet and gun" ~ "Multiple weapons",
          armed == "ice pick" ~ "Ax or Knife",
          armed == "incendiary device" ~ "Incendiary weapon",
          armed == "incendiary weapon" ~ "Incendiary weapon",
          armed == "knife" ~ "Household object",
          armed == "knife and vehicle" ~ "Multiple weapons",
          armed == "knife, hammer and gasoline can" ~ "Multiple weapons",
          armed == "lawn mower blade" ~ "Ax or Knife",
          armed == "machete" ~ "Ax or Knife",
          armed == "machete and gun" ~ "Multiple weapons",
          armed == "machete and hammer" ~ "Multiple weapons",
          armed == "meat cleaver" ~ "Ax or Knife",
          armed == "metal hand tool" ~ "Household object",
          armed == "metal object" ~ "Other",
          armed == "metal pipe" ~ "Household object",
          armed == "metal pole" ~ "Household object",
          armed == "metal rake" ~ "Household object",
          armed == "metal stick" ~ "Household object",
          armed == "microphone" ~ "Household object",
          armed == "motorcycle" ~ "Vehicle",
          armed == "nail gun" ~ "Gun",
          armed == "oar" ~ "Household object",
          armed == "pair of scissors" ~ "Household object",
          armed == "pellet gun" ~ "Airsoft/BB Gun",
          armed == "pen" ~ "Household object",
          armed == "pepper spray" ~ "Pepper spray",
          armed == "pick-axe" ~ "Ax or Knife",
          armed == "piece of wood" ~ "Household object",
          armed == "pipe" ~ "Household object",
          armed == "pitchfork" ~ "Household object",
          armed == "pole" ~ "Household object",
          armed == "pole and knife" ~ "Multiple weapons",
          armed == "railroad spikes" ~ "Ax or Knife",
          armed == "rock" ~ "Other",
          armed == "samurai sword" ~ "Ax or Knife",
          armed == "screwdriver" ~ "Household object",
          armed == "sharp object" ~ "Other",
          armed == "shovel" ~ "Household object",
          armed == "spear" ~ "Ax or Knife",
          armed == "stake" ~ "Ax or Knife",
          armed == "stapler" ~ "Household object",
          armed == "stone" ~ "Other",
          armed == "straight edge razor" ~ "Ax or Knife",
          armed == "sword" ~ "Ax or Knife",
          armed == "Taser" ~ "Taser",
          armed == "tire iron" ~ "Household object",
          armed == "toy weapon" ~ "Toy weapon",
          armed == "unarmed" ~ "Unarmed",
          armed == "undetermined" ~ "Undetermined",
          armed == "unknown weapon" ~ "Unknown weapon",
          armed == "vehicle" ~ "Vehicle",
          armed == "vehicle and gun" ~ "Multiple weapons",
          armed == "vehicle and machete" ~ "Multiple weapons",
          armed == "walking stick" ~ "Household object",
          armed == "wasp spray" ~ "Household object",
          armed == "wrench" ~ "Household object",
          is.na(armed) ~ "Unknown if victim was armed",
          TRUE ~ "Unknown if victim was armed"),
         
         flee_clean = case_when(flee == "Foot" ~ "Victim fled on foot",
                                flee == "Car" ~ "Victim fled in vehicle",
                                flee == "Not fleeing" ~ "Victim was not fleeing",
                                flee == "Other" ~ "Other",
                                TRUE ~ "Unknown"),
         male = case_when(gender == "M" ~ 1,
                          TRUE ~ 0),
        exist = 1
  )

summary(shooting_data_clean)

date <- as.Date(shooting_data$date)
summary(date)
class(date)
shooting_data_final <- shooting_data_clean %>%
  filter(id != 8103 & gender == "M" | gender == "F")

# Creating state dataframe
state_data <- shooting_data_clean %>%
  group_by(year, state) %>%
  summarise(count = n()) %>%
  mutate(straight_percentage = round(count / sum(count), 4))

# Cleaning census data
census_data <- census_data_orig %>%
  filter(Areaname != "United States") %>%
  mutate(state_abb = case_when(Areaname == "District of Columbia" ~ "DC",
    TRUE ~ state.abb[match(Areaname, state.name)])) %>%
  mutate(percent_pop = round(Population/sum(Population), 4))

# Joining state data to census data
state_data_joined <- sqldf(
  'select t1.*,
  t2.Population as population,
  t2.percent_pop as percent_pop
  from state_data t1 left join census_data t2 on t1.state = t2.state_abb'
)

# Cleaning the joined dataset
state_data_final <- state_data_joined %>%
  # filter(!state %in% c("DC","AK")) %>%
  group_by(year, state) %>%
  mutate(weight_percent = round(weighted.mean(straight_percentage,percent_pop),4),
         per_capita = count/population) %>%
  ungroup()


write.csv(state_data_final, "/Users/francescarael/Documents/R-Shiny/App-1/state_data.csv")

sh_shooting_data <- shooting_data_final
sh_state_data <- state_data_final

us_map <- us_map() # used to add map scale

# Testing out bar plot
ggplot(data = sh_shooting_data) + 
  geom_bar(mapping = aes(x = age, y = ..prop.., group = gender, fill = gender)) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  scale_fill_manual(values = c("#856084","#84E296")) +
  labs(title = "Murders by Gender and Age of Victim", x = "Age of Victim",y = "Percentage of Total Murders", fill = "Gender") +
  coord_flip()

# Testing out some summary statistics
median_male <- median(sh_shooting_data$age[sh_shooting_data$male==1])
median_female <-median(sh_shooting_data$age[sh_shooting_data$male==0])

mean_female <- mean(sh_shooting_data$age[sh_shooting_data$male==0])
mean_male <- mean(sh_shooting_data$age[sh_shooting_data$male==1])

quantile10female <- quantile(sh_shooting_data$age[sh_shooting_data$male==0], .1)
quantile15female <- quantile(sh_shooting_data$age[sh_shooting_data$male==0], .25)
quantile25female <- quantile(sh_shooting_data$age[sh_shooting_data$male==0], .5)
quantile10male <- quantile(sh_shooting_data$age[sh_shooting_data$male==1], .1)
quantile15male <- quantile(sh_shooting_data$age[sh_shooting_data$male==1], .25)
quantile25male <- quantile(sh_shooting_data$age[sh_shooting_data$male==1], .5)

maxmale <- max(sh_shooting_data$age[sh_shooting_data$male==1])
maxfemale<- max(sh_shooting_data$age[sh_shooting_data$male==0])
minmale <- min(sh_shooting_data$age[sh_shooting_data$male==1])
minfemale <- min(sh_shooting_data$age[sh_shooting_data$male==0])

maxall <- max(sh_shooting_data$age)
minall <- min(sh_shooting_data$age)

under18 <- sum(sh_shooting_data$age < 18)
under18female <- sum(sh_shooting_data$age[sh_shooting_data$male==0] < 18)
under18male <- sum(sh_shooting_data$age[sh_shooting_data$male==1] < 18)

females <- sum(sh_shooting_data$male==0)
males <- sum(sh_shooting_data$male==1)
females
males

malearrests <- 4333749
femalearrests <- 1666578
fbicrimestat <- (malearrests/(malearrests+femalearrests))*100

females2019 <- sum(sh_shooting_data$male==0 & sh_shooting_data$year==2019)
males2019 <- sum(sh_shooting_data$male==1 & sh_shooting_data$year==2019)


malearrests <- 4333749
femalearrests <- 1666578
fbicrimestat <- (malearrests/(malearrests+femalearrests))*100
malemurdersprop <- round((males2019 / malearrests),5) *100
femalemurdersprop <- round((females2019 / femalearrests),5) *100

total <- sum(sh_shooting_data$exist)

print(median_male)
print(median_female)
colourss <- c("#84E296", "#856084")
vertical_lines <- c(median_male, median_female)

# Testing out barplot  
ggplot(data = sh_shooting_data) + 
  geom_bar(mapping = aes(x = age, y = ..prop.., group = gender, fill = gender)) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  labs(title = "Murders by Gender and Age of Victim", x = "Age of Victim",y = "Percentage of Murders", fill = "Gender") +
  geom_vline(xintercept = vertical_lines, colour = colourss, size = 1, linetype = "dashed") 


sh_shooting_data %>%
  count(date) 

summary(sh_shooting_data)
class(sh_shooting_data$date)

# Creating moving average data
rolling_data <- sh_shooting_data %>%
  group_by(date) %>%
  summarise(count = n()) %>%
  mutate(death_03da = rollmean(count, k = 3, fill = NA),
         death_05da = rollmean(count, k = 5, fill = NA),
         death_07da = rollmean(count, k = 7, fill = NA),
         death_10da = rollmean(count, k = 10, fill = NA),
         death_15da = rollmean(count, k = 15, fill 
                               = NA),
         death_21da = rollmean(count, k = 21, fill = NA)) %>% 
  ungroup() %>%
  group_by(year(date)) %>%
  mutate(cmsum = cumsum(count)) %>%
  ungroup() %>%
  mutate(month = month.name[month(date)])

# Testing out moving average plot
rolling_data %>%
  filter(year(date) == 2015) %>%
  ggplot(aes(x=date, y=as.numeric(death_10da))) + geom_line() + ylim(0,5) +
  labs(title = "10-Day Moving Average", xlab="Date",ylab="Murders")

rolling_data %>%
  count(month)

sum(sh_shooting_data$race_clean == "Native American")

# Creating an aggregated race dataframe
race_data_clean <- 
data.frame(race = c("White","Black","Asian", "Other"),
  per_million = c(
  white_permil <- sum(sh_shooting_data$race_clean == "White") / as.numeric(race_data$Population[1]) * 1000000,
  black_permil <- sum(sh_shooting_data$race_clean == "Black") / as.numeric(race_data$Population[2]) * 1000000,
  asian_permil <- sum(sh_shooting_data$race_clean == "Asian") / as.numeric(race_data$Population[4]) * 1000000,
  other_permil <- sum(sh_shooting_data$race_clean == "Other") / as.numeric(race_data$Population[5]) * 1000000))

sum(sh_shooting_data$race_clean == "Asian")

# Testing out ethnicity barplot
ggplot(race_data_clean, aes(x = race, y=per_million, fill=per_million)) +
         geom_bar(stat= "identity") +
  scale_fill_continuous(high = "#624862", low = "#baa1ba",name="Per Million") + 
  labs(
    title = "Murders Per Million by Ethnicity",
    subtitle = "Ethnicity Data Sourced from 2020 United States Census"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + 
  xlab("Ethnicity") +
  ylab("Murders per Million") 

state_data_final <- 
  state_data_final %>%
  mutate(per_million = per_capita*1000000)

# Testing map
usmap::plot_usmap(data = state_data_final, values = "per_million", color = "black") +
  scale_fill_continuous(high = "#856084", low = "#DDE1E4", name = "Per Million", breaks = c(2,4,6,8,10)) + 
  theme(legend.position = "left") +
  labs(title = "Murders Per Capita (By State)")

shooting_data <- shooting_data %>%
  mutate(exists = 1)

# Saving datasets
write.csv(shooting_data_final, "/Users/francescarael/Documents/R-Shiny/App-1/data_clean.csv")
write.csv(rolling_data, "/Users/francescarael/Documents/R-Shiny/App-1/rolling_data.csv")
write.csv(state_data_final, "/Users/francescarael/Documents/R-Shiny/App-1/state_data.csv")
write.csv(race_data_clean,"/Users/francescarael/Documents/R-Shiny/App-1/race_data_clean.csv")

save(shooting_data_final, file="../App-1/shooting_data.RData")
save(rolling_data, file="../App-1/rolling_data.RData")
save(state_data_final, file="../App-1/state_data.RData")
save(race_data_clean,file="../App-1/race_data.RData")
save(shooting_data, file="../App-1/original_data.RData")
