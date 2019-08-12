options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
?titanic_train
table(titanic$Survived)
table(titanic$Pclass)
table(titanic$Sex)
table(titanic$SibSp)
table(titanic$Parch)
table(titanic$Fare)

titanic %>%
  ggplot(aes(Age, color = Sex)) +
  geom_density(aes(y = ..count..),alpha=0.2)
sum(titanic$Age < 17 & titanic$Sex == 'female', na.rm = TRUE) / sum( titanic$Sex == 'female', na.rm = TRUE) 
sum(titanic$Age < 17 & titanic$Sex == 'male', na.rm = TRUE) / sum( titanic$Sex == 'male', na.rm = TRUE) 
sum(titanic$Age == 40 & titanic$Sex == 'female', na.rm = TRUE) 
sum(titanic$Age == 40 & titanic$Sex == 'male', na.rm = TRUE) 
sum(titanic$Age >=18  &titanic$Age <=35  & titanic$Sex == 'female', na.rm = TRUE) / sum( titanic$Sex == 'female', na.rm = TRUE) 
sum(titanic$Age >=18  &titanic$Age <=35  & titanic$Sex == 'male', na.rm = TRUE) / sum( titanic$Sex == 'male', na.rm = TRUE) 

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(sample = (Age))) +
  geom_qq( dparams = params) +
  geom_abline()

titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge())

titanic %>%
  ggplot(aes(Age, fill = Survived)) +
  geom_density(aes(y = ..count..),alpha=0.2)

titanic %>%
  filter(Fare != 0 ) %>%
  ggplot(aes(y = log(Fare, base = 2), x = Survived)) +
  geom_boxplot() +
  geom_jitter(alpha = 0.2)

#correct
titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(alpha = 0.2)

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar(position = position_fill())

titanic %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(Age, fill = Survived)) +
  geom_density(aes(y = ..count..),alpha=0.2) +
  facet_grid(Sex ~ Pclass)

#Properties of Stars Exercises
update.packages()
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)

mean(stars$magnitude)
sd(stars$magnitude)

stars %>%
  ggplot(aes(magnitude)) +
  geom_density()

stars %>%
  ggplot(aes(temp)) +
  geom_density()

stars %>%
  ggplot(aes(temp, magnitude)) +
  geom_point() +
  geom_smooth()

stars %>%
  ggplot(aes(log10(temp), magnitude)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous(trans = "reverse") 

stars %>%
  filter(temp>5000)  %>%
  ggplot(aes((temp), magnitude, label = star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous(trans = "reverse") +
  geom_text(aes(label = star))

stars %>%
  ggplot(aes((temp), magnitude, label = star)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous(trans = "reverse") +
  geom_text(aes(label = star))

stars %>%
  ggplot(aes(log10(temp), magnitude, colour = type)) +
  geom_point() +
  scale_y_reverse() +
  scale_x_continuous(trans = "reverse") 

#Climate Change Exercises
library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  pull(year) %>%
  min()

temp_carbon$carbon_emissions[temp_carbon$year == '2014'] /temp_carbon$carbon_emissions[temp_carbon$year == '1751']

temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  select(year) %>%
  max()

temp_anomaly <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) 

min(temp_anomaly$year)
which.max(temp_anomaly$year)
temp_anomaly$temp_anomaly[which.max(temp_anomaly$year)]-temp_anomaly$temp_anomaly[which.min(temp_anomaly$year)]

p <- temp_anomaly %>% 
  ggplot(aes(year,temp_anomaly)) +
  geom_point()

p <- p + geom_hline(aes(yintercept = 0), col = "blue")

p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p <- p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

p + 
  geom_line(aes(year, ocean_anomaly), col = "blue") +
  geom_line(aes(year, land_anomaly), col = "green") 
  
greenhouse_gases %>%
  ggplot(aes(year, concentration)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(xintercept = 1850) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

temp_carbon %>%
  ggplot(aes(year,carbon_emissions)) +
  geom_line()

co2_time <- historic_co2 %>%
  ggplot(aes(year,co2, col = source)) +
  geom_line()

co2_time +
  xlim(c(-800000,-775000))
 
co2_time +
  xlim(c(-375000,-330000))

co2_time +
  xlim(c(-140000,-120000))

co2_time +
  xlim(c(-3000,2018))

  
  
