#Loading necessary libraries for the project

library(tidyverse)
library(palmerpenguins)
library(dplyr)

#Exploring the data set
#The View() function opens the entire data set in a different window

View(penguins)

#Using the glimpse() function to view a summary of the data set

glimpse(penguins)

#This data set has a total of 344 rows and 8 columns
#The head function displays the first few rows of the data set

head(penguins)

#Creating a data frame with kilograms instead of grams and meters instead of millimeters

new_penguins <- penguins %>% 
  mutate(body_mass_kg = body_mass_g/1000,
         flipper_length_m = flipper_length_mm/1000,
         bill_length_m = bill_length_mm/1000,
         bill_depth_m = bill_depth_mm/1000)

#Summarizing the mean and max bill length for each penguin species

new_penguins %>% 
  group_by(species) %>% 
  drop_na() %>% 
  summarize(mean_bl = mean(bill_length_m), max_bl = max(bill_length_m))

#Summarizing the mean and max bill depth for each penguin species

new_penguins %>% 
  group_by(species) %>% 
  drop_na() %>% 
  summarize(mean_bd = mean(bill_depth_m), max_bd = max(bill_depth_m))

#Summarizing the mean and max flipper length for each penguin species

new_penguins %>% 
  group_by(species) %>% 
  drop_na() %>% 
  summarize(mean_fl = mean(flipper_length_m), max_fl = max(flipper_length_m))

#Summarizing the mean and max body mass for each penguin species

new_penguins %>% 
  group_by(species) %>% 
  drop_na() %>% 
  summarize(mean_bm = mean(body_mass_kg), max_bm = max(body_mass_kg))

#Visualizing bill length vs body mass
ggplot(data=new_penguins) +
  geom_smooth(mapping = aes(x=bill_length_m, y=body_mass_kg), color="black") +
  geom_point(mapping = aes(x = bill_length_m, y = body_mass_kg, color = species, shape = species))

#Visualizing bill depth vs body mass
ggplot(data=new_penguins) +
  geom_smooth(mapping = aes(x=bill_depth_m, y=body_mass_kg), color="black") +
  geom_point(mapping = aes(x = bill_depth_m, y = body_mass_kg, color = species, shape = species))

#Visualizing flipper length vs body mass
ggplot(data=new_penguins) +
  geom_smooth(mapping = aes(x=flipper_length_m, y=body_mass_kg), color="black") +
  geom_point(mapping = aes(x = flipper_length_m, y = body_mass_kg, color = species, shape = species))
