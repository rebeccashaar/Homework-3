#Business Analytics Homework 3
#Rebecca Shaar

graphics.off()
rm(list = ls())

#Installing packages that will be used later in the assignment
library(tidyverse)
library(ggplot2)

#Question 1
#Part A
install.packages("coronavirus")
library(coronavirus)

#Part B
head(coronavirus, n = 100)

#Part C
#The date column shows the date of the summary data.
#The province and country columns denote the country name and province information if applicable.
#The lat and long columns give latitude and longitude coordinate points.
#The type column refers to the type of case, such as confirmed or death.
#The cases column gives the number of daily cases corresponding to the specified type.

#Question 2
#Part A
#Summarizing the total confirmed cases by country (top 20)
top20_countries_data <- coronavirus %>% 
  filter(type == "confirmed") %>%
  group_by(country) %>%
  summarise(total_cases = sum(cases)) %>%
  arrange(desc(total_cases))  %>%
  top_n(20)
top20_countries_data

#Part B
#Finding top 5 countries with confirmed coronavirus cases
top5_countries_data <- coronavirus %>% 
                       filter(type == "confirmed") %>%
                       group_by(country) %>%
                       summarise(total_cases = sum(cases)) %>%
                       top_n(5)
#Plotting the data
top5_countries_barplot <- ggplot(data = top5_countries_data, aes(x=reorder(country, -total_cases), y = total_cases/1000)) +
     geom_bar(stat="identity", color="steelblue", fill="skyblue", width=0.5) +
     labs(title ="Top 5 Countries by Total Cases", 
          subtitle = "Coronavirus Data",
          x ="Country", 
          y = "Cases (in thousands)") +
     theme(text=element_text(size=14,  family="serif"), 
           panel.background = element_rect(fill = "#BFD5E3", colour = "gray"))
top5_countries_barplot 

#Part C
#Converting to a horizontal barplot
top5_countries_barplot + coord_flip()

#Question 3
#Part A
#Creating a a data frame of confirmed cases by date
recent_cases <- coronavirus %>% 
                filter(type == "confirmed") %>%
                group_by(date) %>%
                summarise(total_cases = sum(cases))

#Part B
#Plotting the data as a line graph
recent_cases_line_graph <- ggplot(data = recent_cases, aes(x=date, y = total_cases/1000)) +
  geom_line(stat="identity", color="steelblue") +
  labs(title ="Total Cases Overtime", 
       subtitle = "Coronavirus Data",
       x ="Date", 
       y = "Cases (in thousands)") +
  theme(text=element_text(size=14,  family="serif"), 
        panel.background = element_rect(fill = "#BFD5E3", colour = "gray"))
recent_cases_line_graph 
  
#Extra Credit
#I scaled the axes to make the number of cases more understandable
#I changed the background color of the graph to a custom light blue
#I added a gray outline
#I changed the color of the bars
#I added an outline color to the bars
#I added axis labels that are more descriptive than the default
#I changed the width of the bars
#I added a subtitle specifying that we are looking at Coronavirus data
#I ordered the bars in descending order by number of cases
#I changed the font
#I changed the text size
