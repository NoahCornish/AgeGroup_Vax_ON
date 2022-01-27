library(httr)
library(jsonlite)
library(tidyverse)
library(janitor)
library(dplyr)
library(ggplot2)

Today <- Sys.Date() # date today for filtering

#get the data
PHUvaccinedata_511_api <- fromJSON(
  "https://data.ontario.ca/api/3/action/datastore_search?resource_id=2a362139-b782-43b1-b3cb-078a2ef19524&limit=50000000000")

# clean & filter what we need
PHUvaccine_data_5_11 <- as.data.frame(PHUvaccinedata_511_api$result$records) %>% 
  filter(Date == Today) %>% 
  filter(Agegroup == "05-11yrs") %>% 
  filter(!`PHU name` == "UNKNOWN") %>% 
  rename(`Public Health Unit` = `PHU name`) %>% 
  select(Date, `Public Health Unit`, Agegroup, 
         Percent_at_least_one_dose, Percent_fully_vaccinated,
         Percent_3doses)
#set desired columns as numeric values
PHUvaccine_data_5_11$Percent_at_least_one_dose <- as.numeric(PHUvaccine_data_5_11$Percent_at_least_one_dose)
PHUvaccine_data_5_11$Percent_fully_vaccinated <- as.numeric(PHUvaccine_data_5_11$Percent_fully_vaccinated)
PHUvaccine_data_5_11$Percent_3doses <- as.numeric(PHUvaccine_data_5_11$Percent_3doses)

#remove unwanted data in the "Date" column
PHUvaccine_data_5_11$Date <- gsub("T00:00:00","", as.character(PHUvaccine_data_5_11$Date))

#converting decimals to percentages
PHUvaccine_data_5_11 <- PHUvaccine_data_5_11 %>% 
  mutate(OneDosePercent = Percent_at_least_one_dose * 100) %>% 
  mutate(TwoDosePercent = Percent_fully_vaccinated * 100) %>% 
  mutate(ThreeDosePercent = Percent_3doses * 100) %>% 
  select(Date, `Public Health Unit`, Agegroup, OneDosePercent, 
         TwoDosePercent, ThreeDosePercent)

#writing finished file to directory (csv)
write.csv(PHUvaccine_data_5_11,
          file = "5_11_VaccineData.csv",
          row.names = F)

#plot using ggplot2 
plot_fd <- ggplot(data = PHUvaccine_data_5_11, 
      aes(y = reorder(`Public Health Unit`, OneDosePercent), x = OneDosePercent, fill = -OneDosePercent)) +
      geom_bar(stat = "identity") +
      xlab("At least One Dose Percentage") + ylab("Public Health Unit") +
      expand_limits(x = c(0,80))+
      labs(title = "Percentage of children aged 5 to 11 with at least one dose by Public Health Unit",
  subtitle = "As of Jan 27th, 2022",
  caption = "Data source: ON Gov | visual: @NoahCornish") +
  theme(legend.position = "none") +
  #geom_text(aes(label = OneDosePercent), size = 3, vjust = 0) +
  theme(
  plot.title = element_text(color = "red", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "black", size = 9, face = "bold"),
  plot.caption = element_text(color = "black", size = 10, face = "bold"),
  )

#view plot
plot_fd

#plot using ggplot2 
plot_sd <- ggplot(data = PHUvaccine_data_5_11, 
                  aes(y = reorder(`Public Health Unit`, TwoDosePercent), x = TwoDosePercent, fill = -TwoDosePercent)) +
  geom_bar(stat = "identity") +
  xlab("Fully Vaccinated Percentage") + ylab("Public Health Unit") +
  expand_limits(x = c(0,100))+
  labs(title = "Percentage of children aged 5 to 11 that are fully vaccinated by Public Health Unit",
       subtitle = "As of Jan 27th, 2022",
       caption = "Data source: ON Gov | visual: @NoahCornish") +
  theme(legend.position = "none") +
  #geom_text(aes(label = OneDosePercent), size = 3, vjust = 0) +
  theme(
    plot.title = element_text(color = "red", size = 12, face = "bold"),
    plot.subtitle = element_text(color = "black", size = 9, face = "bold"),
    plot.caption = element_text(color = "black", size = 10, face = "bold"),
  )

#view plot
plot_sd



















