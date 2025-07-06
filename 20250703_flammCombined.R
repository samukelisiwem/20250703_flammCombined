
setwd("/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/THESIS Writing/Data collection/Data")

#Functions used
library(tidyverse)
library(readxl)
library(writexl)

#list.files("/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/THESIS Writing/Data collection/Data")

# See what sheets are in the Excel workbook
excel_sheets("20250626_msc_cederberg_combined_flammability.xlsx")

CombinedData <- read_xlsx("20250626_msc_cederberg_combined_flammability.xlsx", sheet = "Combined")
CombinedData
View(CombinedData)

#add units 
CombinedData <- CombinedData %>%
  rename(
    `PostBurntMassEstimate (%)` = PostBurntMassEstimate,
    `MaximumFlameTemperature (°C)` = MaximumFlameTemperature,
    `TimeToFlaming (s)` = TimeToFlaming,
     )

CombinedData <- CombinedData %>%
  rename(
        `FuelMoistureContent (%)` = `FMC_percentage`
  )


#summarizing data by species-means
SpeciesFlammMean <- CombinedData %>% 
  group_by(species_name) %>%
  summarize(
    `TimeToFlaming (s)` = mean(`TimeToFlaming (s)`, na.rm = TRUE), 
    `MaximumFlameTemperature (°C)` = mean(`MaximumFlameTemperature (°C)`, na.rm = TRUE), 
    `PostBurntMassEstimate (%)` = mean(`PostBurntMassEstimate (%)`, na.rm = TRUE)
  ) %>% 
  print(n = Inf)
# Reshape to long format
##long format means transforming wide-format data so that each variable is stored in its own row instead of its own column
SpeciesLong <- SpeciesFlammMean %>%
  pivot_longer(
      cols = c(`TimeToFlaming (s)`, `PostBurntMassEstimate (%)`, `MaximumFlameTemperature (°C)`),
    names_to = "Variable",
    values_to = "Value"
  )
# Plot Faceted bar plots
ggplot(SpeciesLong, aes(x = reorder(species_name, Value), y = Value, fill = Variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(title = "Species-level Summary", x = "Species", y = "Mean Value") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none")

#
#write_xlsx(SpeciesFlammMean, "SpeciesFlammMeans.xlsx")

#flammabilityIndex mean
FlammIndex <- CombinedData %>%
  group_by(species_name) %>%               # group by species
  summarise(FImean = mean(FlammabilityIndex, na.rm = TRUE))   # calculate mean per spp
#plotFImeans
ggplot(FlammIndex, aes(x = reorder(species_name, FImean), y = FImean)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +  # flips the axes for better readability
  labs(
    title = "Mean Flammability Index per Species",
    x = "Species",
    y = "Flammability Index"
  ) +
  theme_minimal()

#add FImeans to the summary sheet
SpeciesFlammMean_joined <- SpeciesFlammMean %>%
  left_join(FlammIndex, by = "species_name")

View(SpeciesFlammMean_joined)

#save/update xlsx again
#write_xlsx(SpeciesFlammMean_joined, "SpeciesFlammMeans.xlsx")


#Flammability vs Species
#Reorder species and plot with custom y-axis and reference lines
boxplot(FlammabilityIndex ~ reorder(species_code, FlammabilityIndex, FUN = median), 
        data = CombinedData,
        las = 2,         # rotate x-axis labels for readability
        cex.axis = 0.6,  # shrink axis labels if too many species
        main = "Flammability by Species",
        xlab = "",
        ylab = "Flammability Index",
        ylim = c(0, 3),  # limit y-axis from 0 to 3
        yaxt = "n")      # suppress default y-axis
# Add custom y-axis ticks
axis(2, at = c(1, 2, 3))
##Add horizontal dotted lines at 1.5 and 2.0
#"Flammability was arbitrarily considered as ‘low’ given flammability index scores of <1.5, as ‘medium’ with scores of 1.5–2.0, and as ‘high’ with scores of >2.0." Kraaij(2024) 
abline(h = 1.5, lty = 2)
abline(h = 2.0, lty = 2)


#Flammability vs Growthform
#Reorder species and plot with custom y-axis and reference lines
boxplot(FlammabilityIndex ~ reorder(growth_form, FlammabilityIndex, FUN = median), 
        data = CombinedData,
        las = 2,         # rotate x-axis labels for readability
        cex.axis = 0.6,  # shrink axis labels if too many species
        main = "Flammability by Growth form",
        xlab = "",
        ylab = "Flammability Index")

##Flammability vs Family
boxplot(FlammabilityIndex ~ reorder(Accepted_family, FlammabilityIndex, FUN = median), 
        data = CombinedData,
        las = 2,         # rotate x-axis labels for readability
        cex.axis = 0.6,  # shrink axis labels if too many species
        main = "Flammability by Family",
        xlab = "",
        ylab = "Flammability Index",
        ylim = c(0, 3),  # limit y-axis from 0 to 3
        yaxt = "n")      # suppress default y-axis
# Add custom y-axis ticks
axis(2, at = c(1, 2, 3))

abline(h = 1.5, lty = 2)
abline(h = 2.0, lty = 2)

#filer by site these variables
CombinedData %>% 
    filter(Site == "Cederberg") %>%
    select(Vegtype, growth_form)

#filter, add new column and arrange
CombinedData %>% 
  select(Vegtype, growth_form, `FuelMoistureContent (%)`) %>%
  mutate(FMC_prop = `FuelMoistureContent (%)`/100) %>%
  arrange(desc(FMC_prop))

#
#select columns I need. maybe save this new selection in Data folder??
SelectedCombined <- CombinedData %>% 
  select(Site, Date, species_name, species_code, Accepted_family, growth_form, 
         Vegtype, `TimeToFlaming (s)`, `PostBurntMassEstimate (%)`, 
         `MaximumFlameTemperature (°C)`, FlammabilityIndex, `FuelMoistureContent (%)`)

write_xlsx(SelectedCombined, "/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/THESIS Writing/Data collection/Data/SelectedCombined.xlsx")

view(SelectedCombined)


#make long format 
selectedLong <- read_xlsx("SelectedCombined.xlsx") %>%
  pivot_longer(cols = c("TimeToFlaming (s)", "PostBurntMassEstimate (%)", "MaximumFlameTemperature (°C)", "FlammabilityIndex", "FuelMoistureContent (%)"),
               names_to = "variable", values_to = "value") 

write_xlsx(selectedLong, "/Users/samukelisiwem/OneDrive - Nelson Mandela University/Documents/3_SCHOLARY/PhD/THESIS Writing/Data collection/Data/selectedLong.xlsx")

#convert to wide format ??
selectedWide <- selectedLong %>%
  pivot_wider(
    id_cols = c(Site, Date, species_name),
    names_from = variable,
    values_from = value,
    values_fn = list
  )                          #not sure about this...


