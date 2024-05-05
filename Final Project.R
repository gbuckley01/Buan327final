library(dplyr)
library(ggplot2)
library(maps)

# Assume 'cost_report' is your dataset loaded with columns 'County' and 'Number_of_Beds'
cost_report <- read.csv("2021_CostReport.csv")  # Update path to your dataset
provider_info <-read.csv("ProviderInfo_2021.csv")

colnames(cost_report)
colnames(provider_info)

cost_prov_data <- inner_join(cost_report, provider_info, by = c("Street.Address" = "Provider.Address"))


# a)
cost_report %>% summarise(mean(Gross.Revenue, na.rm = TRUE))

# b)
cost_report %>% summarise(mean(Net.Income, na.rm = TRUE))

#c) 
county_profit <- cost_report %>%
  group_by(County) %>%
  summarise(Total_Gross_Revenue = sum(Gross.Revenue, na.rm = TRUE),
            Total_Net_Income = sum(Net.Income, na.rm = TRUE))

county_margin <- county_profit %>%
  mutate(Profit_Margin = (Total_Net_Income / Total_Gross_Revenue) * 100)

county_margin

#d) 
county_margin %>%
  arrange(desc(Profit_Margin)) %>%
  slice_head(n = 10)

# e)
cost_report %>%
  group_by(County)%>%
  arrange(Number.of.Beds)

#Map Privided:
counties <- map_data("county")

# Prepare the data: clean and aggregate
county_map <- cost_report %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County) %>%
  summarise(Total_Beds = sum(Number.of.Beds, na.rm = TRUE))

# Prepare the map data: merge and clean
counties <- counties %>%
  mutate(subregion = tolower(subregion)) %>%
  left_join(county_map, by = c("subregion" = "County"))

# Plot the map
ggplot(data = counties, aes(x = long, y = lat, group = group, fill = Total_Beds)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Total Beds") +
  labs(title = "Total Number of Beds by County") +
  theme_minimal()   

#Map 1:
county_prof_marg <- county_margin %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County)

# Merge with map data
county_profit_margin <- counties %>%
  left_join(county_prof_marg, by = c("subregion" = "County"))

# Plot the map for profit margin
ggplot(data = county_profit_margin, aes(x = long, y = lat, group = group, fill = Profit_Margin)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Profit Margin (%)") +
  labs(title = "Profit Margin per County") +
  theme_minimal()

#map 2:
county_net_prof <- county_profit %>%
  mutate(County = tolower(gsub(" county", "", County))) %>%
  group_by(County)

# Merge with map data
county_net_profit <- counties %>%
  left_join(county_net_prof, by = c("subregion" = "County"))

# Plot the map with net income
ggplot(data = county_net_profit, aes(x = long, y = lat, group = group, fill = Total_Net_Income)) +
  geom_polygon(color = "white") +
  coord_quickmap() +
  scale_fill_viridis_c(option = "plasma", name = "Net Income") +
  labs(title = "Net Income per County") +
  theme_minimal()


#graph of my choice:

cost_rating <- cost_prov_data %>%
  filter(!is.na(SNF.Average.Length.of.Stay.Title.XIX) & !is.na(Overall.Rating))

ggplot(data = cost_rating, aes(x = Overall.Rating, y = SNF.Average.Length.of.Stay.Title.XIX, group = Overall.Rating)) +
  geom_boxplot() +  # Boxplot
  labs(
    x = "Overall Rating",
    y = "SNF Average Length of Stay (Title XIX)",
    title = "Boxplot of Length of Stay by Ratings"
  ) +
  theme_minimal()

#pie chart
state_to_region <- c(
  "CA" = "West",
  "OR" = "West",
  "WA" = "West",
  "NV" = "West",
  "ID" = "West",
  "MT" = "West",
  "WY" = "West",
  "CO" = "West",
  "UT" = "West",
  "AZ" = "West",
  "NM" = "West",
  "HI" = "West",
  "AK" = "West",
  "TX" = "South",
  "OK" = "South",
  "AR" = "South",
  "LA" = "South",
  "MS" = "South",
  "AL" = "South",
  "TN" = "South",
  "KY" = "South",
  "WV" = "South",
  "VA" = "South",
  "NC" = "South",
  "SC" = "South",
  "GA" = "South",
  "FL" = "South",
  "NY" = "Northeast",
  "NJ" = "Northeast",
  "PA" = "Northeast",
  "MA" = "Northeast",
  "CT" = "Northeast",
  "RI" = "Northeast",
  "NH" = "Northeast",
  "VT" = "Northeast",
  "ME" = "Northeast",
  "IL" = "Midwest",
  "IN" = "Midwest",
  "OH" = "Midwest",
  "MI" = "Midwest",
  "WI" = "Midwest",
  "MN" = "Midwest",
  "IA" = "Midwest",
  "MO" = "Midwest",
  "ND" = "Midwest",
  "SD" = "Midwest",
  "NE" = "Midwest",
  "KS" = "Midwest"
)

# Add a new column 'Region' to the DataFrame based on State Code
cost_prov_data$Region <- state_to_region[cost_prov_data$State.Code]

# View the updated DataFrame
print(cost_prov_data$Region)



income_by_region <- aggregate(Total.Income ~ Region, data = cost_prov_data, sum)

# Step 2: Create a Pie Chart
with(income_by_region, {
  pie(Total.Income, labels = paste(Region, ": $", Total.Income), 
      main = "Total Income by Region",
      col = rainbow(length(Region)), cex = 0.8)
})


#ANOVA

# Filter data for California, New York, and Texas
filtered_data <- cost_report %>%
  filter(State.Code %in% c("CA", "NY", "TX")) %>%
  select(State.Code, Gross.Revenue) %>%
  na.omit()  # Remove rows with NA values in Net Patient Revenue

# Perform ANOVA
anova_result <- aov(cost_report$Gross.Revenue ~ cost_report$State.Code, cost_report = filtered_data)
summary(anova_result)

#Regression:
lm(Total_Gross_Revenue~Total_Net_Income, county_margin)

 