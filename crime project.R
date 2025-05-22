
# Load tidyverse for data wrangling
install.packages("tidyverse")
library(tidyverse)

# Import crime data (2001-2013)
crime1 <- read.csv("C:/Users/SHUMAIL AHSAN/Documents/Crimes_in_india_2001-2013(in)(in).csv")

# Preview the data
head(crime1)
 
# Understand structure and summary
str(crime1)
summary(crime1)


# Remove rows where DISTRICT is TOTAL or ZZ TOTAL
crime <- crime1 %>%
  filter(DISTRICT != "TOTAL" & DISTRICT !="ZZ TOTAL" & DISTRICT !="DELHI UT TOTAL")

view(crime)

# Check for NULLs and duplicates
sum(is.null(crime))
sum(duplicated(crime))

# Standardize STATE.UT names to uppercase
crime <- crime %>%
  mutate(STATE.UT = toupper(STATE.UT))


# Convert STATE.UT and YEAR to factor
crime <- crime %>%
  mutate(across(c(STATE.UT, YEAR), as.factor))

# Check the changes
str(crime)
n_distinct(crime$STATE.UT)
unique(crime$STATE.UT)

# Create a total crime column per district-year
crime <- crime %>%
  mutate(total_crime = rowSums(across(MURDER:IMPORTATION.OF.GIRLS.FROM.FOREIGN.COUNTRIES), na.rm = TRUE))

head(crime)

# Aggregate total crime by state
crime_by_state <- crime %>%
  group_by(STATE.UT) %>%
  summarise(total_crime = sum(total_crime, na.rm = TRUE))

# Aggregate total crime by years
crime_by_year <- crime %>%
  group_by(YEAR) %>%
  summarise(total_crime = sum(total_crime, na.rm = TRUE))

# top 5 Highest crime States
top_5_states <- crime_by_state %>%
  arrange(desc(total_crime)) %>%
  slice(1:5)
# Top 5 least Crime State
bottom_5_states <- crime_by_state %>%
  arrange(total_crime) %>%
  slice(1:5)
#  Visualize Top 5 States by Total Crime
ggplot(top_5_states, aes(x = STATE.UT, y = total_crime, fill =total_crime)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "black") +
  labs(title = "Top 5 States by Total Crime", x = "State/UT", y = "Total Crime") +
  theme_minimal()
# Visualize Bottom 5 States by Total Crime
ggplot(bottom_5_states,aes(x=STATE.UT, y=total_crime,fill = total_crime))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low = "palegreen4",high = "orange4")+
  labs(title = "Bottom 5 States by Total Crime", x="States/UT",y="Total Crime")+
  theme_minimal()

# Bar Chart: Total Crime by Year
ggplot(crime_by_year, aes(x = factor(YEAR), y = total_crime, fill = total_crime)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "#ffe6e6", high = "#990000") +
  labs(title = "Total Crime per Year",
       x = "Year",
       y = "Total Crime") +
  theme_minimal()

# Calculating Year-over-Year Growth Rate (%)
crime_by_year <- crime_by_year %>%
    mutate(growth_rate = (total_crime - lag(total_crime)) / lag(total_crime) * 100)

#Convert YEAR to numeric for plotting
crime_by_year$YEAR <- as.numeric(as.character(crime_by_year$YEAR))

#Remove NA in first growth rate year (Zero)
crime_by_year <- na.omit(crime_by_year)

# Line Chart: Growth Rate Over Time
ggplot(crime_by_year, aes(x = YEAR, y = growth_rate)) +
  geom_line(color = "#d62728", linewidth = 1.2) +
  geom_point(color = "black", size = 2) +
  labs(
    title = "Growth Rate of Crime Over Years",
    x = "Year",
    y = "Growth Rate (%)"
  ) +
  theme_bw()


# Total Crime by category
category_crime_by_year <- crime %>%
  group_by(YEAR) %>%
  summarise(
    total_rape = sum(RAPE, na.rm = TRUE),
    total_kidnapping=sum(KIDNAPPING...ABDUCTION),
    total_assault=sum(ASSAULT.ON.WOMEN.WITH.INTENT.TO.OUTRAGE.HER.MODESTY),
    total_dowry_deaths=sum(DOWRY.DEATHS),
    total_importation_of_girl = sum(IMPORTATION.OF.GIRLS.FROM.FOREIGN.COUNTRIES)
    )


#  Reshaping: Converting to Long Format for Easier Visualization
category1_crime_by_year <- category_crime_by_year %>%
  pivot_longer(cols = -YEAR, names_to = "Crime_Type", values_to = "Count")

# Converting YEAR and Crime_Type to factors 
category1_crime_by_year$YEAR <- as.factor(category1_crime_by_year$YEAR)
category1_crime_by_year$Crime_Type <- as.factor(category1_crime_by_year$Crime_Type)

# Plotting Crime Trends Over the Years
ggplot(category1_crime_by_year, aes(x = YEAR, y = Count, color = Crime_Type, group = Crime_Type)) +
  geom_line(linewidth = 1.2) +
  labs(title = "Crime Trends in India (2001–2013)",
       x = "Year",
       y = "Count of Cases") +
  theme_minimal()


# Calculating Crime Growth Rates by Category
growth_rate_of_crime <- category_crime_by_year %>%
  mutate(
    rape_Growth_Rate = (total_rape-lag(total_rape)) / lag(total_rape)* 100,
    kidnapping_Growth_Rate = (total_kidnapping- lag(total_kidnapping))/ lag(total_kidnapping) * 100,
    total_assault_Growth_Rate = ( total_assault- lag(  total_assault))/ lag(total_assault) * 100,
    dowry_deaths_Growth_rate = (total_dowry_deaths - lag(total_dowry_deaths)) / lag(total_dowry_deaths) * 100,
  )

# Selecting the relevant columns and removing NA values
growth_rate_of_crime <- growth_rate_of_crime %>% select(1,7,8,9,10)
growth_rate_of_crime <- na.omit(growth_rate_of_crime)

# Reshaping: Converting to Long Format for Easier Visualization
growth_rate_of_crime  <- growth_rate_of_crime  %>%
  pivot_longer(cols = -YEAR, names_to = "Crime_Type_Growth", values_to = "Count")

# Converting 'YEAR' and 'Crime_Type_Growth' to factors
growth_rate_of_crime$YEAR <- as.factor(growth_rate_of_crime$YEAR)
growth_rate_of_crime$Crime_Type_Growth <- as.factor(growth_rate_of_crime$Crime_Type_Growth)

# Plotting Growth Rates of Crimes Over Time
ggplot(growth_rate_of_crime, aes(x = YEAR, y = Count, color = Crime_Type_Growth, group = Crime_Type_Growth)) +
  geom_line(size = 1.2) +
  labs(title = "Growth Rate of Each Crime in India (2001–2013)",
       x = "Year",
       y = "Growth Rate") +
  theme_minimal()

# Aggregating Total Crime for Each State
state_crime_total <- crime %>%
  group_by(STATE.UT) %>%
  summarise(
    total_rape = sum(RAPE, na.rm = TRUE),
    total_kidnapping = sum(KIDNAPPING...ABDUCTION, na.rm = TRUE),
    total_assault = sum(ASSAULT.ON.WOMEN.WITH.INTENT.TO.OUTRAGE.HER.MODESTY, na.rm = TRUE),
    total_dowry_deaths = sum(DOWRY.DEATHS, na.rm = TRUE),
    total_importation_of_girl = sum(IMPORTATION.OF.GIRLS.FROM.FOREIGN.COUNTRIES, na.rm = TRUE),
    total_crime = total_rape + total_kidnapping + total_assault + total_dowry_deaths + total_importation_of_girl)

head(state_crime_total)


literacy <- read.csv("C:/Users/SHUMAIL AHSAN/Documents/LiteracyRate.csv")

literacy<- literacy %>% rename( STATE.UT= States)

literacy <- literacy %>% select(2:5)

literacy<- literacy %>%
  mutate(STATE.UT = toupper(STATE.UT))

literacy_crime <- merge(state_crime_total,literacy,by="STATE.UT")


# Scatter plot to visualize the relationship between total crime and literacy

ggplot(literacy_crime, aes(x = Average, y = total_crime, color = total_crime)) +
  geom_point(size = 3) + 
  scale_color_gradient(low = "orange4", high = "red4") + 
  labs(title = "Total Crime vs Literacy Rate by State", x = "Average Literacy (%)", y = "Total Crime") +
  theme_minimal()

