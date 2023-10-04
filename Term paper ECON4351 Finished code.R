### Term paper ECON4351 ###

# install.packages("WDI")
library(WDI) 
library(tidyverse)

# Download numbers for GDP from WDI:

indicator <- "NY.GDP.MKTP.CD"  # GDP at current US dollars
country <- "all"  # Retrieve data for all countries
start_year <- 1960  # Start year for the data
end_year <- 2011  # End year for the data

gdp_data <- WDI(indicator, country = country, start = start_year, end = end_year)

# Define a variable for the countries of interest:

countries_of_interest <- c("JPN","USA","PRT","IRL", "GRC", "ESP", "ITA", "TUR", "DEU", "FRA", "NLD",
                           "BEL", "SWE", "DNK", "LUX", "ISL", "GBR", "CHE", "NOR", "CAN", "FIN")

# Download data for populations:

indicator_pop <- "SP.POP.TOTL" #Population in countries in given years
pop_data <- WDI(indicator_pop, country = countries_of_interest, start = start_year, end = end_year)

# Tidy the data for GDP and Populations to make it easier to work with:

gdp_data_OECD <- gdp_data %>%
  filter(iso3c %in% countries_of_interest) %>%
  rename(GDP = "NY.GDP.MKTP.CD") %>%
  select(country, iso3c, year, GDP) %>%
  filter(year %in% c(1960,2011))

pop_data_OECD <- pop_data %>%
  rename(Population = "SP.POP.TOTL") %>%
  filter(year %in% c(1960,2011)) %>%
  select(country, iso3c, year, Population)

# We see that Germany and Denmark lack data for GDP in 1960, and therefore we cant calculate growth.
# I therefore filter them out of the data:

gdp_data_OECD <- gdp_data_OECD %>%
  filter(iso3c != "DNK") %>%
  filter(iso3c != "DEU")

pop_data_OECD <- pop_data_OECD %>%
  filter(iso3c != "DNK") %>%
  filter(iso3c != "DEU")

# Merge the data, and create GDP per capita:

combined_data <- left_join(pop_data_OECD, gdp_data_OECD)
combined_data <- combined_data %>%
  mutate(GDP_per_capita = GDP/Population)

# Create a dataset for GDP in 1960:

GDP_per_capita_1960 <- combined_data %>%
  filter(year == 1960) %>%
  select(country, iso3c, GDP_per_capita)

gdp_per_capita_usa_1960 <- GDP_per_capita_1960$GDP_per_capita [GDP_per_capita_1960$iso3c == "USA"]

Normalized_GDP_per_capita_1960 <- GDP_per_capita_1960 %>%
  mutate(normalized_gdp_1960 = GDP_per_capita/gdp_per_capita_usa_1960) %>%
  select(country, iso3c, normalized_gdp_1960)

# Create a dataset for growth-rate:

data_111 <- combined_data %>%
  select(iso3c, year, GDP_per_capita)
wide_data <- data_111 %>% pivot_wider(names_from = year,
                                      values_from = GDP_per_capita) %>%
  rename(GDP_per_capita_2011 = "2011") %>%
  rename(GDP_per_capita_1960 = "1960")

# Create a variable for growth rate:

wide_data_1 <- wide_data %>%
  mutate(Growth_rate_1960_2011 = ((GDP_per_capita_2011- GDP_per_capita_1960)/GDP_per_capita_1960)/(2011-1960))

# Merge data for Normalized GDP in 1960 and Growth rate:

fig_data <- left_join(wide_data_1, Normalized_GDP_per_capita_1960)
fig_data_1 <- fig_data %>%
  select(country, Growth_rate_1960_2011, normalized_gdp_1960)

# Make the plot:

ggplot(fig_data_1, aes(x=normalized_gdp_1960, y=Growth_rate_1960_2011, label=country)) +
  geom_point() +
  geom_text() +
  labs(x="GDP per person (US=1) in 1960", y= "Growth rate 1960-2011", title = "Convergence in the OECD")

## Now for Africa ##
# We mostly reuse much of the code:


# Define a variable for the countries of interest:

countries_of_interest_1 <- c("ZWE", "ZMB", "UGA", "TGO", "SDN", "ZAF", "SLE", "SEN",
                             "RWA", "NGA", "USA")

# Download data for populations:

indicator_pop <- "SP.POP.TOTL" #Population in countries in given years
pop_data <- WDI(indicator_pop, country = countries_of_interest_1, start = start_year, end = end_year)

# Tidy the data for GDP and Populations to make it easier to work with:

gdp_data_AFR <- gdp_data %>%
  filter(iso3c %in% countries_of_interest_1) %>%
  rename(GDP = "NY.GDP.MKTP.CD") %>%
  select(country, iso3c, year, GDP) %>%
  filter(year %in% c(1960,2011))

pop_data_AFR <- pop_data %>%
  rename(Population = "SP.POP.TOTL") %>%
  filter(year %in% c(1960,2011)) %>%
  select(country, iso3c, year, Population)

# Merge the data, and create GDP per capita:

combined_data_AFR <- left_join(pop_data_AFR, gdp_data_AFR)
combined_data_AFR <- combined_data_AFR %>%
  mutate(GDP_per_capita = GDP/Population)

# Create a dataset for GDP in 1960:

GDP_per_capita_1960_AFR <- combined_data_AFR %>%
  filter(year == 1960) %>%
  select(country, iso3c, GDP_per_capita)

gdp_per_capita_usa_1960 <- GDP_per_capita_1960_AFR$GDP_per_capita [GDP_per_capita_1960_AFR$iso3c == "USA"]

Normalized_GDP_per_capita_1960_AFR <- GDP_per_capita_1960_AFR %>%
  mutate(normalized_gdp_1960_AFR = GDP_per_capita/gdp_per_capita_usa_1960) %>%
  select(country, iso3c, normalized_gdp_1960_AFR)

# Create a dataset for growth-rate:

data_111_AFR <- combined_data_AFR %>%
  select(iso3c, year, GDP_per_capita)
wide_data_AFR <- data_111_AFR %>% pivot_wider(names_from = year,
                                              values_from = GDP_per_capita) %>%
  rename(GDP_per_capita_2011 = "2011") %>%
  rename(GDP_per_capita_1960 = "1960")

# Create a variable for growth rate:

wide_data_1_AFR <- wide_data_AFR %>%
  mutate(Growth_rate_1960_2011 = ((GDP_per_capita_2011- GDP_per_capita_1960)/GDP_per_capita_1960)/(2011-1960))

# Merge data for Normalized GDP in 1960 and Growth rate:

fig_data_AFR <- left_join(wide_data_1_AFR, Normalized_GDP_per_capita_1960_AFR)
fig_data_1_AFR <- fig_data_AFR %>%
  select(country, Growth_rate_1960_2011, normalized_gdp_1960_AFR)

# Make the plot:

ggplot(fig_data_1_AFR, aes(x=normalized_gdp_1960_AFR, y=Growth_rate_1960_2011, label=country)) +
  geom_point() +
  geom_text() +
  labs(x="GDP per person (US=1) in 1960", y= "Growth rate 1960-2011", title = "Convergence in Africa")
