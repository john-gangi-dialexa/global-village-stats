library(dplyr)

# Estimated world population and continent populations in 2021
world_pop_2021 <- 7.8e9
continent_pop_2021 <- data.frame(
  continent = c("Africa", "Asia", "Europe", "North America", "South America", "Australia"),
  population = c(1.3e9, 4.7e9, 747e6, 592e6, 430e6, 39e6),
  population_density = c(44, 150, 73, 23, 24, 3),
  average_annual_wage = c(3000, 10000, 40000, 40000, 15000, 50000)
)

# Sample housing type percentages for each continent
housing_types <- c("detached", "apartment", "factory_dorm", "townhouse", "duplex", "bungalow", "mobile_home", "co_housing", "informal_settlement", "villa")

housing_data <- data.frame(
  continent = rep(c("Africa", "Asia", "Europe", "North America", "South America", "Australia"), each = length(housing_types)),
  housing_type = rep(housing_types, 6),
  housing_type_percentage = c(
    40, 20, 5, 5, 5, 5, 5, 5, 5, 0,
    30, 40, 5, 5, 5, 5, 0, 5, 0, 5,
    30, 40, 0, 10, 5, 5, 0, 0, 0, 10,
    40, 30, 0, 5, 10, 5, 5, 0, 0, 5,
    40, 30, 0, 10, 5, 5, 5, 0, 0, 5,
    50, 10, 0, 10, 5, 10, 0, 5, 0, 10
  )
)

# Scale the population to 5000
scaled_continent_pop <- continent_pop_2021 %>%
  mutate(
    scaled_population = (population / world_pop_2021) * 5000
  )

# Join the housing data with the scaled continent population data
scaled_continent_pop_housing <- scaled_continent_pop %>%
  inner_join(housing_data, by = "continent") %>%
  mutate(
    housing_population = round(scaled_population * housing_type_percentage / 100)
  )

# Perform cross-tabulation and create a three-way table
three_way_table <- with(scaled_continent_pop_housing, ftable(continent, housing_type, housing_population))
print(three_way_table)

