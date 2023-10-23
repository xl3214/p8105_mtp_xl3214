p8105_midterm_xl3214
================
Xuan Lu
2023-10-19

## Report Overview

This project examines NYC ZIP code-level population changes using USPS
Change of Address (COA) data. We clean, merge, and perform exploratory
analysis to identify population shifts, investigate data quality, and
reveal demographic trends while addressing dataset limitations. The raw
COA dataset contains 5 variables and 11845 observations, while the raw
Zip Codes dataset includes 7 variables and 324 observations.

## Data Wrangling

``` r
# Define file paths and read data from Excel and CSV files
file_path <- "USPS CHANGE OF ADDRESS NYC.xlsx"
sheet_names <- excel_sheets(file_path)
data_frames <- lapply(sheet_names, function(sheet_name) {
  read_excel(file_path, sheet = sheet_name)
})

# Combine COA data, clean column names, and create new columns
coa <- bind_rows(data_frames) |>
  clean_names() |>
  rename(zip_code = zipcode) |>
  mutate(year = year(month)) |>
  mutate(net_change = total_perm_in - total_perm_out)

# Read ZIP code data from CSV, clean column names, and add a 'borough' variable
zip <- read.csv("Zip Codes.csv") |> 
  janitor::clean_names() |> 
  mutate(borough = county_name)

# Compare COA and ZIP data using joins
test1 <- anti_join(coa, zip, by = "zip_code") # 0 observations of 7 variables
test2 <- left_join(coa, zip, by = "zip_code") # 12085 observations of 14 variables
test3 <- right_join(coa, zip, by = "zip_code") # 12168 observations of 14 variables
test4 <- inner_join(coa, zip, by = "zip_code") # 12085 observations of 14 variables
test5 <- full_join(coa, zip, by = "zip_code") # 12168 observations of 14 variables

# Proceed with left_join to include only COA records in the final dataset
rm(test1, test2, test3, test4, test5)

# Merge COA and ZIP data, address many-to-many relationship warning
tidy <- left_join(coa, zip, by = "zip_code")
# Warning: Detected an unexpected many-to-many relationship between `x` and `y`.
# Need to go back and edit the codes to keep a one-to-many relationship between COA and ZIP.

# Check for duplicated combinations of month and zip_code in COA dataset
coa_duplicated <- coa |>
  group_by(month, zip_code) |>
  filter(duplicated(month, zip_code))
print(coa_duplicated)
# No duplicated combinations of month and zip_code in COA dataset.

# Check for duplicated combinations of zip_code and neighborhood in ZIP dataset
zip_duplicated <- zip |>
  group_by(zip_code, neighborhood) |>
  filter(duplicated(zip_code, neighborhood))
print(zip_duplicated)
# Four duplicated combinations of zip_code and neighborhood in COA dataset: 10463, 11201, 11239, 11693
zip |> filter(zip_code %in% c(10463, 11201, 11239, 11693))

# Update 'borough' variable to fix duplicates
unique(pull(zip, county_name))
zip <- zip |> 
  mutate(borough = ifelse(county_name == "Kings", "Brooklyn",
                          ifelse(county_name == "Richmond", "Staten Island",
                                 ifelse(county_name == "New York", "Manhattan", borough))))
unique(pull(zip, borough))

# Fix duplicates of zip codes 10463, 11201, 11239, and 11693
zip <- zip |> 
  mutate(
    county_name = case_when(
      zip_code == 10463 ~ "Bronx", # Code 10463 as Bronx county
      zip_code %in% c(11201, 11239) ~ "Kings", # Code 11201 and 11239 as Kings county
      zip_code == 11693 ~ "Queens", # Code 11693 as Queens county
      TRUE ~ county_name),  # Leave other rows as they are
    borough = case_when(
      zip_code %in% c(11201, 11239) ~ "Brooklyn",
      TRUE ~ borough))  # Leave other rows as they are

# Delete duplicates based on zip_code
zip <- zip |>
  distinct(zip_code, .keep_all = TRUE)
# Checking for duplicates one last time
zip |> filter(zip_code %in% c(10463, 11201, 11239, 11693))

# Merge COA and ZIP data
tidy <- left_join(coa, zip, by = "zip_code")

# Check for NAs in 'neighborhood' in both ZIP and tidy datasets
zip |> filter(is.na(neighborhood)) # 142 NAs in neighborhood variable in ZIP dataset
tidy |> filter(is.na(neighborhood)) # 1288 NAs in neighborhood variable in tidy dataset

summary(tidy)

# List of objects to keep
objects_to_keep <- c("coa", "zip", "tidy")

# Get a list of all objects in the current environment
all_objects <- ls()

# Find objects to remove
objects_to_remove <- setdiff(all_objects, objects_to_keep)

# Remove objects to be deleted
rm(list = objects_to_remove)
rm(objects_to_remove, all_objects)

# Keep only variables needed for further analysis
tidy <- tidy |> 
  select(year, month, city, borough, county_name, neighborhood, total_perm_out, total_perm_in, net_change, zip_code)
```

Logic to clean the datasets is as follows:

Data preparation involved importing files and combining sheets in “coa”.
Column names were cleaned, and new variables (“year” and “net_change” in
“coa,” “borough” in “zip”) were created. Various joining methods were
compared for data consistency. A “left_join” merged “coa” and “zip,”
warning many-to-many relationships. Checks were conducted for duplicates
in “coa” and “zip.” “Borough” names were updated, and duplicates for
specific zip codes were addressed. NAs in “neighborhood” were reviewed
in both “zip” and merged “tidy” datasets.

The final tidy dataset has 10 columns and 11845 rows.

Variables and examples:

- year (numeric): 2018, 2019, 2020, 2021, 2022.

- month (POSIXct, POSIXt): 2018-01-01, 2018-02-01, 2018-03-01,
  2018-04-01, 2018-05-01, 2018-06-01.

- city (character): NEW YORK, STATEN ISLAND, BRONX, GLEN OAKS, FLORAL
  PARK, LONG ISLAND CITY. There are 78 unique entries.

- borough (character): Manhattan, Staten Island, Bronx, Queens,
  Brooklyn. There are 5 unique entries.

- county_name (character): New York, Richmond, Bronx, Queens, Kings.
  There are 5 unique entries.

- neighborhood (character): Chelsea and Clinton, Lower East Side, Lower
  Manhattan, NA, Gramercy Park and Murray Hill, Greenwich Village and
  Soho. There are 43 unique entries. Additionally, there are 1288
  missing entries in this variable.

- total_perm_out (numeric): range 0 to 1772, mean = 267.6287041.

- total_perm_in (numeric): range 0 to 1187, mean = 215.7765302.

- net_change (numeric): range -983 to 744, mean = -51.8521739.

- zip_code (numeric): 1.0001^{4}, 1.0002^{4}, 1.0003^{4}, 1.0004^{4},
  1.0005^{4}, 1.0006^{4}. There are 237 unique entries.

``` r
# Compare 'city' to 'borough'
comparison_result <- tidy |>
  group_by(borough, city) |>
  summarise(count = n())

# Head the comparison result
head(comparison_result)
```

    ## # A tibble: 6 × 3
    ## # Groups:   borough [2]
    ##   borough  city             count
    ##   <chr>    <chr>            <int>
    ## 1 Bronx    BRONX             1500
    ## 2 Brooklyn BROAD CHANNEL        5
    ## 3 Brooklyn BROOKLYN          2369
    ## 4 Brooklyn BROOKLYN HEIGHTS     1
    ## 5 Brooklyn FAR ROCKAWAY         6
    ## 6 Brooklyn ROCKAWAY BEACH      49

``` r
# Filter the tidy dataset for Manhattan and Queens boroughs
manhattan_data <- tidy |>
  filter(borough == "Manhattan")
queens_data <- tidy |>
  filter(borough == "Queens")

# Create tables for the most common values of "city" in Manhattan and Queens
manhattan_table <- manhattan_data |>
  count(city, sort = TRUE) |>
  head(5) # Showing top 5 common cities in Manhattan borough
manhattan_table |> knitr::kable()
```

| city             |    n |
|:-----------------|-----:|
| NEW YORK         | 3477 |
| CANAL STREET     |    4 |
| ROOSEVELT ISL    |    4 |
| ROOSEVELT ISLAND |    4 |
| BOWLING GREEN    |    1 |

``` r
queens_table <- queens_data |>
  count(city, sort = TRUE) |>
  head(5) # Showing top 5 common cities in Queens borough
queens_table |> knitr::kable()
```

| city           |   n |
|:---------------|----:|
| JAMAICA        | 372 |
| FLUSHING       | 309 |
| ASTORIA        | 230 |
| QUEENS VILLAGE | 165 |
| BAYSIDE        | 135 |

The comparison_result table (79 observations) reveals data quality
issues: ‘New York’ mismatched with both ‘Manhattan’ and ‘Brooklyn,’
indicating inconsistent city-to-borough relationships. ‘ROOSEVELT
ISLAND’ appearing as ‘ROOSEVELT ISL’ in the manhattan_table suggests
data transformation problems. Variations like ‘QUEENS VILLAGE’ and
‘QUEENS VILLAGE, QUEENS’ point to city name inconsistencies. ‘BOWLING
GREEN’ and ‘CANAL STREET’ in the ‘city’ variable may signal data entry
errors. ZIP codes with \<60 observations and missing neighborhood data
indicate potential data gaps due to factors like sparse data collection,
inconsistent reporting, demographic variations, and data source
limitations.

## EDA and Visualization

``` r
# Group and summarize data to calculate the average net_change by year and borough
avg_net_change_table <- tidy |>
  group_by(borough, year) |>
  summarize(avg_net_change = mean(net_change, na.rm = TRUE))

# View the resulting table
avg_net_change_table |> knitr::kable()
```

| borough       | year | avg_net_change |
|:--------------|-----:|---------------:|
| Bronx         | 2018 |     -46.303333 |
| Bronx         | 2019 |     -48.016667 |
| Bronx         | 2020 |     -72.653333 |
| Bronx         | 2021 |     -66.100000 |
| Bronx         | 2022 |     -53.190000 |
| Brooklyn      | 2018 |     -46.184265 |
| Brooklyn      | 2019 |     -51.683230 |
| Brooklyn      | 2020 |    -110.672065 |
| Brooklyn      | 2021 |     -76.838115 |
| Brooklyn      | 2022 |     -55.377593 |
| Manhattan     | 2018 |     -41.967422 |
| Manhattan     | 2019 |     -52.784773 |
| Manhattan     | 2020 |    -126.434610 |
| Manhattan     | 2021 |     -38.975504 |
| Manhattan     | 2022 |     -46.588055 |
| Queens        | 2018 |     -26.640479 |
| Queens        | 2019 |     -29.291275 |
| Queens        | 2020 |     -48.284367 |
| Queens        | 2021 |     -45.371778 |
| Queens        | 2022 |     -30.778542 |
| Staten Island | 2018 |      -9.846154 |
| Staten Island | 2019 |      -9.125000 |
| Staten Island | 2020 |     -10.544828 |
| Staten Island | 2021 |     -22.548611 |
| Staten Island | 2022 |     -16.298611 |

The table reveals trends in average net_change by borough and year.
Annual fluctuations, particularly significant decreases in 2020, are
observed. Brooklyn consistently exhibits the highest negative
net_change, contrasting with Staten Island’s stability. The Bronx and
Queens display varying net_change, reflecting population fluctuations,
possibly influenced by the COVID-19 pandemic in 2020. These trends may
stem from demographic shifts and housing patterns.

``` r
# Five lowest values of net_change across all observed data
lowest_net_change_all <- tidy |>
  arrange(net_change) |>
  mutate(month = month(month)) |>
  select(zip_code, neighborhood, year, month, net_change) |>
  head(5)

# Display the resulting tables
lowest_net_change_all |> knitr::kable()
```

| zip_code | neighborhood                  | year | month | net_change |
|---------:|:------------------------------|-----:|------:|-----------:|
|    10022 | Gramercy Park and Murray Hill | 2020 |     5 |       -983 |
|    10009 | Lower East Side               | 2020 |     7 |       -919 |
|    10016 | Gramercy Park and Murray Hill | 2020 |     6 |       -907 |
|    10016 | Gramercy Park and Murray Hill | 2020 |     7 |       -855 |
|    10009 | Lower East Side               | 2020 |     6 |       -804 |

``` r
# Five highest values of net_change before 2020
highest_net_change_before_2020 <- tidy |>
  filter(year < 2020) |>
  arrange(desc(net_change)) |>
  mutate(month = month(month)) |>
  select(zip_code, neighborhood, year, month, net_change) |>
  head(5)

# Display the resulting tables
highest_net_change_before_2020 |> knitr::kable()
```

| zip_code | neighborhood        | year | month | net_change |
|---------:|:--------------------|-----:|------:|-----------:|
|    11101 | Northwest Queens    | 2018 |     4 |        360 |
|    11101 | Northwest Queens    | 2018 |     6 |        344 |
|    11101 | Northwest Queens    | 2018 |     5 |        300 |
|    10001 | Chelsea and Clinton | 2018 |     7 |        225 |
|    11201 | Northwest Brooklyn  | 2018 |     4 |        217 |

``` r
# Calculate neighborhood-level average net_change by month
neighborhood_avg <- tidy |>
  group_by(borough, neighborhood, year, month) |>
  summarize(avg_net_change = mean(net_change))

# Create a line plot to visualize the trends
neighborhood_avg |>
  ggplot(aes(x = month, y = avg_net_change, color = borough, group = borough)) +
  geom_point(alpha = 0.15) +
  geom_smooth(se = FALSE) + 
  labs(title = "Neighborhood-Level Average Net Change Over 5 Years",
       x = "Month", y = "Average Net Change", color = "Borough") +
  theme_minimal() +
  theme_light() + 
  theme(legend.position = "bottom")
```

![](p8105_midterm_xl3214_files/figure-gfm/Visualizations-1.png)<!-- -->

``` r
ggsave("results/neighborhood_avg_net_change_scatter_line_plot.png", width = 8, height = 4)
```

The graph shows Neighborhood-Level Average Net Change to be mostly below
zero across all boroughs, over 5 years. The mean net change is
-51.8521739. Overall, Manhattan has the lowest average net change,
Staten Island has the highest. Net change is stable in 2018, 2019, and
2022. However, a substantial drop occurred between 2020 and 2021,
especially in Manhattan.

Mean of Neighborhood-Level Average Net Change over five years, Grouped
by borough:

- Bronx: -58.4132937

- Brooklyn: -66.0864957

- Manhattan: -83.1492555

- Queens: -34.0814803

- Staten Island: -19.9383126

Mean of Neighborhood-Level Average Net Change, Grouped by year:

- 2018: -41.6273371

- 2019: -45.3431172

- 2020: -95.8952273

- 2021: -57.2484641

- 2022: -46.412256

## Limitations

The dataset has limitations, including data quality issues, borough
mismatches, many-to-many relationships, limited time range, local
influences, and potential seasonal patterns. It lacks demographic and
socioeconomic data. Thorough preprocessing and data integration are
needed for a holistic view of population changes.

``` r
wordcountaddin::text_stats("p8105_midterm_xl3214.Rmd")
```

| Method          | koRpus      | stringi       |
|:----------------|:------------|:--------------|
| Word count      | 485         | 478           |
| Character count | 3525        | 3525          |
| Sentence count  | 70          | Not available |
| Reading time    | 2.4 minutes | 2.4 minutes   |
