p8105_midterm_xl3214
================
Xuan Lu
2023-10-19

## Raw Data Overview and Report Goal

The raw USPS Change of Address NYC dataset has 5 variables and 11845
observations. The raw Zip Codes dataset has 7 variables and 324
observations.

## Data Cleaning, Tidying, Combining and Merging

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

# Merge COA and ZIP data, retaining 'city' variable for data quality check
tidy <- left_join(coa, zip, by = "zip_code")

# Check for NAs in 'neighborhood' in both ZIP and tidy datasets
zip |> filter(is.na(neighborhood)) # 142 NAs in neighborhood variable in ZIP dataset
tidy |> filter(is.na(neighborhood)) # 1288 NAs in neighborhood variable in tidy dataset

# Retain 'city' variable to investigate potential data quality issues
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
```

The logic followed to clean the datasets is as follows:

1.  Data is read from Excel and CSV files, combining the “COA” data and
    “ZIP” code data.

2.  Column names are cleaned and new columns, such as “year” and
    “net_change,” are created in the “COA” dataset.

3.  The “ZIP” code data is read from the CSV file, and column names are
    cleaned. A “borough” variable is added.

4.  Comparison tests are conducted to assess data consistency between
    “COA” and “ZIP” data.

5.  The “left_join” operation is used to merge “COA” and “ZIP” data,
    although a many-to-many relationship warning is noted.

6.  Checks are performed for duplicated combinations of month and
    zip_code in the “COA” dataset, and for duplicated combinations of
    zip_code and neighborhood in the “ZIP” dataset.

7.  The “borough” variable is updated to accurately reflect borough
    names.

8.  Duplicates for specific zip codes (10463, 11201, 11239, 11693) are
    fixed by assigning them to the correct counties and boroughs.

9.  Duplicate entries based on zip_code are removed.

10. The “city” variable is retained in the merged dataset to investigate
    data quality issues.

11. NAs in the “neighborhood” variable are checked in both the “ZIP” and
    merged “tidy” datasets.

12. The “city” variable is retained for further data quality analysis.

13. Finally, objects other than “coa,” “zip,” and “tidy” are removed
    from the environment.

After cleaning, tidying, combining, and joining the datasets, the final
tidy dataset has 14 columns and 11845 rows. Variables include:

- **month**: variable type `POSIXct, POSIXt`. Entries example:
  2018-01-01, 2018-02-01, 2018-03-01, 2018-04-01, 2018-05-01,
  2018-06-01.

- **zip_code**: variable type `numeric`. Entries example: 1.0001^{4},
  1.0002^{4}, 1.0003^{4}, 1.0004^{4}, 1.0005^{4}, 1.0006^{4}. There are
  237 unique entries in this variable.

- **city**: variable type `character`. Entries example: NEW YORK, STATEN
  ISLAND, BRONX, GLEN OAKS, FLORAL PARK, LONG ISLAND CITY. There are 78
  unique entries in this variable.

- **total_perm_out**: variable type `numeric`, range 0 to 1772, mean =
  267.6287041.

- **total_perm_in**: variable type `numeric`, range 0 to 1187, mean =
  215.7765302.

- **year**: variable type `numeric`. Entries example: 2018, 2019, 2020,
  2021, 2022. There are 5 unique entries in this variable.

- **net_change**: variable type `numeric`, range -983 to 744, mean =
  -51.8521739.

- **county_name**: variable type `character`. Entries example: New York,
  Richmond, Bronx, Queens, Kings. There are 5 unique entries in this
  variable.

- **state_fips**: variable type `integer`. Entries example: 36. There
  are 1 unique entries in this variable.

- **county_code**: variable type `integer`. Entries example: 61, 85, 5,
  81, 47. There are 5 unique entries in this variable.

- **county_fips**: variable type `integer`. Entries example: 36061,
  36085, 36005, 36081, 36047. There are 5 unique entries in this
  variable.

- **file_date**: variable type `character`. Entries example: 07/25/2007.
  There are 1 unique entries in this variable.

- **neighborhood**: variable type `character`. Entries example: Chelsea
  and Clinton, Lower East Side, Lower Manhattan, NA, Gramercy Park and
  Murray Hill, Greenwich Village and Soho. There are 43 unique entries
  in this variable. Additionally, there are 1288 missing entries in this
  variable.

- **borough**: variable type `character`. Entries example: Manhattan,
  Staten Island, Bronx, Queens, Brooklyn. There are 5 unique entries in
  this variable.

``` r
# Compare 'city' to 'borough'
comparison_result <- tidy |>
  group_by(city, borough) |>
  summarise(count = n())

# Head the comparison result
head(comparison_result)
```

    ## # A tibble: 6 × 3
    ## # Groups:   city [6]
    ##   city          borough count
    ##   <chr>         <chr>   <int>
    ## 1 ARVERNE       Queens     56
    ## 2 ASTORIA       Queens    230
    ## 3 AUBURNDALE    Queens      2
    ## 4 BAYSIDE       Queens    135
    ## 5 BAYSIDE HILLS Queens      6
    ## 6 BEECHHURST    Queens      4

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

The 79 observations in the comparison_result table reveal potential data
quality issues, such as:

Across the comparison_result table (79 observations) and the
manhattan_table and queens_table, data quality issues include:

- Mismatched City-to-Borough Relationships:

  - Example: In the comparison_result table, “New York” is associated
    with multiple boroughs, like “Manhattan” and “Brooklyn.” This
    mismatch signals a data quality issue as the same city name is
    linked to different boroughs.

- Data Transformation Problems:

  - Example: In the manhattan_table, “ROOSEVELT ISLAND” appears in
    variations like “ROOSEVELT ISL.” This points to data transformation
    issues, where variations of the same city name lack standardization.

- City Name Variations:

  - Example: In the queens_table, variations in city names, such as
    “QUEENS VILLAGE” and “QUEENS VILLAGE, QUEENS,” indicate inconsistent
    formatting, leading to data quality issues.

- Data Entry Errors:

  - Example: Entries like “BOWLING GREEN” and “CANAL STREET” in the city
    variable suggest data entry errors. These should typically represent
    neighborhoods or locations within a larger city like New York, not
    standalone cities, signifying potential data quality concerns
    related to data entry.

ZIP codes with fewer than 60 observations, often missing neighborhood
values, can be due to:

- **Sparse Data Collection**: ZIP code data may not be collected evenly
  over time. Some ZIP codes might have limited data points due to lower
  population densities, reduced mobility, or specific seasonal
  fluctuations.

- **Data Reporting Patterns**: Data reporting may not occur consistently
  over months or years. Certain areas or organizations might not
  contribute data regularly, leading to gaps in specific ZIP codes.

- **Demographic Variations**: ZIP codes with fewer observations could
  correspond to sparsely populated or less urban areas, where changes of
  address occur less frequently, resulting in fewer entries in the
  dataset.

- **Data Source Limitations**: The dataset’s source or provider may not
  have comprehensive coverage of all ZIP codes, particularly in less
  populated or remote regions.

- **Data Entry or Recording Issues**: Inconsistent or incomplete data
  entry could lead to missing neighborhood values or limited
  observations. Data errors, omissions, or delays can contribute to
  these issues.As mentioned above, it is likely that some neighborhood
  values are mistakenly entered into the `city` variable.

Addressing this may involve considering data sources, local context, and
statistical methods for imputing missing values.

## EDA and Visualization
