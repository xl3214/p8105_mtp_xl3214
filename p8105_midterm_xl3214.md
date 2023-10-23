p8105_midterm_xl3214
================
Xuan Lu
2023-10-19

## Raw Data Overview and Report Goal

The raw USPS Change of Address NYC dataset has 5 variables and 11845
observations. The raw Zip Codes dataset has 7 variables and 324
observations.

``` r
file_path <- "USPS CHANGE OF ADDRESS NYC.xlsx"
sheet_names <- excel_sheets(file_path)

data_frames <- lapply(sheet_names, function(sheet_name) {
  read_excel(file_path, sheet = sheet_name)
})

coa <- bind_rows(data_frames) |>
  clean_names() |>
  rename(zip_code = zipcode) |>
  mutate(year = year(month)) |>
  mutate(net_change = total_perm_in - total_perm_out)

zip <- read.csv("Zip Codes.csv") |> 
  janitor::clean_names() |> 
  mutate(borough = county_name)

test1 <- anti_join(coa, zip, by = "zip_code") # 0 observations of 7 variables
test2 <- left_join(coa, zip, by = "zip_code") # 12085 observations of 14 variables
test3 <- right_join(coa, zip, by = "zip_code") # 12168 observations of 14 variables
test4 <- inner_join(coa, zip, by = "zip_code") # 12085 observations of 14 variables
test5 <- full_join(coa, zip, by = "zip_code") # 12168 observations of 14 variables
# Proceeding with left_join because we only want those with a record on "USPS Change of Address NYC" dataset to be included in the final dataset. Comparing the observations of left_join with inner_join, we can also notice that all records on coa dataset is present in zip dataset.
rm(test1, test2, test3, test4, test5)

tidy <- left_join(coa, zip, by = "zip_code") # 12085 observations of 14 variables
# Warning: Detected an unexpected many-to-many relationship between `x` and `y`.
# Need to go back and edit the codes to keep a one-to-many relationship between coa and zip. 

# Create a data frame indicating duplicated combinations of month and zip_code
coa_duplicated <- coa |>
  group_by(month, zip_code) |>
  filter(duplicated(month, zip_code))
print(coa_duplicated)
# No duplicated combinations of month and zip_code in coa dataset.

# Create a data frame indicating duplicated combinations of zip_code and neighborhood
zip_duplicated <- zip |>
  group_by(zip_code, neighborhood) |>
  filter(duplicated(zip_code, neighborhood))
print(zip_duplicated)
# Four duplicated combinations of zip_code and neighborhood in coa dataset: 10463, 11201, 11239, 11693
zip |> filter(zip_code %in% c(10463, 11201, 11239, 11693))
# Seems like the four zip codes have been categorized into more than one county. Check the unique county names in zip dataset.
unique(pull(zip, county_name))
# Seems like "New York" is of Borough "Manhattan", "Kings" is of Borough "Brooklyn", and "Richmond" is of Borough "Staten Island".
zip <- zip |> 
  mutate(borough = ifelse(county_name == "Kings", "Brooklyn",
                     ifelse(county_name == "Richmond", "Staten Island",
                            ifelse(county_name == "New York", "Manhattan", borough))))
unique(pull(zip, borough))

# Fixing duplicates of zip codes 10463, 11201, 11239, and 11693
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
# Delete duplicates
zip <- zip |>
  distinct(zip_code, .keep_all = TRUE)
# Checking for one last time
zip |> filter(zip_code %in% c(10463, 11201, 11239, 11693))
# No more duplicates. Now there should not be many-to-many relationships when merging.

tidy <- left_join(coa, zip, by = "zip_code")

# Since the coa dataset should only be consisted of NYC residents, and all of its zip codes are found in the zip dataset, the "city" variable exists for no reason. However, I will first check if there are NAs in the neighborhood information from zip. 
zip |> filter(is.na(neighborhood)) # 142 NAs in neighborhood variable in zip dataset
tidy |> filter(is.na(neighborhood)) # 1288 NAs in neighborhood variable in tidy dataset

# I will keep the "city" variable as I suspect neighborhoods might be errorly entered into it. 
summary(tidy)
```

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
