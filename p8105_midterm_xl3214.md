p8105_midterm_xl3214
================
Xuan Lu
2023-10-19

## Raw Data Overview and Report Goal

The raw USPS Change of Address NYC dataset has 5 variables and 2383
observations. The raw Zip Codes dataset has 7 variables and 324
observations.

``` r
coa <- readxl::read_excel("USPS\ CHANGE\ OF\ ADDRESS\ NYC.xlsx") |>
  janitor::clean_names() |>
  rename(zip_code = zipcode) |>
  mutate(year = lubridate::year(month)) |>
  mutate(net_change = total_perm_in - total_perm_out) 
# pivot_wider(names_from = month, values_from = c(net_change, total_perm_in, total_perm_out))
zip <- read.csv("Zip Codes.csv") |> 
  janitor::clean_names() |> 
  mutate(borough = county_name) |>
  filter(!(zip_code == 10463 & county_name == "New York"))
tidy <- merge(coa, zip, by = "zip_code", all.x = TRUE)


summary(tidy)
```

    ##     zip_code         month                            city          
    ##  Min.   :10001   Min.   :2018-01-01 00:00:00.00   Length:2419       
    ##  1st Qu.:10159   1st Qu.:2018-03-01 00:00:00.00   Class :character  
    ##  Median :11104   Median :2018-06-01 00:00:00.00   Mode  :character  
    ##  Mean   :10787   Mean   :2018-06-15 22:19:23.78                     
    ##  3rd Qu.:11358   3rd Qu.:2018-09-01 00:00:00.00                     
    ##  Max.   :11697   Max.   :2018-12-01 00:00:00.00                     
    ##  total_perm_out   total_perm_in         year        net_change     
    ##  Min.   :   0.0   Min.   :   0.0   Min.   :2018   Min.   :-404.00  
    ##  1st Qu.: 114.0   1st Qu.: 101.0   1st Qu.:2018   1st Qu.: -65.00  
    ##  Median : 235.0   Median : 205.0   Median :2018   Median : -27.00  
    ##  Mean   : 266.0   Mean   : 230.3   Mean   :2018   Mean   : -35.65  
    ##  3rd Qu.: 377.5   3rd Qu.: 319.0   3rd Qu.:2018   3rd Qu.:   0.00  
    ##  Max.   :1256.0   Max.   :1117.0   Max.   :2018   Max.   : 360.00  
    ##  county_name          state_fips  county_code     county_fips   
    ##  Length:2419        Min.   :36   Min.   : 5.00   Min.   :36005  
    ##  Class :character   1st Qu.:36   1st Qu.:47.00   1st Qu.:36047  
    ##  Mode  :character   Median :36   Median :61.00   Median :36061  
    ##                     Mean   :36   Mean   :59.09   Mean   :36059  
    ##                     3rd Qu.:36   3rd Qu.:81.00   3rd Qu.:36081  
    ##                     Max.   :36   Max.   :85.00   Max.   :36085  
    ##   file_date         neighborhood         borough         
    ##  Length:2419        Length:2419        Length:2419       
    ##  Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character  
    ##                                                          
    ##                                                          
    ## 

Import, tidy, combine, and otherwise clean the data. In the ZIP code
data, create a borough variable using county names. Resolve any issues
that arise when merging COA and ZIP code data. Restrict your dataset to
only variables necessary for later parts of this report. Describe the
major steps in the data wrangling process in words, including what steps
you took to address data quality issues.
