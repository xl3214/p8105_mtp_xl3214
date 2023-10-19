p8105_midterm_xl3214
================
Xuan Lu
2023-10-19

## Raw Data Overview and Report Goal

We are looking at the USPS Change of Address NYC dataset. The raw
dataset has 5 variables and 2383 observations. Variables include:

- **MONTH**: variable type `POSIXct, POSIXt`, range 2018-01-01 to
  2018-12-01, mean = 2018-06-15 22:06:59.974822.

- **ZIPCODE**: variable type `numeric`, range 1.0001^{4} to 1.1697^{4},
  mean = 1.0777936^{4}.

- **CITY**: variable type `character`, there are 64 distinct entries for
  this variable.

- **TOTAL PERM OUT**: variable type `numeric`, range 0 to 1256, mean =
  265.873269.

- **TOTAL PERM IN**: variable type `numeric`, range 0 to 1117, mean =
  229.2631137.

Import, tidy, combine, and otherwise clean the data. In the ZIP code
data, create a borough variable using county names. When importing COA
data, add a year variable for later use; also, create a net_change
variable by subtracting outbound COAs from inbound COAs. Resolve any
issues that arise when merging COA and ZIP code data. Restrict your
dataset to only variables necessary for later parts of this report.
Describe the major steps in the data wrangling process in words,
including what steps you took to address data quality issues.
