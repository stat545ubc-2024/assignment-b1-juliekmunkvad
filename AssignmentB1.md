Assignment_B1
================

Initially, the required packages are installed. The function uses dplyr,
and I will provide examples using the *penguins* data set from
`palmerpenguins` package. The tests for the functions are developed
using the `testthat` package.

``` r
library(palmerpenguins)
library(dplyr)
library(testthat)
```

### Creation of a function calculating summary statistics

In both Milestone 1 and 2 of my MDA, I frequently used summary
statistics to explore the different variables in my dataset. Therefore,
I want to create a function that computes the minimum, maximum, range,
mean, median, standard deviation, and count of a numerical variable
across the groups of a categorical variable in a dataset.

``` r
#' Summary statistics of a numerical variable across the groups of a categorical variable
#'
#' This function calculates summary statistics (min, max, range, mean, meadian, standard deviation and count) of one numerical variable across the groups of one categorical variable from a data set.
#'
#' @param data A data frame containing the variables of interest. 
#' It is named 'data' as it represents the data set that the function operates on.
#' @param cat_var A categorical variable within the data set. 
#' It is called 'cat_var' to clearly indicate that this argument should represent a factor or categorical variable.
#' @param num_var A numeric variable within the data set. 
#' It is named 'num_var' to indicate that this argument should represent a numeric variable.
#' @param na_handling Logical; can be either TRUE or FALSE. 
#' If TRUE, NA values are removed from the calculations; if FALSE, NA values are included.
#' It is named 'na_handling' to clearly specify its role in handling missing values.
#
#' @return A data frame with summary statistics (min, max, range, mean, median, standard deviation, count) for the numeric variable, grouped by the categorical variable.
#'
#' 
#' @examples
#' # Examples with the penguins data set from palmerpenguins package
#' summary_stats_variable(penguins,species,body_mass_g, na_handling=TRUE)
#' summary_stats_variable(penguins, island, flipper_length_mm, na_handling = TRUE)
#'
summary_stats_variable <- function(data, cat_var, num_var, na_handling = TRUE) {
  #Check if cat_var is a factor. If not, stop the execution of the function and execute an error
  if (!is.factor(data %>% pull({{cat_var}}))) {
    stop('Error: The provided cat_var is not a categorical variable. Please provide a factor variable.')
  }
  
  #Check if num_var is numeric. If not, stop the execution of the function and execute an error
  if (!is.numeric(data %>% pull({{num_var}}))) {
    stop('Error: The provided num_var is not numeric. Please provide a numeric variable.')
  }

  #Calculate summary statistics for the numerical variable by groups of the categorical variable
  data %>%
    group_by({{cat_var}}) %>%
    summarise(
      min = min({{num_var}}, na.rm = na_handling),
      max = max({{num_var}}, na.rm = na_handling),
      range = max({{num_var}}, na.rm = na_handling) - min({{num_var}}, na.rm = na_handling),
      mean = mean({{num_var}}, na.rm = na_handling),
      median = median({{num_var}}, na.rm = na_handling),
      sd = sd({{num_var}}, na.rm = na_handling),
      count = n()
    )
}
```

### Examples of function

Next, a series of examples are conducted on the created function
summary_stats_variable using the penguins dataset. First, the dataset
structure is displayed using glimpse(). Then, summary statistics are
calculated for different combinations of numerical and categorical
variables.

Examples 1 and 2 demonstrate valid calculations where the numerical
variable is grouped by a categorical variable. In Examples 3 and 4,
errors are shown when incorrect types of variables are provided as
inputs to the function.

``` r
glimpse(penguins)
```

    ## Rows: 344
    ## Columns: 8
    ## $ species           <fct> Adelie, Adelie, Adelie, Adelie, Adelie, Adelie, Adel…
    ## $ island            <fct> Torgersen, Torgersen, Torgersen, Torgersen, Torgerse…
    ## $ bill_length_mm    <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, …
    ## $ bill_depth_mm     <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18.1, …
    ## $ flipper_length_mm <int> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190, 186…
    ## $ body_mass_g       <int> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 3475, …
    ## $ sex               <fct> male, female, female, NA, female, male, female, male…
    ## $ year              <int> 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007…

``` r
#Example 1: Calculate summary statistics for 'body_mass_g' grouped by 'species'
#This is a valid example with 'species' as the categorical variable and 'body_mass_g' as the numeric variable
summary_stats_variable(penguins, species, body_mass_g, na_handling = TRUE)
```

    ## # A tibble: 3 × 8
    ##   species     min   max range  mean median    sd count
    ##   <fct>     <int> <int> <int> <dbl>  <dbl> <dbl> <int>
    ## 1 Adelie     2850  4775  1925 3701.   3700  459.   152
    ## 2 Chinstrap  2700  4800  2100 3733.   3700  384.    68
    ## 3 Gentoo     3950  6300  2350 5076.   5000  504.   124

``` r
#Example 2: Calculate summary statistics for 'flipper_length_mm' grouped by 'island'
#This is another valid example with 'island' as the categorical variable and 'flipper_length_mm' as the numeric variable
summary_stats_variable(penguins, island, flipper_length_mm, na_handling = TRUE)
```

    ## # A tibble: 3 × 8
    ##   island      min   max range  mean median    sd count
    ##   <fct>     <int> <int> <int> <dbl>  <dbl> <dbl> <int>
    ## 1 Biscoe      172   231    59  210.    214 14.1    168
    ## 2 Dream       178   212    34  193.    193  7.51   124
    ## 3 Torgersen   176   210    34  191.    191  6.23    52

``` r
#Example 3: Attempt to use a numeric variable for 'cat_var'
#This example will generate an error because 'body_mass_g' is numeric, not categorical
summary_stats_variable(penguins, body_mass_g, flipper_length_mm, na_handling = TRUE)
```

    ## Error in summary_stats_variable(penguins, body_mass_g, flipper_length_mm, : Error: The provided cat_var is not a categorical variable. Please provide a factor variable.

``` r
#Example 4: Attempt to use a categorical variable for 'num_var'
#This example will generate an error because 'species' is categorical, not numeric
summary_stats_variable(penguins, species, island, na_handling = TRUE)
```

    ## Error in summary_stats_variable(penguins, species, island, na_handling = TRUE): Error: The provided num_var is not numeric. Please provide a numeric variable.

### Test of function

Finally, I conduct formal tests of the `summary_stats_variable` function
using the `testthat` package. The function is tested under various
conditions to ensure that it works as expected.

The first three tests verify the function’s behavior when handling
variables with no missing values, as well as missing values in the
numerical and categorical variables, respectively. The last two tests
assess the function’s error-handling by attempting to use incorrect
variable types for the categorical and numerical inputs. These tests
confirm that the function is robust and capable of managing different
scenarios effectively.

``` r
#Test 1: Variables with no NAs
test_that("summary_stats_variable works with vectors without NAs", {
  result <- summary_stats_variable(penguins, species, body_mass_g, na_handling = TRUE)
  #Expect that the result is a data frame
  expect_s3_class(result, "data.frame")
  
  #Expect the number of rows to be equal to the number of unique categories in species
  expect_equal(nrow(result), length(unique(penguins$species)))
  
  #Expect that the columns contain the expected summary statistics
  expect_true(all(c("min", "max", "range", "mean", "median", "sd", "count") %in% colnames(result)))
})
```

    ## Test passed 🥇

``` r
#Test 2: Numeric variable with NAs
test_that("summary_stats_variable handles NAs correctly", {
  #Introduce some NAs into the numerical variable
  penguins_na <- penguins
  penguins_na$body_mass_g[1:5] <- NA
  
  result <- summary_stats_variable(penguins_na, species, body_mass_g, na_handling = TRUE)
  
  #Expect that the result is still a data frame
  expect_s3_class(result, "data.frame")
  
  #Expect that the number of rows is equal to the number of unique categories in species
  expect_equal(nrow(result), length(unique(penguins$species)))
  
  #Ensure that no errors occur and the result is valid
  expect_true(all(!is.na(result$mean)))
})
```

    ## Test passed 🥳

``` r
#Test 3: Categorical variable with NAs
test_that("summary_stats_variable handles NAs in cat_var correctly", {
  # Introduce some NAs into the categorical variable
  penguins_na <- penguins
  penguins_na$species[1:5] <- NA
  
  result <- summary_stats_variable(penguins_na, species, body_mass_g, na_handling = TRUE)
  
  #Expect that the result is still a data frame
  expect_s3_class(result, "data.frame")
  
  #Expect that the number of rows is equal to the number of unique categories in species + 1 for NA
  expect_equal(nrow(result), length(unique(penguins$species)) + 1)
  
  #Check if there is a row where 'species' is NA
  expect_true(any(is.na(result$species)))
  
  #Ensure that the mean is calculated correctly for non-NA rows
  non_na_means <- result$mean[!is.na(result$species)]
  expect_true(all(!is.na(non_na_means)))
})
```

    ## Test passed 🎊

``` r
#Test 4: Attempt to use a numeric variable for 'cat_var'
test_that("summary_stats_variable throws error when numeric is used for cat_var", {
  expect_error(
    summary_stats_variable(penguins, body_mass_g, flipper_length_mm, na_handling = TRUE),
    "Error: The provided cat_var is not a categorical variable"
  )
})
```

    ## Test passed 😀

``` r
#Test 5: Attempt to use a categorical variable for 'num_var'
test_that("summary_stats_variable throws error when categorical is used for num_var", {
  expect_error(
    summary_stats_variable(penguins, species, island, na_handling = TRUE),
    "Error: The provided num_var is not numeric"
  )
})
```

    ## Test passed 😀
