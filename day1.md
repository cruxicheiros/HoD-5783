Hanukkah of Data - Day 1
================
Clark Seanor

## The question

The objective of today’s challenge is to find a person in a CSV
containing names and phone numbers whose surname is the same as their
phone number when spelled using a letters-to-numbers phone keypad.

## My approach

### Opening up the data

First, I want to have a quick look at the data.

``` r
library(readr)
library(dplyr)

customers = read_csv("data/noahs-customers.csv")
glimpse(customers)
```

    Rows: 11,080
    Columns: 6
    $ customerid   <dbl> 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 101~
    $ name         <chr> "Jack Quinn", "David Powell", "Carrie Green", "Steven Mil~
    $ address      <chr> "201 E Park St", "224C Tysens Ln", "1608 W 53rd Way", "17~
    $ citystatezip <chr> "Los Angeles, CA 91343", "Staten Island, NY 10306", "Tamp~
    $ birthdate    <date> 1960-05-14, 1978-04-04, 1969-01-21, 1953-08-17, 1983-06-~
    $ phone        <chr> "805-287-8515", "516-768-1652", "727-209-0470", "607-941-~

The columns of note for me are `name` and `phone`. Names and phone
numbers are types of data that programmers infamously believe
[falsehoods](https://www.kalzumeus.com/2010/06/17/falsehoods-programmers-believe-about-names/)
[about](https://github.com/google/libphonenumber/blob/master/FALSEHOODS.md),
so I’m being a bit more thorough than I could be. So, first I want to
check whether I can expect all of the phone numbers to be 10 numbers
long (12, with formatting dashes). It looks like I can from taking a
quick visual look, but I want to check:

``` r
library(stringr)
max(str_length(customers$phone))
```

    [1] 12

Fortunately it looks like I can rely on that. So, considering all phone
numbers are only 10 numbers long, I can exclude any entry where someone
has a surname that is longer or shorter than 10 characters. However, a
person’s surname is not always the last word in their name: sometimes
their name ends in a suffix, like ‘Jr.’ or ‘III’.

``` r
non_surnames <- c('Jr.', 'Sr.', 'I', 'II', "III", 'IV', 'V', 'VI', 'VII', 'VIII')

find_surname_str <- function(n) {
  split_name <- str_split(n, " ")
  likely_surname <- sapply(split_name, tail, 1)
  alternative_surname <- sapply(split_name, function(n) {head(tail(n, 2), 1)})
  non_surname <- likely_surname %in% non_surnames
  
  return(if(non_surname) alternative_surname else likely_surname)
}

find_surname <- function(full_name) {
  return(sapply(full_name, find_surname_str))
}

customers <- customers %>%
 select(name, phone) %>%
 mutate(
  surname = find_surname(name),
  surname_length = str_length(surname),
  phone = str_replace_all(phone, "-", "")
 )

possible_investigators <- customers[customers$surname_length == 10,]

pi_surnames <- unique(possible_investigators$surname)
```

Turns out there are only a few surnames that fit the bill. So let’s
generate some phone numbers:

``` r
library(mgsub)
name_to_number = function(surnames) {
  return(mgsub(toupper(surnames), c('[ABC]', '[DEF]', '[GHI]', '[JKL]', '[MNO]', '[PQRS]', '[TUV]', '[WXYZ]'), c(2, 3, 4, 5, 6, 7, 8, 9)))
}

pi_numbers = name_to_number(pi_surnames) 
pi_numbers
```

     [1] "3489437253" "6252844546" "6228556844" "4277464866" "7424273766"
     [6] "2866464426" "9455426766" "5484647866" "6668466379" "7837436766"
    [11] "4882446766" "7874255263" "8455277325" "9274464866" "7366464866"
    [16] "4888362374" "8253698352" "8455268382"

Now to find the person who has one of those numbers…

``` r
possible_investigators[possible_investigators$phone %in% pi_numbers,]
```

    # A tibble: 1 x 4
      name           phone      surname    surname_length
      <chr>          <chr>      <chr>               <int>
    1 Sam Guttenberg 4888362374 Guttenberg             10

Complete :)
