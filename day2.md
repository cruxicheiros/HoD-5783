Hanukkah of Data - Day 2
================
Clark Seanor

## The question

The objective of today’s challenge is to find a person’s phone number
whose first name starts with J and whose surname starts with D, and who
did a job in 2017. The name will be in the customer database, because
the contractors were all also customers. The date of the job can be
found from the orders database, because the order would have occurred at
the same time as the job.

## My approach

### Opening up the data

First, I want to have a quick look at the customers table.

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

Fortunately I wrote some code for processing this table in Day 1. Now
for the orders table:

``` r
orders = read_csv("data/noahs-orders.csv")
glimpse(orders)
```

    Rows: 214,207
    Columns: 6
    $ orderid    <dbl> 1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1010,~
    $ customerid <dbl> 4308, 11683, 5676, 3097, 10374, 9241, 7189, 7228, 1125, 734~
    $ ordered    <dttm> 2017-01-31 00:32:19, 2017-01-31 00:58:31, 2017-01-31 01:34~
    $ shipped    <dttm> 2017-01-31 07:15:00, 2017-01-31 18:00:00, 2017-01-31 09:00~
    $ items      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    $ total      <dbl> 25.52, 35.33, 30.79, 77.60, 109.04, 15.13, 11.02, 33.11, 17~

In this table I’m interested in retrieving `customerid` for orders made
with an `ordered` date in 2017.

To start, I’ll join the tables and extract the customer surnames from
the names.

``` r
library(stringr)
```

    Warning: package 'stringr' was built under R version 4.0.5

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
 select(name, phone, customerid) %>%
 mutate(
  surname = find_surname(name),
 )

customers <- customers[grep("^J", customers$name), 1:ncol(customers)]
customers <- customers[grep("^D", customers$surname), 1:ncol(customers)]
orders <- orders[format(orders$ordered, "%Y") == 2017, 1:ncol(orders)]

possible_contractors = merge(x=customers, y=orders, by="customerid")
glimpse(possible_contractors)
```

    Rows: 223
    Columns: 9
    $ customerid <dbl> 1611, 1611, 1611, 1611, 1611, 1611, 1611, 1611, 1611, 1611,~
    $ name       <chr> "Jacob Davis", "Jacob Davis", "Jacob Davis", "Jacob Davis",~
    $ phone      <chr> "516-521-7244", "516-521-7244", "516-521-7244", "516-521-72~
    $ surname    <chr> "Davis", "Davis", "Davis", "Davis", "Davis", "Davis", "Davi~
    $ orderid    <dbl> 17759, 1341, 19000, 5852, 4674, 19502, 27444, 7270, 22975, ~
    $ ordered    <dttm> 2017-07-19 09:00:05, 2017-02-03 11:34:48, 2017-07-31 20:03~
    $ shipped    <dttm> 2017-07-19 20:45:00, 2017-02-03 18:00:00, 2017-08-02 13:30~
    $ items      <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,~
    $ total      <dbl> 20.70, 29.72, 95.71, 46.69, 3.90, 6.40, 6.15, 101.26, 132.4~

Unfortunately, there are too many possible contractors to just go
through and guess. Fortunately, the order table contains the `orderid`,
the order-products table contains the SKU of the ordered products, and
the products table contains the `desc` of each product. We know that
they would order coffee, bagels, and cleaning supplies.

``` r
products = read_csv("data/noahs-products.csv")
```

    Rows: 1124 Columns: 3
    -- Column specification --------------------------------------------------------
    Delimiter: ","
    chr (2): sku, desc
    dbl (1): wholesale_cost

    i Use `spec()` to retrieve the full column specification for this data.
    i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
products <- products[grep("coffee|bagel", products$desc, ignore.case=TRUE), 1:ncol(products)]

order_products = read_csv("data/noahs-orders_items.csv")
```

    Rows: 427258 Columns: 4
    -- Column specification --------------------------------------------------------
    Delimiter: ","
    chr (1): sku
    dbl (3): orderid, qty, unit_price

    i Use `spec()` to retrieve the full column specification for this data.
    i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
possible_orders = merge(x=products, y=order_products, by="sku")

merge(x=possible_contractors, y=possible_orders, by="orderid")
```

      orderid customerid                name        phone surname
    1    7409       4164        Jeremy Davis 212-771-8924   Davis
    2    7409       4164        Jeremy Davis 212-771-8924   Davis
    3    7409       4164        Jeremy Davis 212-771-8924   Davis
    4   28010       6122 Julie Melissa Duffy 347-716-8024   Duffy
    5   28092       3998         James Dixon 212-277-2382   Dixon
    6   29507       8979         Javier Diaz 838-264-0667    Diaz
                  ordered             shipped items total     sku
    1 2017-04-05 12:49:41 2017-04-05 12:49:41    NA 33.22 BKY5887
    2 2017-04-05 12:49:41 2017-04-05 12:49:41    NA 33.22 DLI1464
    3 2017-04-05 12:49:41 2017-04-05 12:49:41    NA 33.22 KIT5861
    4 2017-10-29 13:40:05 2017-10-29 13:40:05    NA 13.00 KIT5861
    5 2017-10-30 11:21:15 2017-10-30 11:21:15    NA  5.08 KIT1297
    6 2017-11-13 20:06:42 2017-11-13 20:06:42    NA 34.09 KIT0825
                           desc wholesale_cost qty unit_price
    1              Sesame Bagel           6.38   1       8.48
    2              Coffee, Drip           6.21   1       7.73
    3   Vintage Coffee Strainer          11.24   1      12.53
    4   Vintage Coffee Strainer          11.24   1      13.00
    5 Mechanical Coffee Spatula           4.31   1       5.08
    6   Handmade Coffee Spatula           9.30   1      12.75

Contextually, the answer can only be Jeremy Davis, because none of the
other names ordered coffee and bagels more than once.
