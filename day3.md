Hanukkah of Data - Day 3
================
Clark Seanor

## The question

The objective of today’s challenge is to find a person’s phone number
who was born in the year of the dog and is an Aries.

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

I’m not totally sure why the birthdate is stored here, but whatever?

Anyway, because the date comparison library I wanted to use doesn’t seem
to work properly on my computer because it can’t talk to the time
management system or whatever ***(I blame Crouton)*** I’m just going to
split out the date parts I want to be able to use. Aries are born
between March 21 and April 20, according to Google. The year of the dog
occurs every 12 years, when `year % 12 = 2`. So I need to be able to use
all these date parts, and I need to be able to use them as ints.

``` r
customers <- customers %>%
 select(name, phone, birthdate, customerid) %>%
 mutate(
  year = as.integer(format(customers$birthdate, "%Y")),
  month = as.integer(format(customers$birthdate, "%m")),
  day = as.integer(format(customers$birthdate, "%d")),
 )

customers <- customers[customers$year %% 12 == 2,]

customers <- customers[(customers$month == 3 & customers$day > 20) | (customers$month == 4 & customers$day < 21),]

glimpse(customers)
```

    Rows: 83
    Columns: 7
    $ name       <chr> "Eric Brown", "April Reynolds", "Jeffrey White", "Jay Vasqu~
    $ phone      <chr> "838-862-8138", "531-313-8952", "521-945-0864", "631-825-83~
    $ birthdate  <date> 1982-04-16, 1958-04-15, 1994-04-12, 1970-04-15, 1958-03-24~
    $ customerid <dbl> 1078, 1152, 1184, 1271, 1340, 1624, 1648, 1883, 1933, 2008,~
    $ year       <int> 1982, 1958, 1994, 1970, 1958, 1970, 1970, 1994, 1970, 1982,~
    $ month      <int> 4, 4, 4, 4, 3, 4, 4, 4, 4, 4, 4, 3, 4, 4, 4, 3, 3, 4, 3, 4,~
    $ day        <int> 16, 15, 12, 15, 24, 12, 3, 20, 14, 17, 19, 25, 8, 5, 1, 25,~

So it turns out that there are a lot of Aries born in the year of the
dog. Okay. So we also know that the person probably visited Noah’s in
2017.

``` r
orders = read_csv("data/noahs-orders.csv")
orders <- orders[format(orders$ordered, "%Y") == 2017,]
customers <- merge(x=customers, y=orders, by="customerid")

unique(customers$name)
```

     [1] "Eric Brown"              "Jay Vasquez Jr."        
     [3] "Brent Nguyen"            "David Chang"            
     [5] "Ryan Wilson"             "Cynthia Barton"         
     [7] "James Alvarez"           "Brandon Suarez Jr."     
     [9] "Morgan Lopez"            "Amanda Benton"          
    [11] "Denise Mcdonald"         "Cory Reeves"            
    [13] "Megan Cain"              "Elizabeth Megan Frazier"
    [15] "Veronica Anderson"       "Anthony Li"             
    [17] "Madison Turner"          "Christian Lara"         
    [19] "Edwin Aguirre"           "Brenda Kane"            
    [21] "Margaret Torres"         "Carlos Rice"            
    [23] "Denise Villa"            "Shannon Davis"          
    [25] "William Frazier"         "Tyler Newman"           
    [27] "Jacob Smith"             "Gregory Richardson"     
    [29] "Bethany Watson"          "Brian Williams"         

Well. There are still far too many of them. So maybe one of them bought
rug cleaner (SKU HOM8601)?

``` r
orders_items = read_csv("data/noahs-orders_items.csv")
orders_items <- orders_items[orders_items$sku == "HOM8601",]
customers <- merge(x=customers, y=orders_items, by="orderid")

customers
```

      orderid customerid           name        phone  birthdate year month day
    1   18172       8753    Carlos Rice 631-995-7218 1994-04-04 1994     4   4
    2   20129       2274   Brent Nguyen 516-636-7397 1958-03-25 1958     3  25
    3   27309       3175 Cynthia Barton 838-790-1735 1946-03-26 1946     3  26
                  ordered             shipped items total     sku qty unit_price
    1 2017-07-23 10:43:08 2017-07-23 10:43:08    NA 22.69 HOM8601   1       4.26
    2 2017-08-12 16:18:23 2017-08-12 16:18:23    NA  8.24 HOM8601   2       4.12
    3 2017-10-22 11:01:48 2017-10-22 14:00:00    NA 31.01 HOM8601   1       4.72

Well… since it’s a guy it’s probably either going to be Carlos or Brent.
Since Brent bought the most rug cleaner and the rug was really dirty, I
decided to enter Brent’s phone number first. It was Brent :)
