# Data Mashups in R

My version of the code from the Data Mashups in R book, which is sorely out-of-date.

After painstakingly working through the foreclosure mapping example and pouring over the [errata](http://oreilly.com/catalog/errata.csp?isbn=9780596804787), I decided to share my solution. The book requires using the Yahoo geocoding API, which is now a paid service. The errata acknowledges the API change but still requires you to use the paid XML service. I switched to the [Google geocoding API](https://developers.google.com/maps/documentation/geocoding/) with JSON.

## Running it

To run this code, launch R then source the file in.

```R
source("mapping-foreclosures.r")
```

You might need to `setwd(".")`

## Legal

Code adapted from [Data Mashups in R](http://shop.oreilly.com/product/9780596804787.do).
