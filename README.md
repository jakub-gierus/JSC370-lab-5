Lab 05 - Data Wrangling
================

# Learning goals

- Use the `merge()` function to join two datasets.
- Deal with missings and impute data.
- Identify relevant observations using `quantile()`.
- Practice your GitHub skills.

# Lab description

For this lab we will be dealing with the meteorological dataset `met`.
In this case, we will use `data.table` to answer some questions
regarding the `met` dataset, while at the same time practice your
Git+GitHub skills for this project.

This markdown document should be rendered using `github_document`
document.

# Part 1: Setup a Git project and the GitHub repository

1.  Go to wherever you are planning to store the data on your computer,
    and create a folder for this project

2.  In that folder, save [this
    template](https://github.com/JSC370/JSC370-2024/blob/main/labs/lab05/lab05-wrangling-gam.Rmd)
    as “README.Rmd”. This will be the markdown file where all the magic
    will happen.

3.  Go to your GitHub account and create a new repository of the same
    name that your local folder has, e.g., “JSC370-labs”.

4.  Initialize the Git project, add the “README.Rmd” file, and make your
    first commit.

5.  Add the repo you just created on GitHub.com to the list of remotes,
    and push your commit to origin while setting the upstream.

Most of the steps can be done using command line:

``` sh
# Step 1
cd ~/Documents
mkdir JSC370-labs
cd JSC370-labs

# Step 2
wget https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd
mv lab05-wrangling-gam.Rmd README.Rmd
# if wget is not available,
curl https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd --output README.Rmd

# Step 3
# Happens on github

# Step 4
git init
git add README.Rmd
git commit -m "First commit"

# Step 5
git remote add origin git@github.com:[username]/JSC370-labs
git push -u origin master
```

You can also complete the steps in R (replace with your paths/username
when needed)

``` r
# Step 1
setwd("~/Documents")
dir.create("JSC370-labs")
setwd("JSC370-labs")

# Step 2
download.file(
  "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/labs/lab05/lab05-wrangling-gam.Rmd",
  destfile = "README.Rmd"
  )

# Step 3: Happens on Github

# Step 4
system("git init && git add README.Rmd")
system('git commit -m "First commit"')

# Step 5
system("git remote add origin git@github.com:[username]/JSC370-labs")
system("git push -u origin master")
```

Once you are done setting up the project, you can now start working with
the MET data.

## Setup in R

1.  Load the `data.table` (and the `dtplyr` and `dplyr` packages),
    `mgcv`, `ggplot2`, `leaflet`, `kableExtra`.

``` r
library(data.table)
library(dtplyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:data.table':
    ## 
    ##     between, first, last

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.9-0. For overview type 'help("mgcv-package")'.

``` r
library(ggplot2)
library(leaflet)
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
fn <- "https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz"
if (!file.exists("met_all_2023.gz"))
  download.file(fn, destfile = "met_all_2023.gz")
met <- data.table::fread("met_all_2023.gz")
```

2.  Load the met data from
    <https://github.com/JSC370/JSC370-2024/main/data/met_all_2023.gz> or
    (Use
    <https://raw.githubusercontent.com/JSC370/JSC370-2024/main/data/met_all_2023.gz>
    to download programmatically), and also the station data. For the
    latter, you can use the code we used during lecture to pre-process
    the stations data:

``` r
# Download the data
stations <- fread("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
stations[, USAF := as.integer(USAF)]
```

    ## Warning in eval(jsub, SDenv, parent.frame()): NAs introduced by coercion

``` r
# Dealing with NAs and 999999
stations[, USAF   := fifelse(USAF == 999999, NA_integer_, USAF)]
stations[, CTRY   := fifelse(CTRY == "", NA_character_, CTRY)]
stations[, STATE  := fifelse(STATE == "", NA_character_, STATE)]

# Selecting the three relevant columns, and keeping unique records
stations <- unique(stations[, list(USAF, CTRY, STATE, LAT, LON)])

# Dropping NAs
stations <- stations[!is.na(USAF)]

# Removing duplicates
stations[, n := 1:.N, by = .(USAF)]
stations <- stations[n == 1,][, n := NULL]

# Read in the met data and fix lat, lon, temp
met[, `:=`(
  lat = lat / 1000,
  lon = lon / 1000,
  wind.sp = wind.sp / 10,
  temp = temp / 10,
  dew.point = dew.point / 10,
  atm.press = atm.press / 10
)]
```

3.  Merge the data as we did during the lecture. Use the `merge()` code
    and you can also try the tidy way with `left_join()`

``` r
met_dt <- merge(
# Data
 x = met,
 y = stations,
# List of variables to match
 by.x = "USAFID",
 by.y = "USAF",
# Which obs to keep?
 all.x = TRUE,
 all.y = FALSE
 )
```

## Question 1: Identifying Representative Stations

Across all weather stations, which stations have the median values of
temperature, wind speed, and atmospheric pressure? Using the
`quantile()` function, identify these three stations. Do they coincide?

``` r
median_temp <- median(met_dt$temp, na.rm = TRUE)
median_wind_sp <- median(met_dt$wind.sp, na.rm = TRUE)
median_atm_press <- median(met_dt$atm.press, na.rm = TRUE)

closest_to_median_temp <- met_dt %>%
  filter(!is.na(temp)) %>%
  arrange(abs(temp - median_temp)) %>%
  slice(1)

closest_to_median_wind_sp <- met_dt %>%
  filter(!is.na(wind.sp)) %>%
  arrange(abs(wind.sp - median_wind_sp)) %>%
  slice(1)

closest_to_median_atm_press <- met_dt %>%
  filter(!is.na(atm.press)) %>%
  arrange(abs(atm.press - median_atm_press)) %>%
  slice(1)
```

Next identify the stations have these median values.

``` r
print(closest_to_median_temp)
```

    ##    USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ## 1: 690150 93121 2023     6   1    6  56 34.294 -116.147  696      300
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ## 1:           5              N     6.7          5      22000             5
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp
    ## 1:                 9        N    16093           5       N          5 21.7
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc CTRY STATE    LAT
    ## 1:       5       6.7            5    1006.6            5   US    CA 34.294
    ##         LON
    ## 1: -116.147

``` r
print(closest_to_median_wind_sp)
```

    ##    USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ## 1: 690150 93121 2023     6   1    5  56 34.294 -116.147  696       20
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ## 1:           5              N     3.1          5      22000             5
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp
    ## 1:                 9        N    16093           5       N          5 22.2
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc CTRY STATE    LAT
    ## 1:       5       6.7            5    1006.5            5   US    CA 34.294
    ##         LON
    ## 1: -116.147

``` r
print(closest_to_median_atm_press)
```

    ##    USAFID  WBAN year month day hour min    lat      lon elev wind.dir
    ## 1: 690150 93121 2023     6   2   17  56 34.294 -116.147  696      110
    ##    wind.dir.qc wind.type.code wind.sp wind.sp.qc ceiling.ht ceiling.ht.qc
    ## 1:           5              N     2.6          5      22000             5
    ##    ceiling.ht.method sky.cond vis.dist vis.dist.qc vis.var vis.var.qc temp
    ## 1:                 9        N    16093           5       N          5 27.8
    ##    temp.qc dew.point dew.point.qc atm.press atm.press.qc CTRY STATE    LAT
    ## 1:       5       3.9            5    1011.7            5   US    CA 34.294
    ##         LON
    ## 1: -116.147

The stations are all the same.

Knit the document, commit your changes, and save it on GitHub. Don’t
forget to add `README.md` to the tree, the first time you render it.

## Question 2: Identifying Representative Stations per State

Now let’s find the weather stations by state with closest temperature
and wind speed based on the euclidean distance from these medians.

``` r
median_temp <- median(met_dt$temp, na.rm = TRUE)
median_wind_sp <- median(met_dt$wind.sp, na.rm = TRUE)


met_dt <- met_dt %>%
  mutate(euclidean_distance = sqrt((temp - median_temp)^2 + (wind.sp - median_wind_sp)^2))


closest_stations_by_state <- met_dt %>%
  group_by(STATE) %>%
  filter(euclidean_distance == min(euclidean_distance, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()


print(closest_stations_by_state)
```

    ## # A tibble: 48 × 34
    ##    USAFID  WBAN  year month   day  hour   min   lat    lon  elev wind.dir
    ##     <int> <int> <int> <int> <int> <int> <int> <dbl>  <dbl> <int>    <int>
    ##  1 720307 63804  2023     6    15    13    35  34.9  -86.6   230      150
    ##  2 720175 53919  2023     6     4     2    53  33.6  -91.8    82      310
    ##  3 722720 93026  2023     6    28    10    56  31.5 -110.   1250       90
    ##  4 690150 93121  2023     6    10    10    56  34.3 -116.    625      300
    ##  5 720538   164  2023     6    17    19    35  40.2 -105.   1541      210
    ##  6 725027 54788  2023     6     6    18    53  41.5  -72.8    32      340
    ##  7 724093 13764  2023     6     1    23    54  38.7  -75.4    16       90
    ##  8 720383 53847  2023     6     1     5    56  30.7  -87.0    54       30
    ##  9 720263 63834  2023     6     9    14    15  34.1  -82.8   183       80
    ## 10 720293  4989  2023     6    10    23    15  42.5  -91.9   298       30
    ## # ℹ 38 more rows
    ## # ℹ 23 more variables: wind.dir.qc <chr>, wind.type.code <chr>, wind.sp <dbl>,
    ## #   wind.sp.qc <chr>, ceiling.ht <int>, ceiling.ht.qc <int>,
    ## #   ceiling.ht.method <chr>, sky.cond <chr>, vis.dist <int>, vis.dist.qc <chr>,
    ## #   vis.var <chr>, vis.var.qc <chr>, temp <dbl>, temp.qc <chr>,
    ## #   dew.point <dbl>, dew.point.qc <chr>, atm.press <dbl>, atm.press.qc <int>,
    ## #   CTRY <chr>, STATE <chr>, LAT <dbl>, LON <dbl>, euclidean_distance <dbl>

Knit the doc and save it on GitHub.

## Question 3: In the Geographic Center?

For each state, identify which station is closest to the geographic
mid-point (median) of the state. Combining these with the stations you
identified in the previous question, use `leaflet()` to visualize all
~100 points in the same figure, applying different colors for the
geographic median and the temperature and wind speed median.

``` r
geo_midpoints <- met_dt %>%
  group_by(STATE) %>%
  summarise(median_lat = median(LAT, na.rm = TRUE),
            median_lon = median(LON, na.rm = TRUE)) %>%
  ungroup()


closest_geo_stations <- met_dt %>%
  rowwise() %>%
  mutate(distance = sqrt((LAT - geo_midpoints$median_lat[match(STATE, geo_midpoints$STATE)])^2 + 
                         (LON - geo_midpoints$median_lon[match(STATE, geo_midpoints$STATE)])^2)) %>%
  group_by(STATE) %>%
  filter(distance == min(distance, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()


all_stations <- bind_rows(
  closest_stations_by_state %>% mutate(type = "Temp/Wind Speed"),
  closest_geo_stations %>% mutate(type = "Geographic Median")
)

leaflet(all_stations) %>%
  addTiles() %>%
  addCircleMarkers(~LON, ~LAT, color = ~ifelse(type == "Temp/Wind Speed", "blue", "red"),
                   popup = ~paste(STATE, ":", USAFID, "<br>", type))
```

<div class="leaflet html-widget html-fill-item" id="htmlwidget-39c25129fd010d280611" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-39c25129fd010d280611">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"https://openstreetmap.org/copyright/\">OpenStreetMap<\/a>,  <a href=\"https://opendatacommons.org/licenses/odbl/\">ODbL<\/a>"}]},{"method":"addCircleMarkers","args":[[34.861,33.636,31.458,34.294,40.167,41.51,38.69,30.697,34.095,42.453,43.743,41.425,41.275,39.428,36.855,30.033,42.47,39.167,43.642,42.543,43.987,38.958,32.8,47.133,35.256,46.924,41.196,43.205,40.617,36.422,35.976,40.96,39.531,34.699,42.074,41.033,41.351,33.65,43.578,35.034,30.383,39.609,36.687,44.567,45.619,44.333,37.784,41.158,33.178,35.257,33.466,36.985,39.05,41.384,39.133,28.821,32.633,41.691,43.567,40.483,41.066,38.068,37.578,30.558,42.212,39.173,44.533,43.322,45.544,38.947,32.32,47.517,35.582,48.39,40.893,43.205,40.624,33.45,39.601,41.701,40.28,35.357,44.5,40.218,41.597,33.967,43.767,35.38,31.133,40.219,37.4,44.533,47.445,44.783,39,43.062],[-86.557,-91.756,-109.606,-116.147,-105.167,-72.828,-75.36199999999999,-87.021,-82.816,-91.94799999999999,-111.098,-88.419,-85.84,-101.046,-84.85599999999999,-91.883,-71.289,-77.167,-70.304,-83.178,-95.783,-94.371,-88.833,-104.8,-81.601,-96.812,-96.11199999999999,-71.503,-74.25,-105.29,-115.133,-72.252,-84.395,-99.33799999999999,-124.29,-80.417,-71.806,-81.68300000000001,-96.754,-85.2,-103.683,-110.755,-77.483,-72.017,-121.166,-89.02,-81.123,-104.808,-86.782,-93.095,-111.721,-120.111,-105.516,-72.506,-75.467,-81.81,-83.59999999999999,-93.566,-116.241,-88.95,-86.182,-97.861,-84.77,-92.099,-71.114,-76.684,-69.667,-84.688,-94.05200000000001,-92.68300000000001,-90.078,-111.183,-79.101,-100.024,-97.997,-71.503,-74.669,-105.517,-116.006,-74.795,-83.11499999999999,-96.943,-123.283,-76.855,-71.41200000000001,-80.467,-99.318,-86.246,-97.717,-111.723,-77.517,-72.61499999999999,-122.314,-89.667,-80.274,-108.447],10,null,null,{"interactive":true,"className":"","stroke":true,"color":["blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red"],"weight":5,"opacity":0.5,"fill":true,"fillColor":["blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","blue","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red","red"],"fillOpacity":0.2},null,null,["AL : 720307 <br> Temp/Wind Speed","AR : 720175 <br> Temp/Wind Speed","AZ : 722720 <br> Temp/Wind Speed","CA : 690150 <br> Temp/Wind Speed","CO : 720538 <br> Temp/Wind Speed","CT : 725027 <br> Temp/Wind Speed","DE : 724093 <br> Temp/Wind Speed","FL : 720383 <br> Temp/Wind Speed","GA : 720263 <br> Temp/Wind Speed","IA : 720293 <br> Temp/Wind Speed","ID : 720369 <br> Temp/Wind Speed","IL : 720137 <br> Temp/Wind Speed","IN : 720266 <br> Temp/Wind Speed","KS : 720422 <br> Temp/Wind Speed","KY : 720379 <br> Temp/Wind Speed","LA : 722314 <br> Temp/Wind Speed","MA : 725059 <br> Temp/Wind Speed","MD : 720334 <br> Temp/Wind Speed","ME : 726060 <br> Temp/Wind Speed","MI : 720113 <br> Temp/Wind Speed","MN : 720368 <br> Temp/Wind Speed","MO : 720306 <br> Temp/Wind Speed","MS : 720708 <br> Temp/Wind Speed","MT : 726676 <br> Temp/Wind Speed","NC : 720277 <br> Temp/Wind Speed","ND : 727530 <br> Temp/Wind Speed","NE : 720308 <br> Temp/Wind Speed","NH : 726050 <br> Temp/Wind Speed","NJ : 720581 <br> Temp/Wind Speed","NM : 720411 <br> Temp/Wind Speed","NV : 722096 <br> Temp/Wind Speed","NY : 722098 <br> Temp/Wind Speed","OH : 722249 <br> Temp/Wind Speed","OK : 720354 <br> Temp/Wind Speed","OR : 720365 <br> Temp/Wind Speed","PA : 720378 <br> Temp/Wind Speed","RI : 722151 <br> Temp/Wind Speed","SC : 720601 <br> Temp/Wind Speed","SD : 726510 <br> Temp/Wind Speed","TN : 723240 <br> Temp/Wind Speed","TX : 720151 <br> Temp/Wind Speed","UT : 724700 <br> Temp/Wind Speed","VA : 720278 <br> Temp/Wind Speed","VT : 720492 <br> Temp/Wind Speed","WA : 726988 <br> Temp/Wind Speed","WI : 720343 <br> Temp/Wind Speed","WV : 724120 <br> Temp/Wind Speed","WY : 725640 <br> Temp/Wind Speed","AL : 722300 <br> Geographic Median","AR : 723429 <br> Geographic Median","AZ : 722783 <br> Geographic Median","CA : 745046 <br> Geographic Median","CO : 726396 <br> Geographic Median","CT : 720545 <br> Geographic Median","DE : 724088 <br> Geographic Median","FL : 722213 <br> Geographic Median","GA : 722175 <br> Geographic Median","IA : 725466 <br> Geographic Median","ID : 726810 <br> Geographic Median","IL : 724397 <br> Geographic Median","IN : 720736 <br> Geographic Median","KS : 724506 <br> Geographic Median","KY : 720448 <br> Geographic Median","LA : 720468 <br> Geographic Median","MA : 744907 <br> Geographic Median","MD : 724060 <br> Geographic Median","ME : 726073 <br> Geographic Median","MI : 725405 <br> Geographic Median","MN : 726550 <br> Geographic Median","MO : 720869 <br> Geographic Median","MS : 722350 <br> Geographic Median","MT : 727755 <br> Geographic Median","NC : 722201 <br> Geographic Median","ND : 720867 <br> Geographic Median","NE : 725513 <br> Geographic Median","NH : 726050 <br> Geographic Median","NJ : 722247 <br> Geographic Median","NM : 722683 <br> Geographic Median","NV : 724770 <br> Geographic Median","NY : 725145 <br> Geographic Median","OH : 720928 <br> Geographic Median","OK : 722187 <br> Geographic Median","OR : 726945 <br> Geographic Median","PA : 725118 <br> Geographic Median","RI : 725074 <br> Geographic Median","SC : 747900 <br> Geographic Median","SD : 726530 <br> Geographic Median","TN : 721031 <br> Geographic Median","TX : 722570 <br> Geographic Median","UT : 725724 <br> Geographic Median","VA : 720498 <br> Geographic Median","VT : 726114 <br> Geographic Median","WA : 727930 <br> Geographic Median","WI : 726465 <br> Geographic Median","WV : 720328 <br> Geographic Median","WY : 726720 <br> Geographic Median"],null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[28.821,48.39],"lng":[-124.29,-69.667]}},"evals":[],"jsHooks":[]}</script>

Knit the doc and save it on GitHub.

## Question 4: Summary Table with `kableExtra`

Generate a summary table using `kable` where the rows are each state and
the columns represent average temperature broken down by low, median,
and high elevation stations.

Use the following breakdown for elevation:

- Low: elev \< 93
- Mid: elev \>= 93 and elev \< 401
- High: elev \>= 401

``` r
met_dt <- met_dt %>%
  mutate(elevation_category = case_when(
    elev < 93 ~ "Low",
    elev >= 93 & elev < 401 ~ "Mid",
    elev >= 401 ~ "High",
    TRUE ~ NA_character_ # for missing data
  ))


avg_temp_by_state_elev <- met_dt %>%
  group_by(STATE, elevation_category) %>%
  summarise(average_temp = mean(temp, na.rm = TRUE), .groups = 'drop')


wide_avg_temp <- avg_temp_by_state_elev %>%
  pivot_wider(names_from = elevation_category, values_from = average_temp)


kable(wide_avg_temp, format = "html") %>%
  kable_styling(bootstrap_options = c("striped", "hover"))
```

<table class="table table-striped table-hover" style="margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
STATE
</th>
<th style="text-align:right;">
Low
</th>
<th style="text-align:right;">
Mid
</th>
<th style="text-align:right;">
High
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
AL
</td>
<td style="text-align:right;">
25.07106
</td>
<td style="text-align:right;">
23.79775
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
AR
</td>
<td style="text-align:right;">
25.58698
</td>
<td style="text-align:right;">
24.40578
</td>
<td style="text-align:right;">
23.723926
</td>
</tr>
<tr>
<td style="text-align:left;">
AZ
</td>
<td style="text-align:right;">
29.28585
</td>
<td style="text-align:right;">
30.38057
</td>
<td style="text-align:right;">
23.892609
</td>
</tr>
<tr>
<td style="text-align:left;">
CA
</td>
<td style="text-align:right;">
18.25508
</td>
<td style="text-align:right;">
18.77071
</td>
<td style="text-align:right;">
18.148808
</td>
</tr>
<tr>
<td style="text-align:left;">
CO
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
15.184075
</td>
</tr>
<tr>
<td style="text-align:left;">
CT
</td>
<td style="text-align:right;">
19.37249
</td>
<td style="text-align:right;">
18.78433
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
DE
</td>
<td style="text-align:right;">
21.40611
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
FL
</td>
<td style="text-align:right;">
26.61484
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
GA
</td>
<td style="text-align:right;">
24.80529
</td>
<td style="text-align:right;">
23.23841
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
IA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.26228
</td>
<td style="text-align:right;">
21.992787
</td>
</tr>
<tr>
<td style="text-align:left;">
ID
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
16.415667
</td>
</tr>
<tr>
<td style="text-align:left;">
IL
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.11707
</td>
<td style="text-align:right;">
20.843173
</td>
</tr>
<tr>
<td style="text-align:left;">
IN
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
20.12731
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
KS
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
24.16196
</td>
<td style="text-align:right;">
22.098776
</td>
</tr>
<tr>
<td style="text-align:left;">
KY
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.36103
</td>
<td style="text-align:right;">
20.178196
</td>
</tr>
<tr>
<td style="text-align:left;">
LA
</td>
<td style="text-align:right;">
27.61819
</td>
<td style="text-align:right;">
26.09414
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MA
</td>
<td style="text-align:right;">
17.44477
</td>
<td style="text-align:right;">
17.59058
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MD
</td>
<td style="text-align:right;">
21.25462
</td>
<td style="text-align:right;">
20.62255
</td>
<td style="text-align:right;">
20.648332
</td>
</tr>
<tr>
<td style="text-align:left;">
ME
</td>
<td style="text-align:right;">
15.23159
</td>
<td style="text-align:right;">
15.43930
</td>
<td style="text-align:right;">
15.329681
</td>
</tr>
<tr>
<td style="text-align:left;">
MI
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
18.54432
</td>
<td style="text-align:right;">
17.977982
</td>
</tr>
<tr>
<td style="text-align:left;">
MN
</td>
<td style="text-align:right;">
22.66275
</td>
<td style="text-align:right;">
21.15523
</td>
<td style="text-align:right;">
19.931963
</td>
</tr>
<tr>
<td style="text-align:left;">
MO
</td>
<td style="text-align:right;">
25.79654
</td>
<td style="text-align:right;">
23.77652
</td>
<td style="text-align:right;">
23.300286
</td>
</tr>
<tr>
<td style="text-align:left;">
MS
</td>
<td style="text-align:right;">
26.34285
</td>
<td style="text-align:right;">
24.66682
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
MT
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
16.293015
</td>
</tr>
<tr>
<td style="text-align:left;">
NC
</td>
<td style="text-align:right;">
22.82945
</td>
<td style="text-align:right;">
21.21073
</td>
<td style="text-align:right;">
18.046833
</td>
</tr>
<tr>
<td style="text-align:left;">
ND
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
21.79236
</td>
<td style="text-align:right;">
20.415848
</td>
</tr>
<tr>
<td style="text-align:left;">
NE
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
23.48598
</td>
<td style="text-align:right;">
21.048920
</td>
</tr>
<tr>
<td style="text-align:left;">
NH
</td>
<td style="text-align:right;">
17.78844
</td>
<td style="text-align:right;">
16.77731
</td>
<td style="text-align:right;">
7.243417
</td>
</tr>
<tr>
<td style="text-align:left;">
NJ
</td>
<td style="text-align:right;">
19.96563
</td>
<td style="text-align:right;">
19.31963
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
NM
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.448418
</td>
</tr>
<tr>
<td style="text-align:left;">
NV
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
20.849170
</td>
</tr>
<tr>
<td style="text-align:left;">
NY
</td>
<td style="text-align:right;">
18.75621
</td>
<td style="text-align:right;">
18.31489
</td>
<td style="text-align:right;">
15.887585
</td>
</tr>
<tr>
<td style="text-align:left;">
OH
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.43774
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
OK
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
25.07676
</td>
<td style="text-align:right;">
24.000040
</td>
</tr>
<tr>
<td style="text-align:left;">
OR
</td>
<td style="text-align:right;">
15.20318
</td>
<td style="text-align:right;">
16.39100
</td>
<td style="text-align:right;">
16.711553
</td>
</tr>
<tr>
<td style="text-align:left;">
PA
</td>
<td style="text-align:right;">
20.34185
</td>
<td style="text-align:right;">
19.40527
</td>
<td style="text-align:right;">
17.286934
</td>
</tr>
<tr>
<td style="text-align:left;">
RI
</td>
<td style="text-align:right;">
17.88116
</td>
<td style="text-align:right;">
17.46589
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
SC
</td>
<td style="text-align:right;">
23.68407
</td>
<td style="text-align:right;">
22.38995
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
SD
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
22.79495
</td>
<td style="text-align:right;">
20.639922
</td>
</tr>
<tr>
<td style="text-align:left;">
TN
</td>
<td style="text-align:right;">
25.81362
</td>
<td style="text-align:right;">
22.89642
</td>
<td style="text-align:right;">
19.457179
</td>
</tr>
<tr>
<td style="text-align:left;">
TX
</td>
<td style="text-align:right;">
28.74462
</td>
<td style="text-align:right;">
28.08021
</td>
<td style="text-align:right;">
26.500393
</td>
</tr>
<tr>
<td style="text-align:left;">
UT
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.754720
</td>
</tr>
<tr>
<td style="text-align:left;">
VA
</td>
<td style="text-align:right;">
21.34826
</td>
<td style="text-align:right;">
20.49998
</td>
<td style="text-align:right;">
17.954522
</td>
</tr>
<tr>
<td style="text-align:left;">
VT
</td>
<td style="text-align:right;">
NaN
</td>
<td style="text-align:right;">
16.89971
</td>
<td style="text-align:right;">
NA
</td>
</tr>
<tr>
<td style="text-align:left;">
WA
</td>
<td style="text-align:right;">
15.25193
</td>
<td style="text-align:right;">
17.80542
</td>
<td style="text-align:right;">
16.810354
</td>
</tr>
<tr>
<td style="text-align:left;">
WI
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.56563
</td>
<td style="text-align:right;">
17.994615
</td>
</tr>
<tr>
<td style="text-align:left;">
WV
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
19.31079
</td>
<td style="text-align:right;">
17.492150
</td>
</tr>
<tr>
<td style="text-align:left;">
WY
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
NA
</td>
<td style="text-align:right;">
13.748173
</td>
</tr>
</tbody>
</table>

Knit the document, commit your changes, and push them to GitHub.

## Question 5: Advanced Regression

Let’s practice running regression models with smooth functions on X. We
need the `mgcv` package and `gam()` function to do this.

- using your data with the median values per station, first create a
  lazy table. Filter out values of atmospheric pressure outside of the
  range 1000 to 1020. Examine the association between temperature (y)
  and atmospheric pressure (x). Create a scatterplot of the two
  variables using ggplot2. Add both a linear regression line and a
  smooth line.

- fit both a linear model and a spline model (use `gam()` with a cubic
  regression spline on wind speed). Summarize and plot the results from
  the models and interpret which model is the best fit and why.

``` r
df_filtered <- met_dt %>%
  filter(atm.press >= 1000 & atm.press <= 1020)


ggplot(df_filtered, aes(x = atm.press, y = temp)) +
  geom_point() +
  geom_smooth(method = "lm", col = "red") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), col = "blue") +
  labs(x = "Atmospheric Pressure", y = "Temperature", title = "Temperature vs Atmospheric Pressure") +
  theme_minimal()
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 389 rows containing non-finite values (`stat_smooth()`).
    ## Removed 389 rows containing non-finite values (`stat_smooth()`).

    ## Warning: Removed 389 rows containing missing values (`geom_point()`).

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
lm_model <- lm(temp ~ wind.sp, data = met_dt)


spline_model <- gam(temp ~ s(wind.sp, bs = "cr"), data = met_dt)


summary(lm_model)
```

    ## 
    ## Call:
    ## lm(formula = temp ~ wind.sp, data = met_dt)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -54.491  -4.333   0.052   4.649  28.509 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 20.536744   0.010038  2045.9   <2e-16 ***
    ## wind.sp      0.542727   0.002453   221.3   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 6.335 on 1880480 degrees of freedom
    ##   (688941 observations deleted due to missingness)
    ## Multiple R-squared:  0.02538,    Adjusted R-squared:  0.02538 
    ## F-statistic: 4.896e+04 on 1 and 1880480 DF,  p-value: < 2.2e-16

``` r
summary(spline_model)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## temp ~ s(wind.sp, bs = "cr")
    ## 
    ## Parametric coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 22.508691   0.004599    4894   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##             edf Ref.df    F p-value    
    ## s(wind.sp) 8.62  8.955 7348  <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.0338   Deviance explained = 3.38%
    ## GCV = 39.781  Scale est. = 39.781    n = 1880482

``` r
plot(spline_model, pages = 1, main = "Spline Model Fit")
```

![](README_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

## Deliverables

- .Rmd file (this file)

- link to the .md file (with all outputs) in your GitHub repository
