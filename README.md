# NFL Weather Study - QB Impact Analysis

Sudarsh Manderwad

This is the repository for an NFL research project where I looked into how weather (things like temperature, wind speed, precipitation, and stadium roof types) affects how well NFL quarterbacks can actually throw the football. I used R and ggplot2 to look at passing trends from 2000 to 2021.

## What we found out:

-   **Temperature doesn't matter as much as you think:** I found a tiny statistical relationship, but honestly, temperature has basically zero practical impact on a QB's completion percentage or how often they get sacked.
-   **Wind is the real killer:** Wind speed had a clear, strong negative impact on both completion percentage (p = 0.0001) and how far the ball travels downfield per attempt (p = 0.0008). High winds definitely ruin accuracy and force teams to play a short-passing or running game.
-   **Coaching takeaways:** Since you can't control the wind, defenses can take advantage of windy days by focusing heavily on stopping short passes, and offenses have to adjust their playbook accordingly.

## Packages you need:

If you want to run the code, make sure you install these packages in RStudio first:

``` r
install.packages(c("dplyr", "ggplot2", "nflreadr"))
```

## Where the data comes from:

The script pulls and merges data from two places: 1. **NFL Stats:** Downloaded automatically right in the script using the `nflreadr` package. 2. **Weather Records:** This uses a historical weather dataset put together by J.B. Thompson on GitHub (`ThompsonJamesBliss/WeatherData`).

## Poster

I uploaded our final presentation poster directly to this repository as well, so you can see all our actual scatterplots, boxplots, and regression lines in one clean layout!

![Uploading NFL_Weather_Study_Poster.png…]()



