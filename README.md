
# projproj

<!-- badges: start -->
<!-- badges: end -->
## Interactive Shiny helper for map projections and plot extents

Intended as a small utility for exploring how maps behave under different
coordinate reference systems, especially for regional, polar, and world maps.

Displays a ggplot map rendering of a given bounding box and projection. The app
also generates the code to reproduce the map in R.

## Installation

You can install the development version of projproj like so:

``` r
# install.packages("pak")
pak::pak("setnorth/projproj")
```

## Running the program

In order to run the program run:

``` r
projproj::ne_projection_helper()
```
