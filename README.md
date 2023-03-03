# Final Project:  Create a ShinyApp with an Interactive Map

## Live Link

You can view the live app [here](https://mgermaine93.shinyapps.io/mgermai2-HW3/).

## General Description

This is my third of three homework assignments for CMU's Spring 2023 "R for Operations Management" course.

## Assignment Details and Directions

To bring the entire course together students will create an App with an interactive map as the central focus.

Requirements:

* One (1) leaflet map.
* The app should have at least two different types of layers (points/markers, lines, heatmap, polygons etc.).
* Leaflet map must make use of LeafletProxy for updating layers (observers).
* One (1) datatable,
* Two (2) interactive charts or graphs (plotly).
* Three (3) input commands.
* One (1) downloadHandler that allows users to get the raw data they are viewing.

Students who use one or more API's to feed either their map or data displayed will receive up to 20 Bonus points on the assignment.

## Data Source

The data source I used is one of my own creation.  I compiled a bunch of data together about each person from my wedding's guest list and organized it all into a GeoJSON file.

It was a highly manual process, but in the end I believe it came together quite nicely.  Of course, any errors (particularly in the ```generation``` category) are my own.

## Improvements I Would Make in the Future

Given more time, there are a few improvements I would make:

* As the project stands, the two bar plots draw from the entire wedding guest list, filtered by the user-input ```state``` or ```generation``` parameters.  Ideally, I would change this so that they instead draw from these user inputs AS WELL AS from the ```guest type``` parameter.  This would ideally result in a [grouped bar chart](https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2), where the grouped bars are broken down by ```guest type```, but as it stands this is challenging to do with the way my data is organized.
* I would make the choropleth map dynamic based on user inputs as well.
* I would adjust the heights of the two bar plots so that there wasn't as much whitespace on the page.
* I would experiment with adding a great variety of map tiles. (e.g., I was never quite able to figure out how to properly display the Stadia map.)
* I would play around with the styles more so that I could use [shinydashboardPlus's](https://rinterface.github.io/shinydashboardPlus/articles/shinydashboardPlus.html#:~:text=shinydashboardPlus%20relies%20on%20the%20same,shinydashboard) [midnight mode](https://rinterface.github.io/shinydashboardPlus/articles/more-skins.html) without the download button and the datatables becoming near-impossible to read.