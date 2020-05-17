# Climate Data Application

## Introduction:
The purpose of our climate data application is to display historical data on CO2 emissions and temperature anomaly side by side in a cohesive way. We will be creating three displays; a line graph, spiralized graph and world map.

## Data
We will be using two datasets. The first is a dataset from the National Oceanic and Atmospheric Administration’s (NOAA) Global Historical Climatology Network. It contains global average temperature anomalies in degrees Celsius for every month from January 1880 to 2017. The data can be found at https://www.ncdc.noaa.gov/cag/data-info/global.

## Displays
### Line
The line graph plots two linear regressions, one for world average CO2 emissions over year and one for world average temperature anomaly over year. On the x-axis is year. The y-axis is a dual axis with tonnes per person on one side and temperature anomaly on the other. Using the side panel, the user will be able to select the year range to display.
* <b>Interactive Features:</b> hovering the mouse over any point on the regression line will display the year and temperature anomaly or CO2 emission.

### Spiralized Graph
The spiralized graph plots average CO2 emissions and average temperature anomaly. The color of the spiral represents the level of CO2 emission, and the distance from the center represents temperature anomaly. The graph is animated by year. 
* <b>Interactive Features:</b> hovering the mouse over any point on the spiralized graph will 
display the year, temperature anomaly and CO2 emission.

### World Map:
The world map plots CO2 emissions by country, with the fill being animated by year. Using the side panel, the user will be able to select the year range to animate.
* <b> Interactive Features:</b clicking on any colored area of the map will display the country name and CO2 emission level for that year.

