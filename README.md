# shinyplot
Shiny app for basic ggplot data viz, largely to be used for teaching purposes. Please file bugs or comments/feature requests under [Issues](https://github.com/sjspielman/shinyplot/issues). *Published app w/ the official shiny powers-that-be is forthcoming*


**Notable caveats**:

+ This app is *not* meant to be a comprehensive data visualization suite. It aims to showcase different visualizations from a *data* standpoint, for pedagogical purposes. Therefore, please keep feature requests reasonable. 
+ This app does little-to-no sanity checking, and absolutely zero mind-reading. It's up to you to choose your input data and plotting variables wisely! 



## Dependencies:

To run locally, you'll need the following packages:

+ `shiny`
+ `tidyverse`
+ `colourpicker`
+ `patchwork`


## Usage:

1. Upload your **`csv`** file under **Choose CSV File for plotting**. Note this *must* be comma delimited!

2. A series of options will present themselves:
	+ **Type of data to visualization to create** determines which kind of data to visualize
	+ For a quantitative option, **Which type of plot would you like to make?** will have you select one (or all) of several common plots used to visualize quantitative distributions. To view relative merits of plot styles, I recommend "Make All".
	+ 1-2 more prompts will appear asking you to specify variable(s) to plot
	+ For scatterplots, you can choose to turn off the linear regression line
3. Thanks to the lovely package `colourpicker`, you get to choose a custom color under **Select color**! This is the best feature, by a long shot.
4. When you're ready to go, click "Go!" For any changes you make, click "Go!" to update the plot.


Imporrantlty
