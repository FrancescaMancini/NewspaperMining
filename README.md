# NewspaperMining
##R code for downloading articles from The Guardian and The New York Times 

The file NewspaperMining.R contains R code for downloading articles from the New York Times and the Guardian containing certain keywords.
The first part of the script uses the packages GuardianR and stringr to download and clean the data from the Guardian articles. In order to run the script you will need to apply for an API key from [this website](http://open-platform.theguardian.com/access/) for the Guardian and [this website](https://developer.nytimes.com/signup) for the New York Times.
Once obtained the keys, create two objects in R named "key" (for the Guardian) and NYTkey for the New York Times containing the character strings with your API key

The second part of the script uses the packages httr and XML to download data from The New York Times.

The .txt files contain the datasets collected using the script. 

The file NewspaperMining.Rproj is the RStudio project. 
