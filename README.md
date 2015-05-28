# Crime_Prediction
R scripts for predict future crime incidents in Chicago city area, by applying Logistic regression, Logistic regression and Random Forest, from Twitter-derived information (originally saved in PostgresSQL database), weather data and standard crime prediction approach based on kernel density estimation (KDE)

## Utility Functions
**CrimePredUtil.R** is a script for all utility functions. The discription of parameters and outputs are inlcuded in the script. 

## Create Senitment Lexicon Dictionary
Go to http://www.cs.uic.edu/~liub/FBS/opinion-lexicon-English.rar to download a list of positive and negative opinion words or sentiment words for English (around 6800 words). The list was compiled by Hu and Liu Minqing Hu and Bing Liu.
Use **SentimentLexicon.R** is a script for adding your own positive/negative words and creating a sentiment dataframe:
```
POLKEY
```
for furture use in `polarity` function. 

## Connnect to PostgresSQL Database
Make sure to have PostgresSQL database configured. If not, download/configured postgresSQL database management system according to http://www.postgresql.org/download/. Then, import twitter data in to a database you created.
**TwitterSentiment.R** has exampel code for connecting PostgreSQL (you will need to modified the code):
```
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, host = 'localhost', port='5432', 
                 dbname = 'postgis_tweet',user = 'postgres', 
                 password='secret')
```
## Clean up twitter data and calculate sentiment score
In **TwitterLexicon.R** script, use `tweet.qry` to draw query for twitter posts within chicago area. You can set different time period. Then you can clean up twitter posts to replace emoticans, remove punctuation, etc. using `tweet.clean` function.
Next, aggregate tweets within the same time period (in this case, 6 hours) and the same neighborhood together as a single document. Calaculate the snetiment score for each document using `polarity` function. 
Furthermore, the trend index for sentiment score is derived by `trend_ind` function.
The script will retune a datafrme: 
```
Jan.2014.pol.trend.6hour
``` 
which contains the time period, grid_id(neighborhood) and sentiment polarity and its trend.

## Get crime density using KDE
Before calsulating crime density, make sure you have historical crime data from Chicago data portal, https://data.cityofchicago.org/. In this case, we use theft incidents data happened in 2013-2014, which contains timestamp and longitude/latitude.
**CrimeDensity.R** is a script for calculating historical crime density in each neighborhood in chicago area.  
The script will retune a datafrme: 
```
theft.training.density
``` 
which contains the time period, grid_id(neighborhood) and density.

## Clean up weather data
Get weather data in Chicago from weather underground, http://www.wunderground.com/history/airport/KORD/2014/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=2014&req_city=NA&req_state=NA&req_statename=NA&format=1. It contains high/low/min tempreture, dew points, windspeed, etc.
**WeatherDensity_6h.R** is a script for cleaning up weather data by assigning min/max/mean data to different period of the day. Also, you can use the script to merge the weather data with crime density data.

## Prepare trainingand test data
In script **ModelingPrediction.R**, you can first merge crime density, weather data and sentiment features together according to their grid_id and period number (sixhr_n). Then mark the crime points as positive points and non-crime points as negative points. Since the response is unbalanced, use the `ovun.sample` to under-sample negative response.
The returned data frame is:
```
Jan.train.balanced
```

## Model and Evaluation





## Version 
* Version 1.0

## Thanks
This code was created under Mathew Gerber's guide within Capstone course at UVa during Spring 2015. Respect and gratitude is due to Professor Gerber. Also, appreciate is due to Northrop Grumman's sponsorship.

## Contact
#### Shin Xinyu Chen
* GitHub: https://github.com/ShinXinyuChen
* e-mail: xc7xn@virginia.edu; xinyu.shin.chen@gmail.com
* Twitter: [@twitterhandle](https://twitter.com/twitterhandle "twitterhandle on twitter")
