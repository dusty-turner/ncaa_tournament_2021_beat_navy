# ncaa_tournament_2021_beat_navy
Repo for Mens NCAA Tournament 2021 for team Beat Navy

## Team Beat Navy 

Dusty Turner: dusty.s.turner@gmail.com 

- Former Engineer Officer and current Army Operations Research Systems Analyst at Center for Army Analysis in FT Belvoir, VA  
- Masters in Industrial and Systems Engineering and Graduate minor in Applied Statistics from THE Ohio State University in Columbus Ohio  
- Worked from 2016-2019 teaching various statistics courses as an Assistant Professor at The United States Military Academy at West Point  
- Enjoys R, spoRts, and unnecessaRily capitalizing Rs in his sentences  

Jim Pleuss: jim.pleuss@gmail.com  

- Former Signal Officer and currently the Assistant Dean for Plans, Analysis, and Personnel at United States Military Academy, West Point, NY 
- Masters in Operations Research from Kansas State University  
- Assistant Professor at West Point from 2016-Present, teaching probability and statistics.  
- Spends an unhealthy amount of time watching, playing, pontificating about sports 

## Opening Thoughts 

We want to mention a few things before we jump into our model summary. 

- Thanks everyone who ran and competed in the competition.  This was a lot of fun. 
- We think our model was “pretty good” - but we certainly rode the high end of our variance.  If the basketball doesn’t bounce our way in a few games, our model goes from “pretty good” to “unremarkable”.  Most notably our model gave Oral Roberts 30% to beat Ohio State, 37% for Florida, but then only 5% against Arkansas (which all worked out in our favor... barely). 

So having said that.... 

## Summary 

We used a random forest model to predict winning outcomes.  We considered end of season summarized statistics, key wins / losses, specific Massey Ordinals, and efficiency metrics.  We tuned our random forest parameters by selecting the parameters with the best classification accuracy (should have used log loss in hindsight but it worked out for us).  We trained multiple models on all seasons through 2014, testing on 2015-2019, and chose the model with the lowest Kaggle score (log-loss).   

## Key factors in our model 

- Average ranking across all Massey Ordinals 
- Clustering of teams based on offensive efficiency, defensive efficiency, percentage of points from 3s, possessions per game, assist to turnover ratio 
- Winning percentage 
- Free throw rate   
- COL Ranking from Massey Ordinals  
- Conference win/loss against all other conferences 
- Quad 1 wins and Quad 4 losses  
- Tournament Seed of Team 
- Schedule Ranking 
- The following average statistics from the last 3 games of the season 
-- Offensive efficiency 
-- Defensive efficiency 
-- Possessions per game  

## External Data 

We used data from the link below for team efficiency, percentage of points from 3s, possessions per game, and assist to turnover ratio.   

https://www.teamrankings.com/ncb/rankings/ 

## Modeling Workflow 

While we feel the most important part of creating this model was development of features, our modeling workflow helped keep things organized.  We did the following: 

- We created one r script that did all of our data cleaning and sourced it in our execution script. 
- We created one r script that contained all the functions we used and sourced it in the main execution script. 
- We created a model registry that tracked factors used in the model, model type, tuning grid size, model performance, and special notes we wanted to track. 
- We created one major function to execute models.  This function did the following: 
- Check the model registry to see if the model had already been executed. 
- Used the tidymodels workflow to create test/train data, develop a tuning grid, tune the model through cross validation, select the best model parameters, apply it to the entire dataset, then apply it to the validation data.   
- Save model performance information to the model registry. 

## Things we tried that didn’t work as well 

We tried xgboost and elastic net models and they did not perform as well.    

## Minimum Viable Code  

[github repo](https://github.com/dusty-turner/ncaa_tournament_2021_beat_navy)  

`git clone git@github.com:dusty-turner/ncaa_tournament_2021_beat_navy.git`