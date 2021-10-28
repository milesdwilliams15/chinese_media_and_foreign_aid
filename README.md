# Xinhua Coverage and Chinese Foreign Aid

Contributors are:

  - Miles D. Williams 
  - Lucie Lu 


# Think about future direction:

  - look at the sentiments for news articles covering existing Chinese aid programs
  - use the salience of countries in Xinhua's news articles to predict aid allocation in the upcoming years (win!)

# Data analysis plan

  - Hypothesis: Chinese government suppresses news coverage over where they give aids. 
  - Potential explanation: aids are not so popular among Chinese publics; maybe safer to keep it secret rather than promote it. 
  - Conventional studies: more coverage -- > higher salience -- > justification of more aids to the recipient countries from the perspectives of aid-giving developed democratic countries;
  - China may be an odd case. So we want to loo at whether Chinese aid allocaion can predict a drop in the media coverage in the subsequent years. More aids -- > less coverage


# Meeting memo (9/30/2021)
- Look at summary statistics of average countries mentioned so we have a baseline of media coverage across aid-recipient countries
- Try a bunch of prediction models to increase the accuracy
- Think about other predictors we can put in X (more leeways in doing predictions)
- A research design that speaks more directly to our theory 

# Meeting memo (10/28/2021)
## Design
- subset of countries: African countries?
- subset of news categories: economy? (need to revisit the dataset)

## Model specification
- look at the distribution of data; outliers and stuff
- measure of salience: counts; frequency; ranking
- look at the preliminary results in the imputed data and the non-imputed raw data to decide whether we should try another imputation method 
- try different model specifications

## data sources
- the aid data is also scrapped from the news, but we defend ourselves in saying those data not only use Xinhua source, so the data sources are not completely overlapping.
