# TW-2020-Baseball-Vegas-Model

Code for predicting baseball games against Vegas' Moneylines


This code will use data from X sources
1) Baseball Prospectus' PECOTA Spreadsheets
  --To predict players Wins Above Replacement
2) MLB API (via BaseballR)
  --To get GameIDs from the league's official API
3) BaseballMonster.com
  --To get probable lineups and pitchers
4) Sportsbook
  --To get odds for each game for comparison to the model's projection
  --To find arbitrage opportunities from different books
5) Cluster Luck
  --Adjust run totals with cluster luck values
  
Using this data coupled with the functions I have created will predict each baseball game. These funtions predict each game by assigning a win percentage for a team
if they were to start every game using the same starting pitcher. This model does not take into account relief pitching as the variance of how much relief pitching
will play in a projection is variable, as it is difficult to project which relief pitchers will make appearances. PECOTA data is the most valuable for this model
becuase it is the best at predicting season performance for young/minor league players, which is necessary to be accurate as younger pitchers will make debuts later
in the season as time goes on and their projections variate based on their volume of production. The data is updated every once in a while and the code accounts for
this where it is easy to plug in the new projections to have the most accurate implied probablities as possible to beat Vegas.

Explaining The Methodology
In 2020 (60 Game Season), Gerrit Cole had a projected 1.9 WAR (https://www.baseballprospectus.com/orgs/new-york-yankees/depth-charts/pitchers/)
This means that in the 10 games he started he adds 1.9 wins above a replacement pitcher. While this does not seem like a lot for an ace like Gerrit Cole, we can 
break this down even further using the theory that every 10 runs is equal to one win. So using that, Gerrit Cole saved 19 more runs in 10 games than a replacement 
pitcher would.

Using the adjusted wins that Gerrit Cole created, we can use this to help predict a winning percentage if Cole pitched all 60 games in the season. Using the PECOTA
data I mentioned above, we can couple this with Baseball Prospectus' PECOTA standings that take into the account games already played as well as their overall 
projections for the team. As of 09/26/20, we see that the Yankees were projected to finish with a .560 win percentage, score 320 runs and have 269 runs against.
With Cole's 19 runs saved in 10 starts, we can extrapolate that to say: "If Cole pitched 60 starts, he would have saved 114 runs with a WAR of 11.4. As of 9/26/20,
the Yankees' starting pitching staff were projected to have a WAR of 5.0. If Cole were to start every game, I can predict that the Yankees starting pitching staff 
would have a WAR of 11.4. To find the advantage that Cole has over the rest of the starting pitching staff, we subtract the 5.0 WAR from Cole's WAR if he started a
60 game season of 11.4 to give us an advantage of 6.4 WAR or 64 runs. A lineup of all Cole would win 6.4 more games and therefore give up 64 less runs than other 
Yankees starting pitchers. This means that with Cole starting every game, the Yankees are now projected to give up 205 runs.

As most people are familiar with the pythagrean formula when it comes to baseball, I won't go into depth here. But using the projected runs scored and the projected 
runs against, we can give the Yankees a projected winning percentage if Cole started every single game. Here the winning percentage would be created using this
formula: ((RS)^1.83)/(((RS)^1.83)+((ADJ_RA)^1.83)))
  -RS: Runs Scored (320 for Yankees)
  -ADJ_RA Adjusted Runs Against (269-64 = 205)
 Leaving us with an adjusted win percentage of .693, a good win percentage and .133 percentage points better than the average Yankee starting pitcher.
 
Now to calculate the implied probablity of winning, I will do the same thing with another team and another pitcher. For this example lets use Toronto with Hyun-Jin
Ryu on the mound. Our numbers look like this: Ryu Games Started: 12, Ryu WAR: 1.4, TOR SP WAR: 4.7, TOR RS: 289, TOR RA: 298. 
To calculate our adjusted winning percentage with Ryu on the mound, we do the same thing.
-- 1.4*(60/12) = 7.0 WAR if Ryu pitched every start
-- 7.0 - 4.7 = 2.3 WAR advantage or 23 runs
-- 298 - 23 = 275 RA w/ Ryu on mound for all starts
-- ((289)^1.83)/(((289)^1.83)+((275)^1.83))) = .523 Winning Percentage w/ Ryu on mound

A small adjustment I like to make is called Cluster Luck and this value is applied to Runs Scores and Runs Against.
Cluster luck is the number of runs above or below expected that the team either left on base in an unlucky situation or were able to score on a lucky situation
(Error, Wild Pitch, Extra Bases, etc.0 and is applied to our Runs Scored and Allowed above. As of 9/26/20, NYY had an offensive cluster luck value of 23.7 and 
defensive value of -25.1. For offensive numbers, we subtract from RS and defensive we add to RA. TOR Off CL = 10.6 and TOR Def Cl = -10.5

Now the Adjusted Runs look like this:
NY RS: 320-23.7 = 296.3
NY RA: 205+(-25.1) = 230.1
TOR RS: 289-10.6 = 278.4
TOR RA: 275-(-10.5) = 285.5

Adjusting for Cluster Luck gives us a more normal picture of what the team is capable of doing and therefore is more indicitve of future results.

Now the final adjusted Win% for each team:
NYY: ((296.3)^1.83)/(((296.3)^1.83)+((230.1)^1.83))) = .615
TOR: ((278.4)^1.83)/(((278.4)^1.83)+((285.5)^1.83))) = .488

Now we need to figure out the implied probablity of each team winning this game. To do so, we need to decide who is the home team and who is the away team.
For this example we will say that the Blue Jays are the home team. This is only used to determine home-field adavantage and was determined using a logistic regression looking at the past 10 years of MLB games. From this analysis, we see that home teams usually have around a 5% advantage, and my model slpits this into a -2.5% disadvantage for the away team and a 2.5% boost for the home team. Here are the percentages we will need to run a simulation to predict how the game will wind up.
NYY Win: .615
NYY Loss: .385
TOR Win: .488
TOR Loss: .512

We have to look at these as probablities and regcognize that the only outcomes are a NYY Win and TOR Loss OR a TOR Win and NYY Loss. However, we need to multiply all these outcomes together to get a percentage chance of outcomes occuring.

NYY Win and TOR Loss: .31488
NYY Loss and TOR Win: .18788

With these being the only options (a TOR win and NYY Win cannot occur), we can now calculate the probablity of both events.

NYY Win: (.31488)/(.31488+.18788) = 62.63%
TOR Win: (.18778)/(.31488+.18788) = 37.35%

Now add home field advantage
NYY: 62.63% - 2.5% = 60.13%
TOR: 37.35% + 2.5% = 39.85%

Now we need to compare our implied probablity to Vegas. Lets say that the odds for this game are as followed:
NYY: -235
TOR: +200
My model has calculations built in and these odds can be converted to implied probablity:
NYY: 235/(235+100) = 70.15%
TOR: 100/(200+100) = 33.33%

Now if you see that the vegas implied probablity adds up to 103.48%. That is correct and not a typo as vegas makes its money on "juice" which is where the odds of
the both occuring is greater than 100% so that you will lose money if you bet both TOR and NYY. With the juice for this bet around 3.5, we would need to predict 
the game correctly over 53.5% of the time to be profitable. We also want our advantage to be greater than the impled odds. So lets check.

NYY: (My Model Probablity - Vegas Implied Probablity) = (60.13% - 70.15%) = -10.02
TOR: (My Model Probablity - Vegas Implied Probablity) = (39.85% - 33.33%) = +6.52

And we see that my model has Toronto winning this game more often than Vegas has projected.

Now lets say those odds were closer to NYY: (-160) and TOR (+140):
Our implied probablity would be NYY: 61.54% TOR: 41.67%
And our advantage for both teams would be negative:
NYY: 60.13% - 61.54% = -1.41%
TOR: 39.85% - 41.67% = -1.82%

I would not bet this game as the percent advantage we have over Vegas is negative for both teams and there is no value in betting this game at these odds.

Some factors to consider:

-- I did not factor team lineups and relief pitching into my model as the impact that relief pitchers and a single batter have on the probablity is miniscule and even tougher to model. However, I am working on fully automating these factors based on some preliminary research into how much variance in the probability is affected by lineup changes and relief pitching.
-- The 2020 season was somewhat odd in that there were no fans in any stadium. I considered adjusting the home field advantage, but decided to keep it for 2020 and 2021 to see if there was any boost in profitability in 2021, thus showing that the home field advantage was lower in 2020.
--Baserunning and defensive WAR were not considered in this model as well as I could not find stats that would translate to runs being scored that could easily be paired with PECOTA WAR.
--This model is based mostly on starting pitching as it is the greatest variable in changing Vegas odds.









