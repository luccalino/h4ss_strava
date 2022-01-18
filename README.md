# Strava Visualisations
This repo is the working folder for the course assignment from Wan Ri Ho and Lucca Zachmann.

# Project idea
- Get data from the Strava API :white_check_mark:
- Build simple shiny app to visualise spatial tracks :white_check_mark:
- Publish the app and let loggers use it (:x: not yet achieved)

# Requirements (user-side)
A Strava API needs to be set up prior to using the app. 
- To achieve this, login into your strava account at [https://www.strava.com](https://www.strava.com)
- After you’ve logged in, click on the arrow dropdown menu to the right of your profile picture and choose “settings” at [https://www.strava.com/settings/profile](https://www.strava.com/settings/profile).
- Then you will need to go to [https://www.strava.com/settings/api](https://www.strava.com/settings/api) to create an app.
- Here, there will be three things you need: 1) App name, 2) Client ID and 3) Client Secret for this app!

# Features
- Data is called via online requests from [OpenStreetMap](https://de.wikipedia.org/wiki/OpenStreetMap) (no need to load any database)
- App can be used anywhere in the world
- English language only (can be easily extended)
- Riding and running activities (other actives can easily be included)

# Exemplary plot
This is how a plot can look like. In this case, it is the Zurich area.

![exemplary_plot](/plots/app_output.png)