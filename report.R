#' Strive
#' 
#' 
library("rStrava")
library(dplyr)


# Before you start, create an app and put the different environment variables 
# in your .Renviron file. See the rStrava README for more information: 
# https://github.com/fawda123/rStrava
app_name = Sys.getenv("STRAVA_APP_NAME")
app_client_id = Sys.getenv("STRAVA_APP_CLIENT_ID")
app_client_secret = Sys.getenv("STRAVA_CLIENT_SECRET")
google_maps_key = Sys.getenv("GOOGLE_MAPS_KEY")
if(app_name == "" || app_client_id == "" || app_client_secret == "" || google_maps_key == "" ){
  stop("Please set STRAVA_APP_NAME, STRAVA_APP_CLIENT_ID and STRAVA_APP_SECRET, GOOGLE_MAPS_KEY")
}

stoken <- httr::config(token = strava_oauth(app_name, 
                                            app_client_id,
                                            app_client_secret))
# Info on me
me <- get_athlete(stoken, id = '2746741')
head(me)

# Get all activites
activities = get_activity_list(stoken)

# Get all runs
is_run = lapply(activities, function(x){x$type=="Run"})
runs = activities[unlist(is_run)]
head(runs)

# Get the last run 
a_run = get_activity(runs[[1]]$id, stoken = stoken)

# Get heart rate data for the last run 
heartrate = get_streams(stoken, id = runs[[1]]$id, types = list('heartrate'))

# Plot heart rate vs distance 
distance = heartrate[[1]]$data
hr = heartrate[[2]]$data
plot(distance,hr, type = "l")
