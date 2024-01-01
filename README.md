# garmin_wrapped

## Overview

**Are you a data nerd or enthusiast looking to dive into your fitness analytics, but want to dive deeper than Garmin Connect or Strava allows?** Maybe you're dreaming up the next new running metric and want to pilot it on some of your previous runs. Or, maybe you just want to condense this data into a few nice figures-- Spotify Wrapped style. 



I've written some scripts that replicate figures similar to those in Connect, as well as a script to plot paths in a manner similar to Strava (Premium) heatmaps. 



You can also add your own metrics/plots and even play around with per-second data only accessible at the `.fit` level. For example, I wanted to test the effect of a few different smoothing windows on elevation gain/loss (Total Ascent/Descent) metrics, and see how the distributions of these estimates compare with Garmin's own (likely more sophisticated) estimate. It appears a 40s smoothing window gets me pretty close to Garmin's estimate...but too small of a window and barometer noise can vastly overpredict ascent/descent on some runs.

![elevation_compare_time.png](![Elevation_Plot](/Figures/elevation_compare_time.png))



Or maybe, you've trained in 3 different states (varying in geography) and want to see if the general hilly-ness of your runs is comparable between the states. 



On top of looking at how this year's runs compare to all 1000+ runs I've completed, I also wanted to see how my training by season this year compared to all my runs. Seems like having COVID in Feb put me at a winter mile deficit!



---

## Getting and Preparing Your Data from Garmin

At the time of writing this, Garmin allows you FULL access to your data and metrics. To run my code, you just need to get all of your activity-related `.fit` or `.txt` files ever recorded (over 40,000 over 12 years in my case!) into a single folder. It's a little annoying in that Garmin doesn't save these files with any decipherable filename you can use to separate runs from other activities and daily health metrics. So...the code needs to parse through all of those files. 



You can request all your data from Garmin [here](https://www.garmin.com/en-US/account/datamanagement/) under '**Export Your Data**'. They will email you a massive compressed file with everything they have on you. 



Download that file, extract the folders from it, and navigate to `/DI_CONNECT/DI-Connect-Uploaded-Files/` . There will be some more compressed folders starting with `UploadedFiles...`. Extract all of them, and merge the contents of all three folders into a separate folder. This is a lot of files, so you're encouraged to use `mv` or `cp` in the terminal if it takes your computer a long time to load them all. 



Know the address of your new folder with all `.fit` or `.txt` files, it will be important in the next step.

---

## Using the code

If you can handle some basic R programming, this repository hopefully should get you started (pardoning a bit of inefficient coding on my part, this is my first R project). 