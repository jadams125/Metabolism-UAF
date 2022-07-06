#Basic StreamPULSE data processing pipeline
#Updated 2020-05-11
#Contact Mike Vlah (michael.vlah@duke.edu) with questions or comments.

# Install StreamPULSE pipeline tools from GitHub
# The StreamPULSE package is in development and changes frequently!
# If something doesn't work as expected, first try reinstalling.
remotes::install_github('streampulse/StreamPULSE', dependencies=TRUE)

# Install latest version of streamMetabolizer.
install.packages('streamMetabolizer', dependencies=TRUE,
                 repos=c('https://owi.usgs.gov/R','https://cran.rstudio.com'))

# Load packages.
library(StreamPULSE)
library(streamMetabolizer)

# View all available sites and their metadata
query_available_data(region='all')

# View all variables at a site, and full available time range for that site.
# Note that USGS depth and discharge data may be available for sites that
# have associated USGS gage IDs, even if depth and discharge do not appear among
# the variables returned here. If USGS data are available, they will be acquired
# automatically when you use prep_metabolism below. Likewise, air pressure and
# PAR estimates will be automatically acquired below, if necessary.
query_available_data(region='AZ', site='OC')

# Select site and date range for which to acquire StreamPULSE data.
# site_code is a combination of regionID and siteID
site_code = 'AZ_OC'
start_date = '2018-06-01'
end_date = '2018-07-01'

# Download data from streampulse.
# Your token can be found by clicking the gear icon in the upper right of data.streampulse.org,
# though note that no token is needed if downloading public data.
sp_data = request_data(sitecode=site_code,
                       startdate=start_date, enddate=end_date)

# Choose model type for streamMetabolizer.
# Only "bayes" is available at this time.
model_type = 'bayes'

# Which modeling framework to use:
# Use "streamMetabolizer" (the default); "BASE" is not available at this time.
model_name = 'streamMetabolizer'

# Format data for metabolism modeling.
sp_data_prepped = prep_metabolism(d=sp_data, type=model_type,
                                  model=model_name, retrieve_air_pres=TRUE)

# Fit metabolism model and generate predictions (calls streamMetabolizer
# functions: mm_name, specs, metab, predict_metab).
model_fit = fit_metabolism(sp_data_prepped)

# Plot results and diagnostics (This behaves unpredictably on some machines.
#If you sent your results to the data portal in the step above, you can view
#them more robustly there. If your data do not all occur within the same
#calendar year, the visualizations may still not work.)
plot_output(model_fit)

#Here's where results and diagnistics live on the data portal:
# http://data.streampulse.org:3838/streampulse_diagnostic_plots/