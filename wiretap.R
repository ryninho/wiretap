################################################################################
###
### Wiretap: Anomaly detection with notifications
###
### The purpose of this script is to run a sql file yielding a time series,
### detect statistical anomalies and notify the target audience if it finds them
###
################################################################################

print("
This script takes univariate time series data of the form (date, value)
and, with the -s flag set, decomposes the level, trend and weekly cycles to produce
a 'forecast' for the most recent measurement with which to compare the actual value.
If the actual value is outside of a reasonable prediction interval based on recent
trends then an alert email is sent to warn of the outlier, complete with graphs
and the text of the query used to generate the time series.
")

# TODO: with flags, remove seasonal trend to tighten forecast bands when there is no seasonality
# TODO: look for weekly trends if and always if it's clearly daily data so flag isn't needed
# TODO: "unit root tests" with the fUnitRoots package to test for seasonality


print("Note that time series must contain equally spaced measurements, so daily or weekly
      totals are good but hourly data spanning multiple days with inconsistent start and end times
      is not good")

# TODO: fill in missing hours if needed (there must be an R package for this- just need zeros... 
# try seq.Date or zooreg)

# Set directory and import libraries and functions ------------------------

library(sendmailR) # for emails from R
library(forecast) # forecasting and time series tools
library(caroline)
library(Hmisc) # for %nin%
# library(zoo) # using this to simplify indexing values on date
# library(ggplot2) # for nice graphs

# for development: 
# setwd("/Users/Eric/data-science")
setwd("/app")
source("common/r/dbconnection_read_replica.R")
source("common/r/functions.R")
# TODO: add ability to send to multiple email addresses
# TODO: add name of query result set to arguments, OR just use the end of the path to name the .txt
# TODO: integrate more directly with sql_query_and_send so changes to one don't have to be in both places

# for demo: connect to redshift
# source("common/r/dbconnection_redshift.R")
# args <- c("/Users/Eric/data-science/metrics_services/wiretap_test_righi.sql"
#           , "eric@instacart.com"
#           , "-s")

# Parse command line arguments --------------------------------------------

# assumes this environment variable is set: MY_EMAIL_ADDRESS="my_email@domain.com"
args <- commandArgs(trailingOnly = TRUE)  # Run with: Rscript wiretap.R path_to_time_series_query.sql your_email@domain.com -s

# for development
# args <- c("/Users/Eric/data-science/metrics_services/wiretap_test_actual_do_iss_ratio.sql"
#           , "eric@instacart.com"
#           , "-s"
#           , 4)

# args <- c("/Users/Eric/data-science/metrics_services/wiretap_test_actual_do_iss_ratio.sql"
#           , "eric@instacart.com"
#           , "-s")

# args <- c("/Users/Eric/data-science/metrics_services/wiretap_test_actual_do_iss_ratio.sql"
#           , "eric@instacart.com")

# set the key variables
query_path <- args[1]
email_from <- Sys.getenv("MY_EMAIL_ADDRESS")
email_to <- as.character(args[2])
email_subject <- paste0("Anomaly caught! Take a look. Via ", query_path)
email_body_text <- "Here are the results of the query:"

path_vector <- strsplit(query_path, "/")[[1]]
query_name <- path_vector[length(path_vector)]
print(query_name)


# frequency of seasonality (7 = weekly, for a daily chart)
f <- ifelse(length(args) < 3,
            1, 
            ifelse(args[3] == "-s" & length(args) == 3,
                   7,
                   as.integer(args[4])
            )
)
paste0("frequency: ", f)
# number of records to test for anomalousness
k <- 1

# p_taxes <- "/Users/Eric/data-science/metrics_services/wiretap_test_taxes.sql"
# p_ratio <- "/Users/Eric/data-science/metrics_services/wiretap_test_do_iss.sql"
# p_deliveries <- "/Users/Eric/data-science/metrics_services/wiretap_test_deliveries.sql"

# p_actuals <- "/Users/Eric/data-science/metrics_services/wiretap_test_actual_do_iss_ratio.sql"
# p_righi <- "/Users/Eric/data-science/metrics_services/wiretap_test_righi.sql"

# query_path <- p_taxes
# q_taxes <- results
# query_path <- p_ratio
# q_ratio <- results
# query_path <- p_deliveries
# q_deliveries <- results
# query_path <- p_actuals
# q_actuals <- results

# source the query
query <- readChar(query_path, file.info(query_path)$size)

# run the query on the read replica
print("running query...")
results <- dbGetQuery2(con, query)

print("number of rows:")
print(nrow(results))
print("result set size as data frame: ")
print(object.size(results))

# pick one for DEVELOPMENT
# results <- q_taxes
# results <- q_ratio
# results <- q_deliveries
# results <- q_actuals



# TODO: probably need to make sure the time series is in ascending order before coverting to ts()

# number of records total
n <- nrow(results)

# Forecast time series ----------------------------------------------------

# plot(results, type = 'l', lwd = 2)

# make forecast for items n - k, n - (k-1), ..., n based on 1, ..., n - (k + 1)
train_index <- 1:(n - k)
train <- results[train_index,]
# start_train <- train[1, 1]
# end_train <- train[nrow(train), 1]
test <- results[1:n %nin% train_index,]

# time series without test period
# ts_zoo <- zoo(results[,2], results[,1], frequency = f)
# ts <- ts(ts_zoo)
ts <- ts(train[, 2], frequency = f)

# plot(ts, type = 'l', ylim = c(min(results[,2]), max(results[,2])))

# make forecast
# TODO: set confidence level for plot intervals based on command line input so the graph matches the threshold
fcst <- forecast(ts, h = max(7, n * 0.2, f, k))
# TODO: use "robust = TRUE"?
plot(fcst, 
     ylim = c(min(results[,2]), max(results[,2])), 
     xaxt = 'n',
     ylab = colnames(results)[2], xlab = query_name, lwd = 2
)

fcst
fcst_axis <- as.numeric(rownames(as.data.frame(fcst)))
 
points(fcst_axis[1], 
       results[n, 2], 
       pch = '*',
       cex = 2.5
)
# TODO: change this to a line

plt <- recordPlot() # so I can email it

# fcst <- forecast(ts, level = c(20, 40))
# plot(fcst, ylim = c(min(results[,2]), max(results[,2]))); points(fcst_axis[1], results[n, 2], pch = 19)
# fcst <- forecast(ts, level = c(50, 80))
# plot(fcst, ylim = c(min(results[,2]), max(results[,2]))); points(fcst_axis[1], results[n, 2], pch = 19)
# fcst <- forecast(ts, level = c(90, 99))
# plot(fcst, ylim = c(min(results[,2]), max(results[,2]))); points(fcst_axis[1], results[n, 2], pch = 19)

# force seasonality even with non-seasonal portion
# ts_all <- ts(results[, 2], frequency = 7)
# plot(forecast(ts_all))

# plot(forecast(results[, 2]))

# plot(forecast(results[14:nrow(results), 2]))


# So was it an outlier? ---------------------------------------------------

# is it above the upper 95 or below the lower 95?
outlier_detected <- test[1, 2] < fcst$lower[1, 2] | test[1, 2] > fcst$upper[1, 2]
# TODO: extend to multiple examples (k > 1)

print("outlier detected?")
print(outlier_detected)

print("save output as png...")
# save png either way
if (outlier_detected) {
  png(paste0("caught.", query_name, ".WARNING", ".png"))
  plot(fcst, ylim = c(min(results[,2]), max(results[,2])), xaxt = 'n')
  points(fcst_axis[1], results[n, 2], pch = '*')
  dev.off()
#   dev.copy(png, paste0("caught.", query_name, ".WARNING", ".png"))
#   dev.off()
  send_email <- TRUE
} else {
  png(paste0("caught.", query_name, ".png"))
  plot(fcst, ylim = c(min(results[,2]), max(results[,2])), xaxt = 'n')
  points(fcst_axis[1], results[n, 2], pch = '*')
  dev.off()
#   dev.copy(png, paste0("caught.", query_name, ".png"))
#   dev.off()  
  send_email <- FALSE
}

print("send email?")
print(send_email)

# Send IF something’s anomalous OR it’s been a while ----------------------

if(send_email) {
  sendmail(from = email_from,
           to = email_to,
           subject = email_subject,
           msg=c(mime_part(plt, "time series with forecasts", width = 20),
                 email_body_text, "\n", "\n",
                 mime_part(results, "query results", sep = ",", row.names = FALSE), "\n",
                 "\n", "SQL query that generated this result:", "\n", "\n",
                 query
           )
  )
}

print("email sent")


# Close out ---------------------------------------------------------------

# save.image("~/data-science/metrics_services/wiretap_CLI_run.RData")

# close out the connection
print("disconnecting...")
dbDisconnect(con)
detach("package:RPostgreSQL", unload=TRUE)
print("end of script.")
