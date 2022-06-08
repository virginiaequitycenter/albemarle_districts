rsconnect::deployApp(appName = "albemarle_districts", 
                     appFiles = c("about.html", "app.R", 
                                  "data/app_data.Rdata", "data/summaries.Rdata", "data/crd_data_geo.RDS",
                                  "www/ACLGSeal-Color-WEB.png", "www/Equity_Center_Logo-STAMP-transparent.png",
                                  "www/style.css"),
                     account = "virginiaequitycenter") 

# rsconnect::deployApp(appName = "albco_districts", 
#                      appFiles = c("about.html", "app.R", 
#                                   "data/app_data.Rdata", "data/summaries.Rdata",
#                                   "www/ACLGSeal-Color-WEB.png", "www/Equity_Center_Logo-STAMP-transparent.png"),
#                      account = "commpaslab") 
