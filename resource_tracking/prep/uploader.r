#---------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Automatically upload Basecamp documents
# DATE: August 2019 
#---------------------------------------------------

#Set working directory to the root of this repository. 

rm(list=ls())
# install.packages("httr")
library(httr) #An R wrapper for JSON code
library(jsonlite)
library(data.table)

#----------------------------------------------
# STEP 1: OBTAIN OAUTH2 AUTHORIZATION, AND 
# REFRESH IF NEEDED 
#----------------------------------------------

  # HOW TO GET VERIFICATION CODE: 
  # 1st step
  # Go and create your application for basecamp at integrate.37signals.com. You can set ANY redirect url, it doesn't matter. Keep the client_id, client_secret and redirect url ready for following steps.
  # This has already been done for the application "uploader", and the client_id, client_secret, and redirect_uri are copied above. 
  # 
  # 2nd step
  # Open this url in a browser with dev-tools (I did it in Chrome):
  # https://launchpad.37signals.com/authorization/new?type=web_server&client_id=7af0652ea8e197d0427c9918427bdbd7d2b07e00&redirect_uri=https://3.basecamp.com/3769859/projects
  # 
  # 3rd step
  # Before you do ANYTHING on the page, open up the Network tab in the inspection tool (dev-tool?). Now continue with the process in the browser by logging in and granting the application access (the user you login with will be the one used by your bot/backend scripts that call the API).
  # 
  # When you land on your redirect page, this step is done.
  # 
  # 4th step
  # Look in the network tab and notice, that when you were redirected to your redirect url, there was added a query string parameter named code which disappears immediately in the actual browser address bar.
  
  #GET("https://launchpad.37signals.com/authorization/new?type=web_server&client_id=7af0652ea8e197d0427c9918427bdbd7d2b07e00&redirect_uri=https://3.basecamp.com/3769859/projects")
  
  # These are the main components of Oauth2 authorization. 
  # verification_code =  "4d57c22c" # THIS CODE NEEDS TO BE CHANGED EVERY TIME! 
  # client_id = "7af0652ea8e197d0427c9918427bdbd7d2b07e00"
  # client_secret = "3311c2551011a2021cfe4bc9ce317a9671e14f9f"
  # redirect_uri = "https://3.basecamp.com/3769859/projects"
  # user_id = "elineb@uw.edu"
  # 
  # request_url = paste0("https://launchpad.37signals.com/authorization/token?type=web_server&client_id=", client_id, "&redirect_uri=", redirect_uri, "&client_secret=", client_secret, "&code=", verification_code)
  # TOKEN_REQUEST = POST(request_url)
  # 
  # # Check http_type() of TOKEN_REQUEST
  # http_type(TOKEN_REQUEST)
  # 
  # # Examine returned text with content()
  # content(TOKEN_REQUEST, as = "text")
  # 
  # # Parse response with content()
  # content(TOKEN_REQUEST, as = "parsed")
  # 
  # # Parse returned text with fromJSON()
  # auth_data = fromJSON(content(TOKEN_REQUEST, as="text"))
  # saveRDS(auth_data, "C:/Users/elineb/Documents/gf/resource_tracking/prep/_common/uploader_auth.rds")
  
#Read in authorization, and set global variables. 
client_id = "7af0652ea8e197d0427c9918427bdbd7d2b07e00"
client_secret = "3311c2551011a2021cfe4bc9ce317a9671e14f9f"
redirect_uri = "https://3.basecamp.com/3769859/projects"
user_id = "elineb@uw.edu"

ACCOUNT_ID = "3769859"
BUCKET = "3931991"
VAULT = "602270806"

auth_data = readRDS("./resource_tracking/prep/_common/uploader_auth.rds")

# Get refresh token 
if (1==2) { # Emily figure out condition to automatically generate this! 
  refresh_url = paste0("https://launchpad.37signals.com/authorization/token?type=refresh&refresh_token=", 
                       auth_data$refresh_token, "&client_id=", client_id, 
                      "&redirect_uri=", redirect_uri, 
                      "&client_secret=", client_secret)
  REFRESH_TOKEN_NEW = POST(refresh_url)
  # Check http_type() of REFRESH_TOKEN_NEW
  http_type(REFRESH_TOKEN_NEW)
  
  # Examine returned text with content()
  content(REFRESH_TOKEN_NEW, as = "text")
  
  # Parse response with content()
  content(REFRESH_TOKEN_NEW, as = "parsed")
  
  # Parse returned text with fromJSON()
  auth_data = fromJSON(content(REFRESH_TOKEN_NEW, as="text"))
} 

# ----------------------------------------
# STEP 2: POST FILES TO BASECAMP
#-----------------------------------------

#Try one example first - create a document in Basecamp. 
base_url =paste0("https://3.basecampapi.com/", ACCOUNT_ID, "/projects.json")

try = GET(base_url, add_headers(Authorization = paste0("Bearer :", auth_data$access_token)))
# Parse response with content()
content(try2, as = "parsed")
if (try$status_code==401) print("Get attempt was unsuccessful.")



#------------------------------------------
# Try to get authorization
auth_url = "https://launchpad.37signals.com/authorization.json"
try2 = GET(auth_url, add_headers(Authorization = paste0("Bearer :", auth_data$access_token)))

# Parse response with content()
content(try2, as = "parsed")
