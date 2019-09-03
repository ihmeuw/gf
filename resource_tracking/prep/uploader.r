#---------------------------------------------------
# AUTHOR: Emily Linebarger 
# PURPOSE: Automatically upload Basecamp documents
# DATE: August 2019 
#---------------------------------------------------


# install.packages("httr")
library(httr) #An R wrapper for JSON code
library(jsonlite)

#----------------------------------------------
# STEP 1: OBTAIN OAUTH2 AUTHORIZATION, AND 
# REFRESH IF NEEDED 
#----------------------------------------------

if (1==2) { #This only needs to be run once, to get initial authorization token
  
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
  
  # These are the main components of Oauth2 authorization. 
  verification_code =  "f1b4c468" # THIS CODE NEEDS TO BE CHANGED EVERY TIME! 
  client_id = "7af0652ea8e197d0427c9918427bdbd7d2b07e00"
  client_secret = "3311c2551011a2021cfe4bc9ce317a9671e14f9f"
  redirect_uri = "https://3.basecamp.com/3769859/projects"
  user_id = "elineb@uw.edu"
  
  TOKEN_REQUEST = POST(paste0("https://launchpad.37signals.com/authorization/token?type=web_server&client_id=", client_id, "&redirect_uri=", redirect_uri, "&client_secret=", client_secret, "&code=", verification_code))

  str(content(TOKEN_REQUEST)) #This should tell you your access token, refresh token, and expiration date. 
} 

ACCESS_TOKEN = "BAhbB0kiAbB7ImNsaWVudF9pZCI6IjdhZjA2NTJlYThlMTk3ZDA0MjdjOTkxODQyN2JkYmQ3ZDJiMDdlMDAiLCJleHBpcmVzX2F0IjoiMjAxOS0wOC0yOVQxOTowNjo0OFoiLCJ1c2VyX2lkcyI6WzM3OTk2MDM3XSwidmVyc2lvbiI6MSwiYXBpX2RlYWRib2x0IjoiMjJhZTVhZmQ0YjI2ODY4ZTk5ZDZhMjFjMDFmNTU0NDAifQY6BkVUSXU6CVRpbWUNs98dwPEDBBsJOg1uYW5vX251bWkCWAE6DW5hbm9fZGVuaQY6DXN1Ym1pY3JvIgc0QDoJem9uZUkiCFVUQwY7AEY=--c46bddc39cebdba1f3fb7c09e268f2402764671a"
REFRESH_TOKEN = "BAhbB0kiAbB7ImNsaWVudF9pZCI6IjdhZjA2NTJlYThlMTk3ZDA0MjdjOTkxODQyN2JkYmQ3ZDJiMDdlMDAiLCJleHBpcmVzX2F0IjoiMjAyOS0wOC0xNVQxOToxMjo1MVoiLCJ1c2VyX2lkcyI6WzM3OTk2MDM3XSwidmVyc2lvbiI6MSwiYXBpX2RlYWRib2x0IjoiMjJhZTVhZmQ0YjI2ODY4ZTk5ZDZhMjFjMDFmNTU0NDAifQY6BkVUSXU6CVRpbWUN810gwIHqNDMJOg1uYW5vX251bWkCfgE6DW5hbm9fZGVuaQY6DXN1Ym1pY3JvIgc4IDoJem9uZUkiCFVUQwY7AEY=--22398013c2c7fec898feaece8368cc5c6e1a0117"
EXPIRES_IN = 1209600
# Emily still need to figure out how to extract the expiration date! 

ACCOUNT_ID = "3769859"
BUCKET = "3931991"
VAULT = "602270806"




# ----------------------------------------
# STEP 2: POST FILES TO BASECAMP
#-----------------------------------------

#Try one example first - create a document in Basecamp. 
test_post = POST(paste0("Authorization: Bearer $", ACCESS_TOKEN, " -H Content-Type: application/json -d '{\"title\":\"New Hire Info\",\"content\":\"<div><strong>Getting started</strong></div>\",\"status\":\"active\"}' https://3.basecampapi.com/", ACCOUNT_ID, "/buckets/", BUCKET, "/vaults/", VAULT, "/documents.json"))

POST(paste0("{\"title\" : \"New Hire Info content\" : \"<div><strong>Getting started</strong></div>\" \"status\":\"active\"}"))


test_post = POST(paste0("https://3.basecampapi.com/", ACCOUNT_ID, "/buckets/", BUCKET, "/vaults/", VAULT, "/documents.json"), 
                 config = (token=ACCESS_TOKEN, 
                           add_headers("title": "New Hire Info",
                                      "content": "<div><strong>Getting started</strong></div>",
                                      "status": "active")))

jsonlite::fromJSON(content(test_post, "text"), simplifyVector = FALSE)







# NEW START 
url = paste0("3.basecampapi.com/", ACCOUNT_ID, "/projects.json")
headers = paste0("{ 'Content-Type': 'application/json', 
'User-Agent': 'uploader (elineb@uw.edu)', 
'access_token': '", ACCESS_TOKEN, "', 
'expires_in': '", EXPIRES_IN, "', 
'refresh_token': '", REFRESH_TOKEN, "' }")
headers = gsub("")





