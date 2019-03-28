#--------------------------------------------------------------
# PURPOSE: Validate final datasets, and upload to Basecamp
# AUTHOR: Emily Linebarger 
# DATE: March 2019. 
# -------------------------------------------------------------

#Make sure all variables are of the type expected, and have the values you would expect. 

#Make sure there are no NAs where you're not expecting them. 

# Make sure you have all the columns you expect, and that all variables are represented in the codebook. 


#--------------------------------------------------------------
# AUTOMATICALLY UPLOAD FILES TO BASECAMP
#--------------------------------------------------------------


system("curl -H \"Authorization: Bearer $ACCESS_TOKEN\" ")
system("-H 'Content-Type: application/json' ")
system("-H 'User-Agent: Global Fund PCE (elineb@uw.edu)'")
system("-d '{ 'name': 'My new project!' }'")
system("https://3.basecampapi.com/999999999/projects.json")


#GLOBAL LEVEL PCE FILES 
#Example curl for uploading a document
system("curl -H 'Authorization: Bearer $ACCESS_TOKEN' \
       -H 'Content-Type: application/json' \
       -H 'User-Agent: MyApp (yourname@example.com)' \
       -d '{ 'name': 'My new project!' }' \
       https://3.basecampapi.com/999999999/projects.json")