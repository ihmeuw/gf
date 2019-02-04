## Emily Linebarger- adapted from code by Emily Dansereau and Hannah Kravitz
## Code to automatically download data from RASS ARV stockouts dashboards
## January 2019

## Works with Python Anaconda package
## Need to have Selenium Python package AND Chrome Driver installed. 
## https://pypi.python.org/pypi/selenium
## https://sites.google.com/a/chromium.org/chromedriver/home

#Pseudo code: 
# 1. Navigate to the table you want. There are two you can pull, let's just do the first one as an example. 
# 2. Download the CSV of the drug data. 
# 3. Import this .csv back into Python, and drop all of the rows with 0's. 
# 4. Reshape the data long, so the number of rows of the data represents the number of clicks you should make to get the facility level data. 
# 5. Navigate to these places on the embedded table, and click to expose the .csv of facilities. 
# 6. Download this .csv of facilities, and import it into Python 
# 7. Reshape the second .csv wide, and append it to the first .csv so each row has the facility names with stock outs. 
# 8. Add on week, district, region, and facility name variables. 
# 9. Remove 2 csvs from downloads so links don't break. 

#---------------------------------------------------------------
# Import packages
#---------------------------------------------------------------
import time
from selenium import webdriver
import pandas as pd
import os
import sys

#---------------------------------------------------------------
# Set up main variables for the website.
#---------------------------------------------------------------
# detect if operating on windows or on the cluster 
j = 'J:'
if sys.platform != 'win32':
    j = '/home/j'

main_url = "http://rass.mets.or.ug/"
save_loc = j + "/Project/Evaluation/GF/outcome_measurement/uga/rass_arv_dashboards/raw/"
admin_level = "District" 
current_week="2019W4" #Change to match the current week, but it won't break the code either way. 
download_link = "C:/Users/elineb/Downloads/MoH Uganda - Realtime ARV Stock Status Monitoring Dashboard.csv" #This is the same for all dashboards. 
overwrite_data = "no" #Make this 'yes' if you want to write over data you've pulled before. (Make it any other string, but probably 'no', if you don't want to.)

#Potentially new URL? 
#http://rass.mets.or.ug/?category=stocks&option=stock_status
#---------------------------------------------------------------
# Start your chrome driver!
#---------------------------------------------------------------
driver = webdriver.Chrome('C:\chromedriver.exe')

#---------------------------------------------------------------
#Set the names of the districts and weeks you want to pull here. 
#---------------------------------------------------------------
districts = {
       # "Buliisa District":"CQTmrrriwOq", 
        #"Kampala District":    "rzsbhKKYISq" ,
        #"Hoima District":"PJFtfCyp6Rb",
        #"Kagadi District":"LtyM5HnzFui",
        #"Kakumiro District":"HRakdY52JPf",
        #"Kibaale District":"AtnLKczpkvP",
        #"Kikuube District":"fXT6ayIyYeH",
        #"Kiryandongo District":"B0G9cqixld8",
#        "Masindi District":"xr8EMirOASp",
#        "Kiboga District":     "GLHh0BXys9w" ,
#        "Kyankwanzi District": "IVuiLJYABw6" ,
#        "Luweero District":    "tr9XWtYsL5P" ,
#        "Mityana District":    "Q7PaNIbyZII" ,
#        "Mubende District":    "lzWuB6bCQeV" ,
#        "Nakaseke District":   "oSbgVKaeCP0" ,
#        "Nakasongola District":"hUcYGQCK9ub" ,
#        "Budaka District":     "kb7iUQISRlx" ,
#        "Bududa District":     "AhwgeZQYj16" ,
#        "Bukwo District":      "e8m9ZYMRoeR" ,
#        "Butaleja District":   "MtpE3CH6vq3" ,
#        "Kibuku District":     "Oyxwe3iDqpR" ,
#        "Kotido District":     "aPZzL4CyBTg" ,
#        "Manafwa District":    "JIZDvNlIhXS" ,
#        "Mbale District":      "yuo5ielNL7W" ,
#        "Moroto District":     "A9kRCvmn6Co" ,
#        "Namisindwa District": "s3X2onnu3iE" ,
#        "Pallisa District":    "WiVj4bEhX4P" ,
#        "Sironko District":    "wJ2a6YKDFZW" ,
#        "Tororo District":     "KhT80mlwJ3Y" ,
#        "Wakiso District":     "aIahLLmtvgT" ,
#        "Bukomansimbi District":"Ame30QOwuX6",
#        "Butambala District":  "a3LMKP8z8Xj" ,
#        "Gomba District":      "lQj3yMM1lI7" ,
#        "Kalangala District":  "JrHILmtK0OU" ,
#        "Kalungu District":    "ahyi8Uq4vaj" ,
#        "Kyotera District":    "UcOzqLVFJVo" ,
#        "Lwengo District":     "bqpd0Y9eXZ2" ,
#        "Lyantonde District":  "ePRNSGUR3vk" ,
        #"Masaka District":     "bIONCoCnt3Q" ,
        #"Mpigi District":      "bFlqjkzbC8N" ,
        #"Rakai District":      "P8iz90eiIrW" ,
        "Sembabule District":  "j7AQsnEYmvi" ,
        "Bundibugyo District": "g8M1cWRJZV6" ,
        "Bunyangabu District": "uAUn3ZcQ6Kt" ,
        "Kabarole District":   "fIbu0dVl0gz" ,
        "Kamwenge District":   "VbX669lGEiY" ,
        "Kasese District":     "fa8xVDzSpte" ,
        "Kyegegwa District":   "g9tQqo1rSj7" ,
        "Kyenjojo District":   "O9MoQcpZ4uA" ,
        "Ntoroko District":    "z8D9ER36EKN" ,
        "Amuria District":     "TM6ccNxawqy" ,
        "Bukedea District":    "tdZbtg9sZkO" ,
        "Kaberamaido District":"QoRZB7xc3j9" ,
        "Katakwi District":    "cSrCFjPKqcG" ,
        "Kumi District":       "saT18HClZoz" ,
        "Ngora District":      "hj4hsYK3dVm" ,
        "Serere District":     "zJfpujxC1kD" ,
        "Soroti District":     "srmGjHrpVE5" ,
        "Adjumani District":   "QYiQ2KqgCxj" ,
        "Arua District":       "WcB3kLlgRTb" ,
        "Koboko District":     "iqsaGItA68C" ,
        "Maracha District":    "WyR8Eetj7Uw" ,
        "Moyo District":       "W0kQBddyGyh" ,
        "Nebbi District":      "Xjc0LDFa5gW" ,
        "Pakwach District":    "StgiDC6czID" ,
        "Yumbe District":      "W1JM2Qdhcv3" ,
        "Zombo District":      "A4aGXEfdb8P" ,
        }

patient_levels = {
        "adult": '//*[@id="app"]/article/div/div/div[1]/ul/li[2]',
        "pediatric": '//*[@id="app"]/article/div/div/div[1]/ul/li[2]',
        "rapid_test_kits": '//*[@id="app"]/article/div/div/div[1]/ul/li[3]'
        }

weeks = list()
for year in {2018, 2017, 2016}:
    for week in range(52): #Just do something for now that we know we should have data for. 
        week_string = str(year) + "W" + str(week+1)
        weeks.append(week_string)
        
#Make sure you've removed the key downloads file, so you know you're saving the right data. 
if os.path.isfile(download_link):
    os.remove(download_link)
    
#---------------------------------------------------------------
# For the districts and weeks specified above, pull the list of facilities
#   that have information on ARV stocks. 
#---------------------------------------------------------------        
for district in districts:
    for week in weeks:
    
        #Set up the save directory for this raw data 
        final_dir = save_loc + district + "/"
        file = final_dir + str(week) + ".csv"
        
        #Check to make sure you haven't downloaded this data before! 
        if os.path.isfile(file) and overwrite_data != "yes":
            print("This data has already been downloaded. Skipping " + str(week) + " for " + str(district))
        else: 
            print("Downloading " + str(district) + " " + str(week))
        
            #Create a URL to access the given week for the given district (accessing API back end)
            url = main_url + "?o=" + districts[district] + "&w=" + week + "&wn=" + week[5:len(week)] + "&on=" + district + "&ol=" + admin_level + "&cw=" + current_week; 
            driver.get(url); 
            time.sleep(5); #Give it a second to load. 
            
            #Break this down by adult, pediatric, and RTKS. 
            aggregated_week = pd.DataFrame()
            for level in patient_levels:
                #Specify the level that you want 
                driver.find_element_by_xpath(patient_levels[level]).click()
                time.sleep(3)
                
                # Download the first table, the stock status for HIV communities, to get which rows and columns have valid data (not 0).
                driver.find_element_by_xpath('//*[@id="stock_wrapper"]/div[1]/button[2]/span').click()
                time.sleep(5)
                
                # Reimport into Python, and only keep the rows that have working links, and valid facility data (value is not 0). 
                commodities = pd.read_csv(download_link) 
                print(len(commodities))
                #Remove the commodities file from your downloads folder so you can pull in other files! 
                os.remove(download_link)
                
                # Reshape the data long, so the number of rows of the data represents the number of clicks you should make to get the facility level data. 
                commodities = pd.melt(commodities, id_vars=['Commodity', 'Category', '#Clients', '#Clients at risk'], value_vars = ['#Under', '#Adequate', '#Over', '#StockOuts'])
                commodities = commodities.dropna(axis=0) #This is the dataset that should guide you for step 5
                commodities = commodities[commodities['value']!=0]
                
                #Make sure you've got some data at this point. 
                print(commodities.head())
                
                #Only attempt to grab data if you have some valid rows. 
                if(len(commodities)!=0): 
                    print("Commodity data found...")

                    #Rename some column names and reset the indices so it's formatted nicely. 
                    commodities.columns = ['commodity', 'category', 'clients', 
                                           'clients_at_risk', 'status', 'num_facilities']
                    commodities.reset_index(inplace=True)
                    #Append the files at the drug/status level to create one week-level file
                    if (len(aggregated_week)==0):
                        aggregated_week = commodities
                    else:
                        aggregated_week = aggregated_week.append(commodities)
    
                    time.sleep(2)
                        
                    #Navigate back to the URL you were working on so you can keep going!
                    driver.get(url)
                    time.sleep(5)
               
                else:
                    print("No facility-level data found for " + str(district) + " " + str(week))
            
            #Save this file on the J:drive in the raw data folder 
            if not os.path.exists(final_dir): #In case you haven't saved this data before, make sure the path you want to save it at exists!
                os.makedirs(final_dir)
                
            #Write aggregated file
            aggregated_week.to_csv(file)


#---------------------------------------------------------------
# For 2019 data, take a different approach, and also grab the 
#   facility names.       #EMILY STILL NEED TO RE-RUN THIS AND GRAB ADULT, KID, AND RAPID TEST DATA! JUST HAVE ADULT RIGHT NOW.           
#---------------------------------------------------------------  
                
weeks = list()
for year in {2019}:
    for week in range(5): #Just do something for now that we know we should have data for. 
        week_string = str(year) + "W" + str(week+1)
        weeks.append(week_string)
        
#Make sure you've removed the key downloads file, so you know you're saving the right data. 
if os.path.isfile(download_link):
    os.remove(download_link)
    
#---------------------------------------------------------------
# For the districts and weeks specified above, pull the list of facilities
#   that have information on ARV stocks. 
#---------------------------------------------------------------        
     
for week in weeks:

    #Set up the save directory for this raw data 
    final_dir = save_loc + "National/"
    file = final_dir + str(week) + "facility_level_.csv"
    
    #Check to make sure you haven't downloaded this data before! 
    if os.path.isfile(file) and overwrite_data != "yes":
        #print("This data has already been downloaded. Skipping " + str(week) + " for " + str(district))
        print("This data has already been downloaded. Skipping " + str(week))
    else: 
        #print("Downloading " + str(district) + " " + str(week))
        print("Downloading " + str(week))
    
        #Create a URL to access the given week for the given district (accessing API back end)
        #url = main_url + "?o=" + districts[district] + "&w=" + week + "&wn=" + week[5:len(week)] + "&on=" + district + "&ol=" + admin_level + "&cw=" + current_week; 
        #http://rass.mets.or.ug/?o=akV6429SUqu&w=2019W4&wn=4&on=Uganda&ol=National&cw=2019W5
        url = main_url + "?o=akV6429SUqu" + "&w=" + week + "&wn=" + week[5:len(week)] + "&on=Uganda" + "&ol=National" + "&cw=" + current_week; 
        driver.get(url); 
        time.sleep(5); #Give it a second to load. 
    
        # Download the first table, the stock status for HIV communities, to get which rows and columns have valid data (not 0).
        driver.find_element_by_xpath('//*[@id="stock_wrapper"]/div[1]/button[2]/span').click()
        time.sleep(5)
        
        # Reimport into Python, and only keep the rows that have working links, and valid facility data (value is not 0). 
        commodities = pd.read_csv(download_link) 
        print(len(commodities))
        #Remove the commodities file from your downloads folder so you can pull in other files! 
        os.remove(download_link)
        commodities.insert(0, 'row_number', range(1, 1+len(commodities))) #Add a number to match the row number in the embedded table in the website to make an xpath later.
        
        # Reshape the data long, so the number of rows of the data represents the number of clicks you should make to get the facility level data. 
        commodities = pd.melt(commodities, id_vars=['Commodity', 'Category', 'row_number', '#Clients', '#Clients at risk'], value_vars = ['#Under', '#Adequate', '#Over', '#StockOuts'])
        commodities = commodities.dropna(axis=0) #This is the dataset that should guide you for step 5
        commodities = commodities[commodities['value']!=0]
        
        #Make sure you've got some data at this point. 
        print(commodities.head())
        
        #Use this dictionary to build up an x-path (corresponds to embedded table in website)
        col_number = {
            '#Under': '3',
            '#Adequate': '4',
            '#Over': '5',
            '#StockOuts': '6'
        }
        
        #Only attempt to grab data if you have some valid rows. 
        if (len(commodities)!=0): 
            print("Commodity data found, pulling facility data...")
            
            #Assign a new column to the data frame giving the column number of the embedded table in the website. 
            #Making this slightly less code using the dictionary above. 
            for number in col_number:
                commodities.loc[commodities['variable'] == number, 'col_number'] = col_number[number]
                
            #Rename some column names and reset the indices so it's formatted nicely. 
            commodities.columns = ['commodity', 'category', 'row_number', 'clients', 
                                   'clients_at_risk', 'status', 'num_facilities', 'col_number']
            commodities.reset_index(inplace=True)
            
            if not os.path.exists(final_dir): #In case you haven't saved this data before, make sure the path you want to save it at exists!
                os.makedirs(final_dir)
            commodities.to_csv(final_dir + str(week) + "commodities_overview_.csv")
            
            aggregated_week = pd.DataFrame()
            #Navigate to this xpath, which represents a list of facilities, and click. Download this .csv to the J:drive. 
            for index, row in commodities.iterrows(): #EMILY THIS ONLY SHOWS ROWS 1-10 AT A TIME. IF YOU GO OVER 10 YOU NEED TO CLICK THE NEXT BUTTON. 
                print(str(index) + " " + str(row['commodity']) + " " + str(row['status'])) #Have it tell you where you are. 
                
                #Only 10 observations are shown at a time in the table. So, if you have a row number over 10, you need to click 'next'. 
                while (row['row_number'] > 10):
                    row['row_number'] -= 10
                    driver.find_element_by_xpath('//*[@id="stock_next"]').click()
                    time.sleep(3)
                
                 #Format a list of xpaths you need to click given the data frame above, which tells you where you have valid facilities. 
                 #Build a string for the xpath of a list of facilities under each of the four types listed in the 'col_number' dictionary above.
                row['xpath'] = '//*[@id="stock"]/tbody/tr[' + str(row["row_number"]) + ']/td[' + str(row["col_number"]) + ']/a'
                driver.find_element_by_xpath(row['xpath']).click()
                time.sleep(3)
                driver.find_element_by_xpath('//*[@id="hf-list_wrapper"]/div[1]/button[2]/span').click()
                time.sleep(5)
                
                #Save this file on the J:drive in the raw data folder 
                facilities = pd.read_csv(download_link)
                
                #Subset to only the row you want. 
                facilities = facilities[['Health Facility', 'Sub County', 'District', 'Region']]
                facilities.columns = ['health_facility', 'sub_county', 'district', 'region']
                
                # Add the drug and status this row was targeting to the facilities file. 
                # This should equal the row number and column number from the facilities table. 
                facilities['status'] = row['status']
                facilities['commodity'] = row['commodity']
                facilities['week'] = week
                facilities['clients'] = row['clients']
                facilities['clients_at_risk'] = row['clients_at_risk']
                facilities['num_facilities'] = row['num_facilities']
                
                #Append the files at the drug/status level to create one week-level file
                if index == 0:
                    aggregated_week = facilities
                else:
                    aggregated_week = aggregated_week.append(facilities)
                    
                os.remove(download_link) #Remove from your downloads folder. 
                time.sleep(2)
                
                #Navigate back to the URL you were working on so you can keep going!
                driver.get(url)
                time.sleep(5)
                
            #Write the aggregated week file to the filepath above
            aggregated_week.to_csv(file) 
            time.sleep(3)
       
        else:
            print("No facility-level data found for " + str(week))
