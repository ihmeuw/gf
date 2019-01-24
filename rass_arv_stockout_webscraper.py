## Emily Linebarger- adapted from code by Emily Dansereau and Hannah Kravitz
## Code to automatically download data from RASS ARV stockouts dashboards
## January 2019

## Works with Python Anaconda package
## Need to have Selenium Python package AND Chrome Driver installed. 
## https://pypi.python.org/pypi/selenium
## https://sites.google.com/a/chromium.org/chromedriver/home

# Import packages
import time
from selenium import webdriver
from bs4 import BeautifulSoup
import urllib.request
import pandas as pd
#from selenium.webdriver.common.keys import Keys
#from selenium.webdriver.common.by import By
#from selenium.webdriver.support.select import Select


#Build up a list of searchable web addreses here that will automatically take you to the Dashboards you want. 
#As an example, this web address represents Week 1 for Bulissa district at the level of the district. 
#http://rass.mets.or.ug/?o=CQTmrrriwOq&w=2019W1&wn=1&on=Buliisa%20District&ol=District&cw=2019W1

#Break this URL into parts so you can run for any part of the data. 
main_url = "http://rass.mets.or.ug/"
week = "2019W1" 
admin_level = "District" 
current_week="2019w3" #CW means current week I think? 

#Not sure what the wn=1 argument means here. 



#-------------------------------------------------
# Write an example script for one observation. 
#-------------------------------------------------
#target_page = 'http://rass.mets.or.ug/?o=CQTmrrriwOq&w=2019W1&wn=1&on=Buliisa%20District&ol=District&cw=2019W3'
#page = urllib.request.urlopen(target_page)
#soup = BeautifulSoup(page, 'html.parser')


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

# 1. 
driver = webdriver.Chrome('C:\chromedriver.exe')
driver.get('http://rass.mets.or.ug/?o=CQTmrrriwOq&w=2019W1&wn=1&on=Buliisa%20District&ol=District&cw=2019W3')
driver.find_element_by_xpath('//*[@id="stock_wrapper"]/div[1]/button[2]/span').click()

#2, 3, and 4
commodities = pd.read_csv("C:/Users/elineb/Downloads/MoH Uganda - Realtime ARV Stock Status Monitoring Dashboard (9).csv") #Emily change this URL. 
commodities.insert(0, 'row_number', range(1, 1+len(commodities)))
commodities = commodities.set_index(['Commodity', 'Category', 'row_number'])
commodities = commodities.stack()

#Can you add a column here to give an index to each drug? 
commodities = commodities[commodities != 0] #This is the dataset that should guide you for step 5

#5. 
#<a href="#" class="show-hfs" data-cat="Adult" data-col="en" data-toggle="modal" data-target="#modal-hfs" data-backdrop="static" data-keyboard="false">6</a>

#First 3 columns have the same format for selectors. 
#//*[@id="stock"]/tbody/tr[1]/td[3]/a
#//*[@id="stock"]/tbody/tr[4]/td[4]/a


#Build up an x-path selector, with column 1 = Under, 2 = Adequate, 3 = Over, and 4 = Stock Outs. 
#Keep 0's in the 

#Another way to do this would be to find all the places where href="#" and the display is not 0. 
#-------------------------------------------------
# Expand the list above to include all facilities for all weeks. 
#-------------------------------------------------




#page = urllib2.urlopen(quote_page)
#soup = BeautifulSoup(page, ‘html.parser’)

districts = {
        "Buliisa District":"CQTmrrriwOq", 
#        "Hoima District":"PJFtfCyp6Rb",
#        "Kagadi District":"LtyM5HnzFui",
#        "Kakumiro District":"HRakdY52JPf",
#        "Kibaale District":"AtnLKczpkvP",
#        "Kikuube District":"fXT6ayIyYeH",
#        "Kiryandongo District":"B0G9cqixld8",
#        "Masindi District":"xr8EMirOASp"
        }

for district in districts:
    #Create a combined URL for each district to access their API for the given week. 
    url = main_url + "?o=" + districts[district] + "&w=" + week + "&wn=1" + "&on=" + district + "&ol=" + admin_level + "&cw=" + current_week; 
    driver.get(url); 
    time.sleep(5); #Give it a second to load. 
    
    #Pull each of the five dashboards. 
    downloads = driver.find_elements_by_class_name("highcharts-menu-item");
    for download in downloads:
        print(download); 
        download.click(); 
        time.sleep(2); 


driver.get("http://rass.mets.or.ug/?o=CQTmrrriwOq&w=2019W1&wn=1&on=Buliisa%20District&ol=District&cw=2019W3"); 

driver.switch_to_window(driver.window_handles[-1]) # Switch commands to operate in the current window.
title = driver.title
driver.switch_to_frame("contentFrame")

time.sleep(5);
driver.get("http://rass.mets.or.ug/?o=LtyM5HnzFui&w=2019W1&wn=1&on=Kagadi%20District&ol=District&cw=2019W3"); 
time.sleep(5);


##driver.find_element_by_css_selector("button.btn.btn-primary").click()
#driver.switch_to_window(driver.window_handles[-1]) # Switch commands to operate in the current window.
#title=driver.title
#
##Open Data Manager
#data_manager = driver.find_element_by_id('ctl00_aDataManagerLink').click()
#driver.switch_to_window(driver.window_handles[-1]) # Switch commands to operate in the current window.
#title = driver.title
#driver.switch_to_frame("contentFrame")
#
#
## Close browser
#time.sleep(5) # Wait 5 seconds so the files can finish DLing
#driver.quit();