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
#from selenium.webdriver.common.keys import Keys
#from selenium.webdriver.common.by import By
#from selenium.webdriver.support.select import Select

# Define arguments passed from STATA or command
driver = webdriver.Chrome('C:\chromedriver.exe')  # Optional argument, if not specified will search path.

#Build up a list of searchable web addreses here that will automatically take you to the Dashboards you want. 
#As an example, this web address represents Week 1 for Bulissa district at the level of the district. 
#http://rass.mets.or.ug/?o=CQTmrrriwOq&w=2019W1&wn=1&on=Buliisa%20District&ol=District&cw=2019W1

#Break this URL into parts so you can run for any part of the data. 
main_url = "http://rass.mets.or.ug/"
week = "2019W1" 
admin_level = "District" 
current_week="2019w3" #CW means current week I think? 

#Not sure what the wn=1 argument means here. 

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
#


#highcharts-xjkdw6e-4 > svg > g.highcharts-button.highcharts-contextbutton.highcharts-button-normal > path

#//*[@id="highcharts-xjkdw6e-4"]/svg/g[6]/path
#time.sleep(5); 
#driver.get("http://rass.mets.or.ug/?o=PJFtfCyp6Rb&w=2019W1&wn=1&on=Hoima%20District&ol=District&cw=2019W3"); 
#//*[@id="highcharts-tv9vxep-4"]/svg/g[6]/path
#
#//*[@id="highcharts-tv9vxep-4"]/div/div/div[6]
#
#
##New graph: 
#//*[@id="highcharts-tv9vxep-6"]/div/div/div[6]
#
##New graph: 
#//*[@id="highcharts-tv9vxep-0"]/div/div/div[6]
#
##New graph: (CSV button already displaying) 
#//*[@id="stock_wrapper"]/div[1]/button[2]
#
##New graph: 
#//*[@id="orgsmry_wrapper"]/div[1]/button[2]/span

time.sleep(5);
driver.get("http://rass.mets.or.ug/?o=LtyM5HnzFui&w=2019W1&wn=1&on=Kagadi%20District&ol=District&cw=2019W3"); 
time.sleep(5);

##Enter username and password
#userfield = driver.find_element_by_id('UserName')
#userfield.send_keys(username)
#passwordfield = driver.find_element_by_id('Password')
#passwordfield.send_keys(password)
#passwordfield.send_keys(Keys.RETURN) # Press Enter
##Press the Login button
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