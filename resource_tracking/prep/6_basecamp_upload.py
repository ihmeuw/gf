## Emily Linebarger- adapted from code written by Emily Dansereau and Hannah Kravitz
## Code to automatically upload resource tracking database to Basecamp after each new iteration is saved. 
## Last updated: November 2018

## Works with Python Anaconda package
## Need to have Selenium Python package AND Chrome Driver installed. 
## https://pypi.python.org/pypi/selenium
## https://sites.google.com/a/chromium.org/chromedriver/home

# Set up python 
import getpass # to hide password
import time
from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.support.select import Select

#Get login information
username=input('Enter username: ')
print('Password is hidden')
password = getpass.getpass('Enter password:')
input('Press <ENTER> to continue')

# Define arguments passed from STATA or command
driver = webdriver.Chrome('C:\chromedriver.exe')  # Optional argument, if not specified will search path.

driver.get("https://ihme.datstathost.com/WebUI/"); 

#Enter username and password
userfield = driver.find_element_by_id('UserName')
userfield.send_keys(username)
passwordfield = driver.find_element_by_id('Password')
passwordfield.send_keys(password)
passwordfield.send_keys(Keys.RETURN) # Press Enter
#Press the Login button
#driver.find_element_by_css_selector("button.btn.btn-primary").click()
driver.switch_to_window(driver.window_handles[-1]) # Switch commands to operate in the current window.
title=driver.title

#Open Data Manager
data_manager = driver.find_element_by_id('ctl00_aDataManagerLink').click()
driver.switch_to_window(driver.window_handles[-1]) # Switch commands to operate in the current window.
title = driver.title
driver.switch_to_frame("contentFrame")

#Open "MEXICO"
NIC = driver.find_element(By.PARTIAL_LINK_TEXT, 'PANAMA_KUNA').click() # CHANGE FOR OTHER COUNTRIES
driver.switch_to_window(driver.window_handles[-1]) # Switch commands to operate in the current window.
title = driver.title
driver.switch_to_frame("contentFrame")
time.sleep(5)


# Close browser
time.sleep(5) # Wait 5 seconds so the files can finish DLing
driver.quit();