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
#from selenium.webdriver.support.select import Select

#Get login information
email_address=input('Enter UW email address ')
print('Password is hidden')
password = getpass.getpass('Enter Basecamp password (UW password)')
input('Press <ENTER> to continue')

# Define arguments passed from STATA or command
driver = webdriver.Chrome('C:\chromedriver.exe')  # Optional argument, if not specified will search path.

#--------------------------------
#       Set up filepaths 
#-------------------------------
drc_final_expenditures = "https://3.basecamp.com/3769859/buckets/4025874/uploads/1560083744"
drc_final_budgets = "https://3.basecamp.com/3769859/buckets/4025874/uploads/1560083487"


#--------------------------------
#               DRC 
#-------------------------------
driver.get("https://3.basecamp.com/3769859/buckets/4025874/vaults/1117008221"); 

#Enter username and password- Select Google sign in 
driver.find_element_by_xpath('/html/body/div/div[1]/div[1]/form[1]/button/img').click()
email = driver.find_element_by_id('identifierId')
email.send_keys(email_address)
driver.find_element_by_xpath('//*[@id="identifierNext"]/content/span').click()
userfield = driver.find_element_by_id('weblogin_netid')
passwordfield = driver.find_element_by_id('weblogin_password')
userfield.send_keys(email_address)
passwordfield.send_keys(password)
passwordfield.send_keys(Keys.RETURN) # Press Enter

#Get past Chrome checking if we're human or not, which we DEFINITELY are... 
driver.find_element_by_xpath('//*[@id="view_container"]/div/div/div[2]/div/div[2]/div/div[1]/div/content/span').click()
print("Yay! We're DEFINITELY human. ;)")
#Press the Login button
#driver.find_element_by_css_selector("button.btn.btn-primary").click()
driver.switch_to_window(driver.window_handles[-1]) # Switch commands to operate in the current window.
title=driver.title

#Open Total Resource Tracking Data 
total_resource = driver.find_element_by_id('recording_1117008945').click()
driver.switch_to_window(driver.window_handles[-1]) # Switch commands to operate in the current window.
title = driver.title
driver.switch_to_frame("contentFrame")

#Select "Upload a new version" 
driver.find_element_by_xpath('//*[@id="recording_1117008945"]/div/header/div[2]/a[2]').click()
driver.find_element_by_xpath('//*[@id="main-content"]/div/section/div[2]/div[2]/div/div/div/button').click()
driver.send_keys()


# Close browser
time.sleep(5) # Wait 5 seconds so the files can finish DLing
driver.quit();