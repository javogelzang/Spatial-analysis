#!/usr/bin/env python
# coding: utf-8

# In[2]:


import pandas as pd
import itertools 
from bs4 import BeautifulSoup
import requests
from requests import get
import time
from random import seed
from random import random
from random import randint
import re


# In[63]:


#url format
url = 'https://www.pararius.com/mietwohnungen/utrecht/mobliert/page-'
# initialize a list 
houses = []
# initialize variable count at 1
count = 1

# fwhile loop that will run # times (number of pages)
while count <= 7:
    # initialize variable new_count at 0
    new_count = 0
    # if loop that specifies the first page separately 
    if count == 1:
        first_page = 'https://www.pararius.com/mietwohnungen/utrecht/mobliert/page-1'
        # request the response
        response = get(first_page)
        # parse through the html 
        html_soup = BeautifulSoup(response.text, 'html.parser')
        # in the html of the page, find all the bins with <li> and class:
        house_data = html_soup.find_all('li', class_="search-list__item search-list__item--listing")
        print(first_page)
        
        # if the response was not empty (if something was actually scraped)
        if house_data != []:
            # add to the list houses
            houses.extend(house_data)
            # random wait times
            value = random()
            scaled_value = 1 + (value * (9 - 5))
            print(scaled_value)
            time.sleep(scaled_value)
    # pages other than the first
    elif count != 1:
    # collect four and wait random times 
        url = 'https://www.pararius.com/mietwohnungen/utrecht/mobliert/page-' + str(count)
        print(url)
        response = get(url)
        html_soup = BeautifulSoup(response.text, 'html.parser')
        print(response)
        house_data = html_soup.find_all('li', class_="search-list__item search-list__item--listing")

        if house_data != []:
            houses.extend(house_data)
            value = random()
            scaled_value = 1 + (value * (9 - 5))
            print(scaled_value)
            time.sleep(scaled_value)

        # stop the loop if empty response
        else:
            print('empty')
            break
            

    count += 1


# In[64]:


## DATA FORMATTING 
## initializing lists and variables
count = 0
house_price = []
rental_agency = []
location = []
city = []
bedrooms = []
surface = []
neighboorhood = []
## how long we are running the while loop for 
n = int(len(houses)) - 1

while count <= n:
    # running the loop through each html bin we scraped
    num = houses[int(count)]
    
    # getting the price: make sure to test this code a few times by itself to understand exactly which parameters will work 
    price = num.find_all('span',{"class":"listing-search-item__price"})[0].text
    house_price.append(price)
    df_price = pd.DataFrame({'Rent':house_price})
    df_price['Rent'] = df_price['Rent'].str.replace("\D","")
    df_price['Rent'] = df_price['Rent'].str.replace("per month","")
    
    # if you have a variable that is not present on all ads the ads, you can use try and except to avoid stopping the loop
    try:
        agency = num.find_all('a', href=True)[2].text
    except IndexError:
        agency = 'none'
    rental_agency.append(agency)
    df_agency = pd.DataFrame({'rental_agency':rental_agency})
    
   #getting the postcode: make sure to test this code a few times by itself to understand exactly which parameters will work 
    postcode = num.find('div',{"class":"listing-search-item__location"}).text
    location.append(postcode)
    df_postcode = pd.DataFrame({'Postcode':location})
    df_postcode['Postcode'] = df_postcode['Postcode'].str.replace("\nApartment\n ","")
    df_postcode['Postcode'] = df_postcode['Postcode'].str.replace("\n","")
    df_postcode['Postcode'] = df_postcode['Postcode'].str.replace("\s","")
    df_postcode['Postcode'] = df_postcode['Postcode'].str.replace("               ","")
    df_postcode['Postcode'] = df_postcode['Postcode'].str.replace("new","")
    df_postcode['Postcode'] = df_postcode['Postcode'].str[0:6]
    
    #Getting the neighboorhood
    neigh = num.find('div',{"class":"listing-search-item__location"}).text
    neigh = re.search(r'\((.*?)\)',neigh).group(1)
    neighboorhood.append(neigh)
    df_neighboorhood = pd.DataFrame({'neighboorhood_name':neighboorhood})
    
    #getting the number of bedrooms: make sure to test this code a few times by itself to understand exactly which parameters will work 
    bedrooms_num = num.find_all('span',{"class":"illustrated-features__description"})[1].text
    bedrooms.append(bedrooms_num)
    df_bedrooms = pd.DataFrame({'Rooms':bedrooms})
    df_bedrooms['Rooms'] = df_bedrooms['Rooms'].str.replace("\D","")
    
    #getting the sq meter size: make sure to test this code a few times by itself to understand exactly which parameters will work 
    size = num.find_all('span',{"class":"illustrated-features__description"})[0].text
    surface.append(size)
    df_surface = pd.DataFrame({'Size':surface})
    df_surface['Size'] = df_surface['Size'].str.replace("\D","")    
    count += 1

# concat all the different dataframes created, culminating in dfa (completed dataframe)
result = pd.concat([df_price, df_agency], axis=1, sort=False)
result2 = pd.concat([result, df_postcode], axis=1, sort=False)
result3 = pd.concat([result2, df_bedrooms], axis=1, sort=False)
result4 = pd.concat([result3, df_neighboorhood], axis=1, sort=False)
dfa = pd.concat([result4, df_surface], axis=1, sort=False)
furnished = dfa
furnished.to_csv('rental_furnished.csv')


# In[113]:


rental = pd.read_csv('rental_new.csv')
postcode = pd.read_csv('utrecht_postcode_v1.csv')


# In[114]:


rental.head()


# In[115]:


postcode.head()


# In[131]:


samen = rental.merge(postcode, on='Zipcode')


# In[ ]:




