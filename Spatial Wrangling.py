#!/usr/bin/env python
# coding: utf-8

# In[1]:


import pandas as pd
import geopandas as gpd
import os 
import numpy as np
from collections import Counter
import time
from sklearn import preprocessing
import matplotlib.pyplot as plt
from shapely.geometry import Point
from shapely.geometry.polygon import Polygon
import shapely.wkt

os.chdir('/Users/jaspervogelzang/Documents/ADS Master/Spatial Statistics/Project/Data')
rental_shp = gpd.read_file('rental_central.shp')
venues_shp = gpd.read_file('utrecht_venues.shp')
rental_new = pd.DataFrame(gpd.read_file('rental_complete.shp'))


# In[4]:


def rescue_code(function):
    import inspect
    get_ipython().set_next_input("".join(inspect.getsourcelines(function)[0]))


# In[94]:


#Subset on category
train_stations = venues_shp.loc[venues_shp['Venue Cate'] == 'Train Station']
restaurant = venues_shp.loc[venues_shp['Venue Cate'] == 'Restaurant']
bus = venues_shp.loc[venues_shp['Venue Cate'] == 'Bus Stop']
supermarket = venues_shp.loc[venues_shp['Venue Cate'] == 'Supermarket']
park = venues_shp.loc[venues_shp['Venue Cate'] == 'Park']
school = venues_shp.loc[venues_shp['Venue Cate'] == 'School']

#Delete duplicates
train_stations = train_stations.drop_duplicates(subset='geometry')
restaurants = restaurants.drop_duplicates(subset='geometry')
bus = bus.drop_duplicates(subset='geometry')
supermarket = supermarket.drop_duplicates(subset='geometry')
park = park.drop_duplicates(subset='geometry')
school = school.drop_duplicates(subset='geometry')


# In[107]:


rental_shp = rental_shp.iloc[1:15,]
rental_shp


# In[135]:


def measure_distance(A, B):
    dist = []
    for k in range(len(A)):
        rel = []
        pnt1 = A.iloc[k,13]
        
        for i in range(len(B)):
            pnt2 = B.iloc[i,6]
            points_df = gpd.GeoDataFrame({'geometry': [pnt1, pnt2]}, crs='EPSG:4326')
            points_df = points_df.to_crs('EPSG:5234')
            points_df2 = points_df.shift() 
            distance = points_df.distance(points_df2)[1]
            rel.append(distance)
        dist.append(min(rel))
    dist = np.around(dist,1)    
    return dist
            
rental_shp['train_dist'] = measure_distance(rental_shp, train_stations)
rental_shp['rest_dist'] = measure_distance(rental_shp, rent_dist)
rental_shp['bus_dist'] = measure_distance(rental_shp, bus_dist)
rental_shp['supermarket_dist'] = measure_distance(rental_shp, supermarket)
rental_shp['park_dist'] = measure_distance(rental_shp, park)
rental_shp['school_dist'] = measure_distance(rental_shp, school)


# In[136]:


rental_shp

