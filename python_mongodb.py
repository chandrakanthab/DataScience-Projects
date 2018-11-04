
# coding: utf-8

# In[8]:


#Load libraries
import pandas as pd
import os
import pymongo as pymongo
from pymongo import MongoClient
import pprint
import json


# In[5]:


# Install pymongo library\bbn",
get_ipython().system(u'pip install pymongo')


# In[12]:


#Set working directory
os.chdir("D:\Edwisor assignments")
#getting wroking directory
os.getcwd()


# In[13]:


#connect Mongo DB through Port
Channel = MongoClient(port=27017)


# In[15]:


#select DB edwisor
Select_db = Channel.edwisor


# In[16]:



#list all collections in db
Collection_Names = Select_db.collection_names(include_system_collections = False)
print(Collection_Names)


# In[19]:


#select Edwisor Collection
Select_collection = Select_db.edwisor_assign
print(Select_collection.find_one())


# In[20]:


#created Student list of Edwisor
student_list = [
  {"ID":104 ,"name": "Amy", "Course": "Data Science"},
  { "ID":105,"name": "Hannah", "Course": "Full Stack Developer"},
  { "ID":106, "name": "Michael", "Course": "Mean Stack Developer"}
  
]


# In[22]:


#inser Records in the Edwiror_assign in Mongo DB

x = Select_collection.insert_many(student_list)
# Print Inserted ID's
#print(x.inserted_ids)

