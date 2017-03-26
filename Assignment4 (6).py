
# coding: utf-8

# # Assignment 4
# 
# Before working on this assignment please read these instructions fully. In the submission area, you will notice that you can click the link to **Preview the Grading** for each step of the assignment. This is the criteria that will be used for peer grading. Please familiarize yourself with the criteria before beginning the assignment.
# 
# This assignment requires that you to find **at least** two datasets on the web which are related, and that you visualize these datasets to answer a question with the broad topic of **economic activity or measures** (see below) for the region of **Ann Arbor, Michigan, United States**, or **United States** more broadly.
# 
# You can merge these datasets with data from different regions if you like! For instance, you might want to compare **Ann Arbor, Michigan, United States** to Ann Arbor, USA. In that case at least one source file must be about **Ann Arbor, Michigan, United States**.
# 
# You are welcome to choose datasets at your discretion, but keep in mind **they will be shared with your peers**, so choose appropriate datasets. Sensitive, confidential, illicit, and proprietary materials are not good choices for datasets for this assignment. You are welcome to upload datasets of your own as well, and link to them using a third party repository such as github, bitbucket, pastebin, etc. Please be aware of the Coursera terms of service with respect to intellectual property.
# 
# Also, you are welcome to preserve data in its original language, but for the purposes of grading you should provide english translations. You are welcome to provide multiple visuals in different languages if you would like!
# 
# As this assignment is for the whole course, you must incorporate principles discussed in the first week, such as having as high data-ink ratio (Tufte) and aligning with Cairo’s principles of truth, beauty, function, and insight.
# 
# Here are the assignment instructions:
# 
#  * State the region and the domain category that your data sets are about (e.g., **Ann Arbor, Michigan, United States** and **economic activity or measures**).
#  * You must state a question about the domain category and region that you identified as being interesting.
#  * You must provide at least two links to available datasets. These could be links to files such as CSV or Excel files, or links to websites which might have data in tabular form, such as Wikipedia pages.
#  * You must upload an image which addresses the research question you stated. In addition to addressing the question, this visual should follow Cairo's principles of truthfulness, functionality, beauty, and insightfulness.
#  * You must contribute a short (1-2 paragraph) written justification of how your visualization addresses your stated research question.
# 
# What do we mean by **economic activity or measures**?  For this category you might look at the inputs or outputs to the given economy, or major changes in the economy compared to other regions.
# 
# ## Tips
# * Wikipedia is an excellent source of data, and I strongly encourage you to explore it for new data sources.
# * Many governments run open data initiatives at the city, region, and country levels, and these are wonderful resources for localized data sources.
# * Several international agencies, such as the [United Nations](http://data.un.org/), the [World Bank](http://data.worldbank.org/), the [Global Open Data Index](http://index.okfn.org/place/) are other great places to look for data.
# * This assignment requires you to convert and clean datafiles. Check out the discussion forums for tips on how to do this from various sources, and share your successes with your fellow students!
# 
# ## Example
# Looking for an example? Here's what our course assistant put together for the **Ann Arbor, MI, USA** area using **sports and athletics** as the topic. [Example Solution File](./readonly/Assignment4_example.pdf)

# In[1]:

import numpy as np
import pandas as pd
import io
#import html5lib
#import lxml
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats
# Retrieve the webpage as a string
# df_list = pd.read_html("http://en.wikipedia.org/wiki/List_of_countries_by_population")




# In[2]:

df_unemp_AnnArbor = pd.read_csv('ANNAunemployment.csv').dropna()


# In[3]:

df_unemp_AnnArbor.tail()


# In[4]:

df_unemp_Detroit = pd.read_csv('DETROIT_WARREN_LIVONIA_unemployment.csv').dropna()
df_unemp_Detroit.tail()


# In[20]:

get_ipython().magic('matplotlib notebook')
plt.figure()
plt.subplot(2, 1, 1)

observation_dates = list(map(pd.to_datetime, df_unemp_Detroit['DATE']))
plt.plot(observation_dates, df_unemp_Detroit['DETR826URN'])
plt.plot(observation_dates, df_unemp_AnnArbor['ANNA426URN'])


# In[22]:

#plt.xlabel("Date")
plt.ylabel("Unemployment rate %")
plt.legend(["Detroit", "Ann Arbor"])
plt.title("Unemployment comparison between Detroit and Ann Arbor")


# In[23]:

#plt.figure()
plt.subplot(2, 1, 2)

plt.hist(df_unemp_Detroit['DETR826URN'], alpha=0.7, bins=np.arange(0,15,0.5), label='Detroit');
plt.hist(df_unemp_AnnArbor['ANNA426URN'], alpha=0.7, bins=np.arange(0,15,0.5), label='Ann Arbor');
plt.legend();

plt.gca().set_ylabel('Frequency')
plt.gca().set_xlabel('Unemployment rate')
#plt.gca().set_title('Histogram on unemployment rate Detroit and Ann Arbor')


# In[24]:

stats.ttest_ind(df_unemp_Detroit['DETR826URN'],df_unemp_AnnArbor['ANNA426URN'])


# In[25]:

# We can state that the average rate of unemployment of AnnArbor county is structurally lower than the one of Detroit with a 
# 95% confidence level.


# In[26]:

df_Detroit_personal_income = pd.read_csv("DETROIT_WARREN_LIVONIA_personalincome_percapita.csv").dropna()


# In[27]:

df_Detroit_personal_income.tail()


# In[28]:

df_AnnArbor_personal_income = pd.read_csv("ANNARBOR_personalincome_percapita.csv").dropna()
df_AnnArbor_personal_income.tail()


# In[29]:

plt.figure()
observation_dates = list(map(pd.to_datetime, df_AnnArbor_personal_income['DATE']))
plt.plot(observation_dates, df_Detroit_personal_income['DETR826PCPI'])
plt.plot(observation_dates, df_AnnArbor_personal_income['ANNA426PCPI'])


# In[30]:

plt.xlabel("Date")
plt.ylabel("Personal income in USD")
plt.legend(["Detroit", "Ann Arbor"])
plt.title("Personal Income comparison between Detroit and Ann Arbor")


# In[31]:

df_Detroit_personal_income["Change"] = df_Detroit_personal_income.DETR826PCPI.pct_change().dropna()

df_Detroit_personal_income = df_Detroit_personal_income[df_Detroit_personal_income.Change.notnull()]
df_Detroit_personal_income.head()


# In[32]:

df_AnnArbor_personal_income["Change"] = df_AnnArbor_personal_income.ANNA426PCPI.pct_change()
df_AnnArbor_personal_income = df_AnnArbor_personal_income[df_AnnArbor_personal_income.Change.notnull()]
df_AnnArbor_personal_income.head()


# In[33]:

plt.figure()
plt.hist(df_Detroit_personal_income['Change'], alpha=0.7, bins=np.arange(-0.03,0.15,0.005), label='Detroit');
plt.hist(df_AnnArbor_personal_income['Change'], alpha=0.7, bins=np.arange(-0.03,0.15, 0.005), label='Ann Arbor');
plt.legend();

plt.gca().set_ylabel('Frequency')
plt.gca().set_xlabel('Personal income percentual change')
plt.gca().set_title('Histogram on personal income growth Detroit and Ann Arbor')


# In[34]:

stats.ttest_ind(df_Detroit_personal_income['Change'],df_AnnArbor_personal_income['Change'])


# In[35]:

# We cannot state that the average rate of percentual change of personal income of AnnArbor county is structurally higher
# than the one of Detroit with a 95% confidence level. 


# In[36]:

import matplotlib.gridspec as gridspec

plt.figure()
gspec = gridspec.GridSpec(3, 3)

top_histogram = plt.subplot(gspec[0, 1:])
side_histogram = plt.subplot(gspec[1:, 0])
lower_right = plt.subplot(gspec[1:, 1:])


# In[37]:

df_merged = pd.merge(df_AnnArbor_personal_income, df_unemp_AnnArbor, left_on=['DATE'],
              right_on=['DATE'],
              how='inner')

df_merged.head()


# In[38]:


Y = df_merged['Change']
X = df_merged['ANNA426URN']
lower_right.scatter(X, Y)
top_histogram.hist(X, bins=np.arange(0,15,0.5), normed = True)
s = side_histogram.hist(Y, bins=np.arange(-0.05,0.09,0.0025), orientation='horizontal', normed = True)
side_histogram.invert_xaxis()


# In[39]:

grid = sns.jointplot(X, Y, alpha=0.9);


# In[41]:

sns.set_style('white')

sns.jointplot(X, Y, kind='kde', space=0);


# In[42]:

# Apparently there is some negative correlation between unemployment rate and percentual growth of personal income in Ann
# Arbor county.


# In[43]:

print (np.corrcoef(X,Y)[1,0])


# In[44]:

plt.figure()
gspec = gridspec.GridSpec(3, 3)

top_histogram = plt.subplot(gspec[0, 1:])
side_histogram = plt.subplot(gspec[1:, 0])
lower_right = plt.subplot(gspec[1:, 1:])


# In[45]:

df_merged = pd.merge(df_Detroit_personal_income, df_unemp_Detroit, left_on=['DATE'],
              right_on=['DATE'],
              how='inner')

df_merged.head()


# In[46]:

Y = df_merged['Change']
X = df_merged['DETR826URN']
lower_right.scatter(X, Y)
top_histogram.hist(X, bins=np.arange(0,15,0.5), normed = True)
s = side_histogram.hist(Y, bins=np.arange(-0.05,0.09,0.0025), orientation='horizontal', normed = True)
side_histogram.invert_xaxis()


# In[47]:

grid = sns.jointplot(X, Y, alpha=0.9);


# In[48]:

sns.set_style('white')

sns.jointplot(X, Y, kind='kde', space=0);


# In[51]:

# Apparently there is a higher negative correlation between unemployment rate and percentual growth of personal income in 
# Detroit


# In[52]:

print (np.corrcoef(X,Y)[1,0])


# In[ ]:




# In[ ]:



