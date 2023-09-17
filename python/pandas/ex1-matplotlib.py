#!/usr/bin/env python
## incorrect output due to input data format issues
#    2023-09-17

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd 
    
# making dataframe 
df = pd.read_csv("votes.csv") 

#df = votes.pivot(index='year', columns='party', values='seats').reset_index()
#from votes import wide as df

# Initialise a figure. subplots() with no args gives one plot.
fig, ax = plt.subplots()

# A little data preparation
years = df['year']
x = np.arange(len(years))

width = 1

# Plot each bar plot. Note: manually calculating the 'dodges' of the bars
ax.bar(x - 3*width/2, df['conservative'], width, label='Conservative', color='#0343df')
ax.bar(x - width/2, df['labour'], width, label='Labour', color='#e50000')
ax.bar(x + width/2, df['liberal'], width, label='Liberal', color='#ffff14')
ax.bar(x + 3*width/2, df['others'], width, label='Others', color='#929591')

# Customise some display properties
ax.set_ylabel('Seats')
ax.set_title('UK election results')
ax.set_xticks(x)    # This ensures we have one tick per year, otherwise we get fewer
ax.set_xticklabels(years.astype(str).values, rotation='vertical')
ax.legend()

# Ask Matplotlib to show the plot
plt.show()
