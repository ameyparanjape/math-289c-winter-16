import numpy as np
import matplotlib.pyplot as plt
plt.style.use('ggplot')

# n_row = 4
n_col = 2
lengh_dna = 229354
intervals = [4587,5090,5594]
intervals = [2000,3000,7000,8000]
fig,ax = plt.subplots(len(intervals),n_col,sharex=True,sharey=True)

data = np.loadtxt("data.txt",skiprows=1)
# print(data.shape)
num_pal = len(data)

random = np.sort(np.random.choice(lengh_dna,num_pal,replace=False))
# # print(random)
# ax.set_xlabel('Intervals')
# ax.set_ylabel('Counts')

for ind,step in enumerate(intervals):
	bins = range(0,lengh_dna,step)[:-1]
	ax[ind][0].hist(data,bins=bins)
	ax[ind][1].hist(random,bins=bins)
	ax[ind][0].set_xlabel('Intervals')
	ax[ind][0].set_ylabel('Counts')
	title = 'Interval='+str(step)+'\nOriginal Data'
	ax[ind][0].set_title(title)


	ax[ind][1].set_xlabel('Intervals')
	ax[ind][1].set_ylabel('Counts')
	title = 'Interval='+str(step)+'\nRandom Generated Data'
	ax[ind][1].set_title(title)

fig.subplots_adjust(hspace=0.5,wspace=0.2)
plt.show()



