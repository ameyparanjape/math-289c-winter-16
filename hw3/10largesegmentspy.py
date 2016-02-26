import numpy as np
import scipy.stats
import matplotlib.pyplot as plt
plt.style.use('ggplot')
segments = 13
# n_row = 4
n_col = 1
lengh_dna = 229354
# intervals = [4587,5090,5594]
intervals = int(lengh_dna/segments)
# fig,ax = plt.subplots(len(intervals),n_col,sharex=False,sharey=False)

data = np.loadtxt("data.txt",skiprows=1)
# print(data.shape)
num_pal = len(data)
uniform = [296/segments for i in range(segments)]

bins = range(0,lengh_dna,intervals)

n,bins,patchs = plt.hist(data,bins=bins)
plt.hist(uniform,bins=bins)
print(scipy.stats.chisquare(f_obs=n,f_exp=uniform,ddof=0))
print (n)
print(uniform)
n = np.array(n)
uniform = np.array(uniform)

print((n-uniform)/np.sqrt(uniform))
plt.xlabel('Intervals')
plt.ylabel('Counts')
title = 'Interval='+str(intervals)
plt.title(title)


# plt.show()



