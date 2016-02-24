import numpy as np
import matplotlib.pyplot as plt
import math
import scipy.stats

def poisson(lamb,k):
	return (lamb**k) * np.exp(-lamb)/math.factorial(k)


def prob_max_hits(lamb,k,m):
	return 1-sum([poisson(lamb,i) for i in range(k)])**m

# n_row = 4
n_col = 2
lengh_dna = 229354
intervals = 5594
n_bins = lengh_dna/intervals
# fig,ax = plt.subplots(len(intervals),n_col,sharex=True,sharey=True)
lamb = 296/n_bins
data = np.loadtxt("data.txt",skiprows=1)
# print(data.shape)	
num_pal = len(data)

random = np.sort(np.random.choice(lengh_dna,num_pal,replace=False))
# # print(random)
# ax.set_xlabel('Intervals')
# ax.set_ylabel('Counts')
# for i in range(20):
# 	print(i,prob_max_hits(lamb,i,3))
# data_counts = []

data_counts = [0,0,1,1,4,5,5,9,8,1,4,1,0,0,1,0,0,0,1]
expected_num = []
lower = 5
upper = 8
for i in range(20):
	expected_num.append(n_bins*poisson(lamb,i))
d_counts = []
e_counts = []

d_counts.append(sum(data_counts [:lower]))
e_counts.append(sum(expected_num[:lower]))

for i in range(lower,upper):
	d_counts.append(data_counts[i])
	e_counts.append(expected_num[i])

d_counts.append(sum((data_counts[upper:])))
e_counts.append(sum((expected_num[upper:])))

d_counts = np.array(d_counts)
e_counts = np.array(e_counts)

print(scipy.stats.chisquare(f_obs=d_counts,f_exp=e_counts,ddof=1))
print(d_counts,e_counts)
# print(sum(np.divide((d_counts-e_counts)**2,e_counts)))
# chi_cal = []
# print(e_counts)

# a = np.array([7,8,10,9,8,5,4,6])
# b= np.array([6.4,7.5,9.7,10,8.6,6.3,4.1,4.5])
# # print(sum(np.divide((b-a)**2,b)))
# print(scipy.stats.chisquare(f_obs=a,f_exp=b,ddof=1))
'''
H0: The distribution is Poisson sactter
p-value = 0.000571


'''
