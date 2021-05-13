import numpy as np
import statistics
from scipy.stats import t
from scipy.stats import norm
from scipy.stats import chi2
import math


def ci_mean(data, gamma, var=-1):
	mu = statistics.mean(data)
	g = (1+gamma)/2

	if var == -1:
		if len(data) >= 2:
			S = statistics.stdev(data)
		else:
			S = data[0]
		return [mu - (t.ppf(g, df=len(data)-1)*(S/math.sqrt(len(data)))), mu + (t.ppf(g, df=len(data)-1)*(S/math.sqrt(len(data))))]

	return [mu - (norm.ppf(g)*(math.sqrt(var/len(data)))), mu + (norm.ppf(g)*(math.sqrt(var/len(data))))]

def ci_variance(data, gamma, shortest=True):
	mu = statistics.mean(data)
	S = statistics.stdev(data)
	g_l = (1+gamma)/2
	g_u = (1-gamma)/2
	c = (len(data) - 1)*(S**2)
	if shortest:
		return [0, (c/chi2.ppf(1-gamma, df=len(data)-1))]
	return [(c/chi2.ppf(g_l, df=len(data)-1)), (c/chi2.ppf(g_u, df=len(data)-1))]

def p_value(data, h_0, var=-1):
	mu = statistics.mean(data)
	
	if var == -1:
		if len(data) >= 2:
			S = statistics.stdev(data)
		else:
			S = data[0]
		num = mu - h_0
		denom = (S**2)/len(data)
		z = abs(num/(math.sqrt(denom)))
		return 2*(1-t.cdf(z, df=len(data)-1))

	num = mu - h_0
	denom = var/len(data)
	z = num/(math.sqrt(denom))
	return 2*(1-norm.cdf(abs(z)))