import numpy as np
import matplotlib.pyplot as plt


inp = np.loadtxt("input_vectors.txt")
output = np.loadtxt("output_results.txt")
t = np.linspace(0, 2, 400)

n_taps = 4
delay = 0.5 * (n_taps-1) / 400
fact = 1024 / 1e4

plt.plot(t[0:394], fact*output[6:], label='out')
#plt.plot(t, output, label='out')
plt.plot(t, inp/100, label='in')
plt.legend()
plt.show()
