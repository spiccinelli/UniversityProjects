import numpy as np
import matplotlib.pyplot as plt
from scipy import signal

def sine_wave(A, time, f): # creates a sine wave
    return A * np.sin(2 * np.pi * f * time)

f1, f2, f3 = 40, 280, 320
A1, A2, A3 = 80, 40, 30
t = np.linspace(0, 1, 800) # f* waveform sampled at 200 Hz for 2s
in_signal_sin = sine_wave(A1, t, f1) + sine_wave(A2, t, f2) + sine_wave(A3, t, f3) # in order to create some noise

t_sq = np.linspace(0, 1, 400)
in_signal_sq = 126*signal.square(2 * np.pi * 5 * t_sq) # 5Hz waveform sampled at 200 Hz for 2s

inv = []
for i in range(0, len(in_signal_sq)): 
    in_signal_sq[i] = np.round(in_signal_sq[i]).astype('int')
    if in_signal_sq[i] >= 0: 
        tmp = in_signal_sq[i]
        inv.append(tmp)
    elif in_signal_sq[i] < 0: 
        tmp = 256 + in_signal_sq[i]
        inv.append(tmp)

np.savetxt("shifted.txt", inv, fmt="%d")
np.savetxt("input_vectors.txt", np.round(in_signal_sq).astype('int'), fmt="%d")

print(np.amax(in_signal_sin), np.amin(in_signal_sin))
plt.figure(figsize=(10,5))

plt.plot(t_sq, in_signal_sq, label='sq')
#print(len(in_signal_sin))
#plt.plot(t_sq, in_signal_sq, label='square')
#print(len(in_signal_sq))
plt.show()
