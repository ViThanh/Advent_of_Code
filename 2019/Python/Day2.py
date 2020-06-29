"""
Topic   : Advent of Code
Year    : 2019
Day     : 2
Title   : 1202 Program Alarm
"""

import numpy as np
from numpy import loadtxt

# =============================================================================
# Part One
# =============================================================================
def Intcode(x):
    pos = 0
    while x[pos] != 99:
        pos1 = x[pos + 1]
        pos2 = x[pos + 2]
        pos3 = x[pos + 3]
        if x[pos] == 1:
            x[pos3] = x[pos1] + x[pos2]
        elif x[pos] == 2:
            x[pos3] = x[pos1] * x[pos2]
        pos += 4
    return x

print(Intcode([1,0,0,0,99]))
print(Intcode([2,3,0,3,99]))
print(Intcode([2,4,4,5,99,0]))
print(Intcode([1,1,1,4,99,5,6,0,99]))

programInput = loadtxt("Day2Input.txt", delimiter = ",").astype(int)
programInput[1] = 12
programInput[2] = 2
print(Intcode(programInput)[0])

# =============================================================================
# Part Two
# =============================================================================
programInput = loadtxt("Day2Input.txt", delimiter = ",").astype(int)
output = 19690720
for i in np.arange(100):
    for j in np.arange(100):
        x = np.concatenate(([programInput[0]], [i, j], programInput[3:]))
        if Intcode(x)[0] == output:
            print(100 * i + j)