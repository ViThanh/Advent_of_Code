"""
Topic   : Advent of Code
Year    : 2019
Day     : 5
Title   : Sunny with a Chance of Asteroids
"""

import numpy as np
from numpy import loadtxt

# =============================================================================
# Part One
# =============================================================================
def ModeToPosition(x, pointer):
    opcode = x[pointer] % 100
    modes = ('0000' + str(x[pointer]))[:-2][::-1]
    if opcode in [1, 2, 7, 8]:
        positions = np.zeros(3).astype(int)
    elif opcode in [3, 4]:
        positions = np.zeros(1).astype(int)
    elif opcode in [5, 6]:
        positions = np.zeros(2).astype(int)
    for i in np.arange(len(positions)):
        if modes[i] == '0':
            positions[i] = x[pointer + 1 + i]
        elif modes[i] == '1':
            positions[i] = pointer + 1 + i
    return positions

def Intcode(programInput, inputValue = None):
    x = programInput.copy()
    pos = 0
    while x[pos] % 100 != 99:
        opcode = x[pos] % 100
        positions = ModeToPosition(x, pos)
        if opcode in [1, 2]:
            if opcode == 1:
                x[positions[2]] = x[positions[0]] + x[positions[1]]
            elif opcode == 2:
                x[positions[2]] = x[positions[0]] * x[positions[1]]
            pos += 4
        elif opcode in [3, 4]:
            if opcode== 3:
                x[positions[0]] = inputValue
            elif opcode == 4:
                if (x[positions[0]] == 0) and (x[pos + 2] % 100 != 99):
                    print('Output instruction: ' + str(x[positions[0]]))
                else:
                    return 'Diagnostic code: ' + str(x[positions[0]])
            pos += 2
    return 'Program output: ' + str(x[0])

# Test runs
print(Intcode([3,0,4,0,99]))
print(Intcode([1002,4,3,4,33]))
print(Intcode([1101,100,-1,4,0]))

# Solution
programInput = loadtxt("Day5Input.txt", delimiter = ",", dtype = int)
print(Intcode(programInput, inputValue = 1))

# =============================================================================
# Part Two
# =============================================================================
def Intcode(programInput, inputValue = None):
    x = programInput.copy()
    pos = 0
    while x[pos] % 100 != 99:
        opcode = x[pos] % 100
        positions = ModeToPosition(x, pos)
        if opcode in [1, 2, 7, 8]:
            if opcode == 1:
                x[positions[2]] = x[positions[0]] + x[positions[1]]
            elif opcode == 2:
                x[positions[2]] = x[positions[0]] * x[positions[1]]
            elif opcode == 7:
                x[positions[2]] = (x[positions[0]] < x[positions[1]]) * 1
            elif opcode == 8:
                x[positions[2]] = (x[positions[0]] == x[positions[1]]) * 1
            pos += 4
        elif opcode in [3, 4]:
            if opcode== 3:
                x[positions[0]] = inputValue
            elif opcode == 4:
                if (x[positions[0]] == 0) and (x[pos + 2] % 100 != 99):
                    print('Output instruction: ' + str(x[positions[0]]))
                else:
                    return 'Diagnostic code: ' + str(x[positions[0]])
            pos += 2
        elif opcode in [5, 6]:
            if opcode == 5:
                if x[positions[0]] != 0:
                    pos = x[positions[1]]
                else:
                    pos += 3
            elif opcode == 6:
                if x[positions[0]] == 0:
                    pos = x[positions[1]]
                else:
                    pos += 3

# Test runs
print(Intcode([3,9,8,9,10,9,4,9,99,-1,8], inputValue = 8))
print(Intcode([3,9,7,9,10,9,4,9,99,-1,8], inputValue = 7))
print(Intcode([3,3,1108,-1,8,3,4,3,99], inputValue = 8))
print(Intcode([3,3,1107,-1,8,3,4,3,99], inputValue = 7))
print(Intcode([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], inputValue = 0))
print(Intcode([3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9], inputValue = 1))
print(Intcode([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], inputValue = 0))
print(Intcode([3,3,1105,-1,9,1101,0,0,12,4,12,99,1], inputValue = 1))
testProgramInput = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
print(Intcode(testProgramInput, inputValue = 7))
print(Intcode(testProgramInput, inputValue = 8))
print(Intcode(testProgramInput, inputValue = 9))

# Solution
programInput = loadtxt("Day5Input.txt", delimiter = ",", dtype = int)
print(Intcode(programInput, inputValue = 5))
