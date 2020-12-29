"""
Topic   : Advent of Code
Year    : 2019
Day     : 7
Title   : Amplification Circuit
"""

import numpy as np
import itertools

# =============================================================================
# Part One
# =============================================================================
def ModeToPosition(x, pointer):
    opcode = x[pointer] % 100
    modes = ("0000" + str(x[pointer]))[:-2][::-1]
    if opcode in [1, 2, 7, 8]:
        positions = np.zeros(3).astype(int)
    elif opcode in [3, 4]:
        positions = np.zeros(1).astype(int)
    elif opcode in [5, 6]:
        positions = np.zeros(2).astype(int)
    for i in np.arange(len(positions)):
        if modes[i] == "0":
            positions[i] = x[pointer + 1 + i]
        elif modes[i] == "1":
            positions[i] = pointer + 1 + i
    return positions

def Intcode(programInput, inputValues = [None]):
    x = programInput.copy()
    pos = 0
    inputCounter = 0
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
            if opcode == 3:
                x[positions[0]] = inputValues[inputCounter]
                inputCounter += 1
            elif opcode == 4:
                if x[pos + 2] % 100 != 99:
                    print("Output instruction: " + str(x[positions[0]]))
                else:
                    return x[positions[0]]
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

def amplifiersOutput(programInput, phaseSetting):
    output = Intcode(programInput, inputValues = [phaseSetting[0], 0])
    for i in phaseSetting[1:]:
        output = Intcode(programInput, inputValues = [i, output])
    return output

def thrustersSignal(programInput, amplifiers):
    phases = list(itertools.permutations(amplifiers))
    signal = np.zeros(len(phases), dtype = int)
    for i in np.arange(len(signal)):
        signal[i] = amplifiersOutput(programInput, phases[i])
    return(max(signal))

# Test runs
testProgramInput = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
print(amplifiersOutput(testProgramInput, [4,3,2,1,0]))
print(thrustersSignal(testProgramInput, np.arange(5)))

testProgramInput = [3,23,3,24,1002,24,10,24,1002,23,-1,23,
                    101,5,23,23,1,24,23,23,4,23,99,0,0]
print(amplifiersOutput(testProgramInput, [0,1,2,3,4]))
print(thrustersSignal(testProgramInput, np.arange(5)))

testProgramInput = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                    1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
print(amplifiersOutput(testProgramInput, [1,0,4,3,2]))
print(thrustersSignal(testProgramInput, np.arange(5)))

# Solution
programInput = np.loadtxt("Day7Input.txt", delimiter = ",", dtype = int)
print(thrustersSignal(programInput, np.arange(5)))

# =============================================================================
# Part Two
# =============================================================================
def Intcode(programInput, inputValues = [None], pos = 0):
    x = programInput.copy()
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
            if opcode == 3:
                inputCounter = sum([i % 100 == 3 for i in x[:pos + 1]]) - 1
                if inputCounter > 1:
                    inputCounter = 1
                x[positions[0]] = inputValues[inputCounter]
            elif opcode == 4:
                if x[pos + 2] % 100 != 99:
                    return x, x[positions[0]], pos + 2, False
                else:
                    return x, x[positions[0]], pos, True
                
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
    return x, x[positions[0]], pos, True

def amplifiersOutput(programInput, phaseSetting):
    x = [programInput.copy() for i in np.arange(len(phaseSetting))]
    pos = np.zeros(len(phaseSetting), dtype = int)
    output, signal = 0, 0
    halt = False
    while not halt:
        for i in np.arange(len(phaseSetting)):
            x[i], output, pos[i], halt = Intcode(x[i], [phaseSetting[i], output], pos[i])
        if not halt or x[i][pos[i]] % 100 == 4:
            signal = output
    return signal

# Test runs
testProgramInput = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                    27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
print(amplifiersOutput(testProgramInput, [9,8,7,6,5]))
print(thrustersSignal(testProgramInput, np.arange(5, 10)))

testProgramInput = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                    -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                    53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
print(amplifiersOutput(testProgramInput, [9,7,8,5,6]))
print(thrustersSignal(testProgramInput, np.arange(5, 10)))

# Solution
programInput = np.loadtxt("Day7Input.txt", delimiter = ",", dtype = int)
print(thrustersSignal(programInput, np.arange(5, 10)))

