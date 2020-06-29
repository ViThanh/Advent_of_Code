"""
Topic   : Advent of Code
Year    : 2019
Day     : 5
Title   : Sunny with a Chance of Asteroids
"""

# =============================================================================
# Part One
# =============================================================================
def Intcode(x):
    pos = 0
    while x[pos] != 99:
        if x[pos] in [1, 2]:
            pos1 = x[pos + 1]
            pos2 = x[pos + 2]
            pos3 = x[pos + 3]
            if x[pos] == 1:
                x[pos3] = x[pos1] + x[pos2]
            elif x[pos] == 2:
                x[pos3] = x[pos1] * x[pos2]
            pos += 4
        elif x[pos] in [3, 4]:
            pos1 = x[pos + 1]
            if x[pos] == 3:
                x[pos1] = 3
            elif x[pos] == 4:
                x[pos1] = x[pos1]
            pos += 2
    return x

print(Intcode([3,0,4,0,99]))
