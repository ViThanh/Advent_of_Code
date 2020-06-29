"""
Topic   : Advent of Code
Year    : 2019
Day     : 4
Title   : Secure Container
"""

import numpy as np
from collections import Counter

# =============================================================================
# Part One
# =============================================================================
lower = 372037
upper = 905157

# Create an array with all numbers with the range
allNumbers = np.arange(lower, upper + 1).astype(str)

# Convert numbers into strings and convert characters into integers again
allNumbers = [[int(i) for i in j] for j in allNumbers]

# Find indicators for possible passwords
# - First condition checks for non-decreasing digits
# - Second condition checks for double digits
passwordsInd = [np.prod(i == np.sort(i)) and (max(Counter(i).values()) > 1) 
                for i in allNumbers]

# Calculate the number of possible passwords
print(sum(passwordsInd))

# =============================================================================
# Part Two
# =============================================================================
# Find indicators for possible passwords
# - First condition checks for non-decreasing digits
# - Second condition checks for double digits
passwordsInd = [np.prod(i == np.sort(i)) and (2 in Counter(i).values()) 
                for i in allNumbers]

# Calculate the number of possible passwords
print(sum(passwordsInd))
