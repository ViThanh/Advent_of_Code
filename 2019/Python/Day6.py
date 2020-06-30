"""
Topic   : Advent of Code
Year    : 2019
Day     : 6
Title   : Universal Orbit Map
"""

import numpy as np
import pandas as pd

# =============================================================================
# Part One 
# =============================================================================
def OrbitCountCheckSum(mapData):
    orbits = pd.DataFrame({"orbited": [i[:i.find(")")] for i in mapData], 
                           "orbiting": [i[i.find(")") + 1:] for i in mapData]})
    while len(set(orbits.orbited) & set(orbits.orbiting)) > 0:
        orbitsCopy = orbits.rename(columns = {"orbited": "link", "orbiting": "last"})
        orbits = orbits.merge(orbitsCopy, "left", left_on = "orbiting", right_on = "link")
        orbits.drop(columns = "link", inplace = True)
        orbits.rename(columns = {"orbiting": "intermediate", "last": "orbiting"}, inplace = True)
    orbits = orbits[orbits.orbited == "COM"]
    orbitLength = np.arange(orbits.shape[1])
    uniqueCount = orbits.describe().loc["unique"]
    return {"orbits": orbits, 
            "Orbit count checksum": "Orbit count checksum: " + str(sum(uniqueCount * orbitLength))}

# Test run
testMapData = np.array(["COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I",
                        "E)J","J)K","K)L"])
print(OrbitCountCheckSum(testMapData)["Orbit count checksum"])

# Solution
mapData = np.loadtxt("Day6Input.txt", dtype = str)
print(OrbitCountCheckSum(mapData)["Orbit count checksum"])

# =============================================================================
# Part Two
# =============================================================================
def OrbitalTransfers(mapData):
    orbits = OrbitCountCheckSum(mapData)["orbits"]
    YOU = np.array(orbits[orbits.eq("YOU").any(1)]).reshape(-1)
    SAN = np.array(orbits[orbits.eq("SAN").any(1)]).reshape(-1)
    transfers = int(np.where(YOU == "YOU")[0] + np.where(SAN == "SAN")[0] - 2 * np.sum(YOU == SAN))
    return "Number of orbital transfers: " + str(transfers)

# Test run
testMapData = np.concatenate((testMapData, ["K)YOU","I)SAN"]))
print(OrbitalTransfers(testMapData))

# Solution
mapData = np.loadtxt("Day6Input.txt", dtype = str)
print(OrbitalTransfers(mapData))
