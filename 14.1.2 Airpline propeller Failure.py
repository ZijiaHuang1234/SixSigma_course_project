import math
def fplane(t):
    return 1-math.exp(-t/750)
print("fplane(600):",fplane(600))
print("fplane(5000):",fplane(5000))
print("percentage of surviving past 6000 days:",fplane(6000))
#given a sample of 300 propollers
n = 300
# We project n * fplane(t = 365.25) will fail in 1 year (365.25 days)
print("number of propellers that will fail in 1 year:",n * fplane(365.25))
print("failure in 3 years:",n * fplane(365.25 * 3))