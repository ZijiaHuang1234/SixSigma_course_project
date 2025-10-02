USL = 2.5
LSL = 1.5
mean = 2.01
sigma = 0.05
cp = (USL - LSL) / (6 * sigma)
cpk = min((mean - LSL) / (3 * sigma), (USL - mean) / (3 * sigma))
pp = (USL - LSL) / (6 * sigma)
ppk = min((mean - LSL) / (3 * sigma), (USL - mean) / (3 * sigma))
print(cp, cpk, pp, ppk)

