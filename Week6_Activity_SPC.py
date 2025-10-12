import pandas as pd 
import numpy as np
import matplotlib.pyplot as plt
import functions_distributions as fd
from pandas import Series

# read the osen.csv
file_path = r"D:\codeWork\5300_repository\workshops\onsen.csv"
df = pd.read_csv(file_path)

# Descriptive statistic function
def describe(x):
    x = Series(x)
    out = pd.DataFrame(
        {
            "mean": [x.mean()],
            "sd": [x.std()],
        }
    )
    out['caption'] = ("process Mean: " + out['mean'].round(2).astype(str) + " | SD: " + out['sd'].round(2).astype(str))
    return out

# Check data structure and find pH column
print("Data shape:", df.shape)
print("Columns:", df.columns.tolist())
print("\nFirst 5 rows:")
print(df.head())

# Calculate the describitive statistic based on PH's value
ph_data = df['ph'].dropna()  
mean_ph = ph_data.mean()
std_ph = ph_data.std()
median_ph = ph_data.median()

print(f"\npH Statistics:")
print(f"Mean: {mean_ph:.3f}")
print(f"Standard Deviation: {std_ph:.3f}")
print(f"Median: {median_ph:.3f}")
print(f"Sample size: {len(ph_data)}")

# For SPC charts, we need to group data into subgroups
# Assuming we want subgroups of size 5 (common in SPC)
subgroup_size = 5
n_subgroups = len(ph_data) // subgroup_size
ph_subgroups = ph_data.iloc[:n_subgroups * subgroup_size].values.reshape(n_subgroups, subgroup_size)

# Calculate subgroup statistics
subgroup_means = np.mean(ph_subgroups, axis=1)
subgroup_stds = np.std(ph_subgroups, axis=1, ddof=1)  # Sample standard deviation
subgroup_ranges = np.max(ph_subgroups, axis=1) - np.min(ph_subgroups, axis=1)

# Overall statistics for control limits
overall_mean = np.mean(subgroup_means)
overall_std = np.mean(subgroup_stds)
overall_range = np.mean(subgroup_ranges)

print(f"\nSubgroup Statistics:")
print(f"Number of subgroups: {n_subgroups}")
print(f"Subgroup size: {subgroup_size}")
print(f"Overall mean: {overall_mean:.3f}")
print(f"Average standard deviation: {overall_std:.3f}")
print(f"Average range: {overall_range:.3f}")

# SPC Control Charts - Generate Averages, Standard Deviations, and Ranges plots

# Control chart constants for subgroup size 5
A2 = 0.577  # For X-bar chart
D3 = 0      # For R chart (lower control limit)
D4 = 2.114  # For R chart (upper control limit)
B3 = 0      # For S chart (lower control limit) 
B4 = 2.089  # For S chart (upper control limit)

# Calculate control limits
# X-bar chart limits
xbar_cl = overall_mean
xbar_ucl = overall_mean + A2 * overall_range
xbar_lcl = overall_mean - A2 * overall_range

# R chart limits
r_cl = overall_range
r_ucl = D4 * overall_range
r_lcl = D3 * overall_range

# S chart limits
s_cl = overall_std
s_ucl = B4 * overall_std
s_lcl = B3 * overall_std

print(f"\nControl Limits:")
print(f"X-bar Chart - CL: {xbar_cl:.3f}, UCL: {xbar_ucl:.3f}, LCL: {xbar_lcl:.3f}")
print(f"R Chart - CL: {r_cl:.3f}, UCL: {r_ucl:.3f}, LCL: {r_lcl:.3f}")
print(f"S Chart - CL: {s_cl:.3f}, UCL: {s_ucl:.3f}, LCL: {s_lcl:.3f}")

# Create the three required charts
plt.figure(figsize=(15, 10))

# 1. AVERAGES PLOT (X-bar Chart)
plt.subplot(2, 2, 1)
plt.plot(range(1, n_subgroups + 1), subgroup_means, 'bo-', markersize=6, linewidth=2)
plt.axhline(y=xbar_cl, color='r', linestyle='-', linewidth=2, label=f'CL: {xbar_cl:.3f}')
plt.axhline(y=xbar_ucl, color='red', linestyle='--', linewidth=2, label=f'UCL: {xbar_ucl:.3f}')
plt.axhline(y=xbar_lcl, color='red', linestyle='--', linewidth=2, label=f'LCL: {xbar_lcl:.3f}')
plt.xlabel('Subgroup Number')
plt.ylabel('Subgroup Average (pH)')
plt.title('X-bar Chart (Averages Plot)')
plt.legend()
plt.grid(True, alpha=0.3)

# 2. STANDARD DEVIATIONS PLOT (S Chart)
plt.subplot(2, 2, 2)
plt.plot(range(1, n_subgroups + 1), subgroup_stds, 'go-', markersize=6, linewidth=2)
plt.axhline(y=s_cl, color='r', linestyle='-', linewidth=2, label=f'CL: {s_cl:.3f}')
plt.axhline(y=s_ucl, color='red', linestyle='--', linewidth=2, label=f'UCL: {s_ucl:.3f}')
plt.axhline(y=s_lcl, color='red', linestyle='--', linewidth=2, label=f'LCL: {s_lcl:.3f}')
plt.xlabel('Subgroup Number')
plt.ylabel('Subgroup Standard Deviation')
plt.title('S Chart (Standard Deviations Plot)')
plt.legend()
plt.grid(True, alpha=0.3)

# 3. RANGES PLOT (R Chart)
plt.subplot(2, 2, 3)
plt.plot(range(1, n_subgroups + 1), subgroup_ranges, 'mo-', markersize=6, linewidth=2)
plt.axhline(y=r_cl, color='r', linestyle='-', linewidth=2, label=f'CL: {r_cl:.3f}')
plt.axhline(y=r_ucl, color='red', linestyle='--', linewidth=2, label=f'UCL: {r_ucl:.3f}')
plt.axhline(y=r_lcl, color='red', linestyle='--', linewidth=2, label=f'LCL: {r_lcl:.3f}')
plt.xlabel('Subgroup Number')
plt.ylabel('Subgroup Range')
plt.title('R Chart (Ranges Plot)')
plt.legend()
plt.grid(True, alpha=0.3)

# 4. Individual Values Plot
plt.subplot(2, 2, 4)
plt.plot(range(1, len(ph_data) + 1), ph_data.values, 'b-', alpha=0.7, linewidth=1)
plt.axhline(y=mean_ph, color='r', linestyle='--', linewidth=2, label=f'Overall Mean: {mean_ph:.3f}')
plt.axhline(y=mean_ph + 3*std_ph, color='red', linestyle=':', linewidth=1, label=f'+3σ: {mean_ph + 3*std_ph:.3f}')
plt.axhline(y=mean_ph - 3*std_ph, color='red', linestyle=':', linewidth=1, label=f'-3σ: {mean_ph - 3*std_ph:.3f}')
plt.xlabel('Sample Number')
plt.ylabel('pH Value')
plt.title('Individual Values Plot')
plt.legend()
plt.grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

# Print summary statistics using the describe function
print("\n" + "="*50)
print("DESCRIPTIVE STATISTICS SUMMARY")
print("="*50)
summary_stats = describe(ph_data)
print(summary_stats['caption'].iloc[0])

# Use functions_distributions hist function
print("\nUsing functions_distributions hist function:")
hist_plot = fd.hist(ph_data)
print(hist_plot)






