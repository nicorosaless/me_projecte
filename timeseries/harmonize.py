import pandas as pd
import os

# Read the CSV
csv_path = os.path.join("timeseries", "data", "cps_asec_poverty_1959_2024.csv")
dat_raw = pd.read_csv(csv_path)

# Filter for US data
dat = dat_raw[dat_raw['us'] == 1]

# Create output directory
out_dir = os.path.join("timeseries", "output")
os.makedirs(out_dir, exist_ok=True)

# Define group priority mapping
group_priority = pd.DataFrame([
    {"group": "All", "RACE_NAME": "All Races", "priority": 1},
    {"group": "White", "RACE_NAME": "White Alone, Not Hispanic", "priority": 1},
    {"group": "White", "RACE_NAME": "White Alone", "priority": 2},
    {"group": "White", "RACE_NAME": "White", "priority": 3},
    {"group": "Black", "RACE_NAME": "Black Alone", "priority": 1},
    {"group": "Black", "RACE_NAME": "Black Alone or in Combination", "priority": 2},
    {"group": "Black", "RACE_NAME": "Black", "priority": 3},
    {"group": "Hispanic", "RACE_NAME": "Hispanic (of any race)", "priority": 1},
    {"group": "Asian", "RACE_NAME": "Asian Alone", "priority": 1},
    {"group": "Asian", "RACE_NAME": "Asian Alone or in Combination", "priority": 2},
    {"group": "Asian", "RACE_NAME": "Asian and Pacific Islander", "priority": 3},
])

# Join and harmonize
harmonized = (dat
    .merge(group_priority, on="RACE_NAME", how="inner")
    .sort_values(['time', 'group', 'priority'])
    .groupby(['time', 'group'])
    .first()
    .reset_index()
    [['time', 'group', 'PCTPOV', 'POV', 'POP', 'RACE_NAME']]
)

# Create wide format
wide = (harmonized
    [['time', 'group', 'PCTPOV']]
    .drop_duplicates()
    .pivot(index='time', columns='group', values='PCTPOV')
    .reset_index()
)

# Save the harmonized data
harmonized.to_csv(os.path.join(out_dir, "poverty_harmonized.csv"), index=False)
wide.to_csv(os.path.join(out_dir, "mapping_summary.csv"), index=False)

print(f"\n✓ Created: {os.path.join(out_dir, 'poverty_harmonized.csv')}")
print(f"✓ Created: {os.path.join(out_dir, 'mapping_summary.csv')}")
print(f"\nHarmonized {len(harmonized)} rows across {harmonized['group'].nunique()} groups")
print(f"Years: {harmonized['time'].min()} - {harmonized['time'].max()}")
