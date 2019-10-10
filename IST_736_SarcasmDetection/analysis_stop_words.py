import pandas as pd
import numpy as np


data = pd.read_csv("word_freq_data.txt")

data_subset = data.query("count > 5000")

print(data_subset)
