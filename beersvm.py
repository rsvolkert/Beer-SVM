import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

beer = pd.read_csv('beers.csv')
beer = beer.dropna()

idx = 0
for style in beer['style']:
    if 'IPA' in style:
        beer.loc[idx, 'style'] = 'IPA'
