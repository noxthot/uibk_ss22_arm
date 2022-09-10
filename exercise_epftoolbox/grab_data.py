import os

import pandas as pd

from epftoolbox.data import read_data

prj_dir = "exercise_epftoolbox"

df_train, df_test = read_data(path=".", dataset='DE', begin_test_date='04-01-2016', end_test_date='31-12-2017')
df_train.query("index >= '2013-01-10'").to_csv(os.path.join(prj_dir, 'DE_train.csv'))
df_test.to_csv(os.path.join(prj_dir, 'DE_test.csv'))

forecast_url = "https://raw.githubusercontent.com/jeslago/epftoolbox/master/forecasts/Forecasts_DE_DNN_LEAR_ensembles.csv"

forecast = pd.read_csv(forecast_url, index_col=0)
forecast.to_csv(os.path.join(prj_dir, "DE_test_forecasts.csv"))