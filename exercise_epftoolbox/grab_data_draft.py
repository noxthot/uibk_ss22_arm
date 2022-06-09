from epftoolbox.data import read_data

df_train, df_test = read_data(path=".", dataset='DE', begin_test_date='04-01-2016', end_test_date='31-12-2017')
df_train.to_csv('DE_train.csv')
df_test.to_csv('DE_test.csv')