#%%
scalemethod = "meanstd"       # possibilites: minmax, meanstd
modelchoice = "gradientboost"      # possibilities: adaboost, elastic, knn, lasso, lightgbm, nn, svr
remove_outliers = True

#%%
import h5py
import lightgbm
import os
import warnings

import numpy as np
import pandas as pd
import plotly.express as px
import tensorflow as tf

from matplotlib import pyplot as plt
from sklearn import linear_model, neighbors, svm
from sklearn.ensemble import AdaBoostRegressor, GradientBoostingRegressor
from sklearn.multioutput import MultiOutputRegressor
from sklearn.preprocessing import MinMaxScaler, StandardScaler
from tables import NaturalNameWarning
from tensorflow.keras.layers import Dense
from tensorflow.keras.models import Sequential
from tensorflow.keras.optimizers import Adam

warnings.filterwarnings('ignore', category=NaturalNameWarning)

filepath = os.path.join("exercise_epftoolbox", "DE_fulldata.hdf5")

def smape(pred, real):
    return (1 / len(pred)) * np.sum(2 * np.abs(real - pred) / (np.abs(pred) + np.abs(real)))

with h5py.File(filepath, "r") as f:
    # List all groups
    print("Keys: %s" % f.keys())

    # Get the data
    data = {key : pd.read_hdf(filepath, key=key, errors="ignore") for key in f.keys()}

X_train = data["xtrain"]
y_train = data["ytrain"]

if remove_outliers:
    for col in y_train.columns:
        q10, q90 = np.percentile(y_train[col], [10, 90])
        intr_qr = q90 - q10
    
        max = q90 + (1.5 * intr_qr)
        min = q10 - (1.5 * intr_qr)
    
        y_train[col][y_train[col] < min] = np.nan
        y_train[col][y_train[col] > max] = np.nan

    rowswithna = y_train.isna().any(axis=1)

    print(f"Drop {sum(rowswithna)} rows")
    X_train = X_train[~rowswithna]
    y_train = y_train[~rowswithna]


X_test = data["xtest"]
y_test = data["ytest"]
df_forecasts = data["forecasts"]

if scalemethod == "minmax":
    xscaler = MinMaxScaler()
    yscaler = MinMaxScaler()
elif scalemethod == "meanstd":
    xscaler = StandardScaler()
    yscaler = StandardScaler()
else:
    raise Exception(f"unknown setting {scalemethod}")

xscaler.fit(X_train)
yscaler.fit(y_train)

X_train_sc = xscaler.transform(X_train)
X_test_sc = xscaler.transform(X_test)

y_train_sc = yscaler.transform(y_train)
y_test_sc = yscaler.transform(y_test)

#%%
if modelchoice == "nn":
    def nnmodel(inputdim, outputdim):
        model = Sequential()
        model.add(Dense(256, input_dim=inputdim, kernel_initializer='he_normal', activation='leaky_relu'))
        model.add(Dense(256, kernel_initializer='he_uniform', activation='leaky_relu'))
        model.add(Dense(256, kernel_initializer='he_uniform', activation='leaky_relu'))
        model.add(Dense(256, kernel_initializer='he_uniform', activation='leaky_relu'))
        model.add(Dense(outputdim, kernel_initializer='he_uniform', activation='linear'))
        model.compile(loss='mean_absolute_error', optimizer=Adam())

        return model

    model = nnmodel(X_train_sc.shape[1], y_train_sc.shape[1])

    tf.keras.backend.clear_session()
    callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=100, restore_best_weights=True)
    history = model.fit(X_train_sc, y_train_sc, epochs=10000, batch_size=8, verbose=1, shuffle=True, validation_split=0.1, workers=15, use_multiprocessing=True, callbacks=[callback])

    plt.plot(history.history['loss'])
    plt.plot(history.history['val_loss'])
    plt.title('model loss')
    plt.ylabel('loss')
    plt.xlabel('epoch')
    plt.legend(['train', 'val'], loc='upper left')
    plt.show()
elif modelchoice == "knn":
    model = neighbors.KNeighborsRegressor(10, weights="distance", p=1)
    model.fit(X_train_sc, y_train_sc)
elif modelchoice == "lasso":
    model = linear_model.MultiTaskLasso(alpha=0.05, fit_intercept=False, max_iter=10000)
    model.fit(X_train_sc, y_train_sc)
elif modelchoice == "elastic":
    model = linear_model.MultiTaskElasticNet(alpha=0.1, fit_intercept=False, max_iter=10000)
    model.fit(X_train_sc, y_train_sc)
elif modelchoice == "svr":
    model = MultiOutputRegressor(svm.SVR())
    model.fit(X_train_sc, y_train_sc)
elif modelchoice == "adaboost":
    model = MultiOutputRegressor(AdaBoostRegressor(n_estimators=100))
    model.fit(X_train_sc, y_train_sc)
elif modelchoice == "gradientboost":
    model = MultiOutputRegressor(GradientBoostingRegressor(n_estimators=100, learning_rate=0.1, max_depth=3))
    model.fit(X_train_sc, y_train_sc)
elif modelchoice == "lightgbm":
    model = MultiOutputRegressor(lightgbm.LGBMRegressor())
    model.fit(X_train_sc, y_train_sc)
else:
    raise Exception(f"unknown setting {modelchoice}")

#%%
pred_sc = model.predict(X_test_sc)
pred = yscaler.inverse_transform(pred_sc)

# %%
smape_all = smape(pred, y_test)
print(np.mean(smape_all))

# %%
fig = px.scatter(x=pred[:, 0], y=y_test.iloc[:, 0], labels=dict(x="pred", y="real"))
fig.update_layout(shapes=[{'type': 'line', 'yref': 'paper', 'xref': 'paper', 'y0': 0, 'y1': 1, 'x0': 0, 'x1': 1}])
#fig.add_trace([[np.min(pred[:, 0]), np.max(pred[:, 0])], [np.min(y_test.iloc[:, 0]), np.max(y_test.iloc[:, 0])]])
fig.show()

# %%
