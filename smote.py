import pandas as pd
import numpy as np

responded = pd.read_csv("data/responded.csv")

responded.columns

## with one hots
X = pd.concat([
    responded.loc[:, "sessionLengthByHit":"nStrips"],
    responded.loc[:, "Decisive_catch-up":"Inspiration_watchlist"]
    ], axis = 1
    )

## with intent column
X = pd.concat([
    responded.loc[:, "sessionLengthByHit":"nStrips"],
    responded.loc[:, "intent"]
    ], axis = 1
    )

y = responded.satisfaction

freqSat = y.value_counts()
oversampledFreqSat = freqSat.copy()

satisfied = np.sum(y > 3)
unsatisfied = np.sum(y < 4)

oversampledFreqSat[1] = np.round(satisfied * (oversampledFreqSat[1] / unsatisfied))
oversampledFreqSat[2] = np.round(satisfied * (oversampledFreqSat[2] / unsatisfied))
oversampledFreqSat[3] = np.round(satisfied * (oversampledFreqSat[3] / unsatisfied))

oversampledFreqSat[1] + oversampledFreqSat[2] + oversampledFreqSat[3] == satisfied

oversampledFreqSat.to_dict()

from imblearn.over_sampling import SMOTENC
# smote_nc = SMOTENC(categorical_features=list(range(9,  17)), random_state=0) # with one-hots
smote_nc = SMOTENC(categorical_features=[9], random_state=0,
                   sampling_strategy = oversampledFreqSat.to_dict())
X_resampled, y_resampled = smote_nc.fit_resample(X, y)

smoteResponded = pd.concat([
    X_resampled, y_resampled
    ], axis = 1)

smoteResponded.to_csv("data/smoteResponded.csv", index = False)

