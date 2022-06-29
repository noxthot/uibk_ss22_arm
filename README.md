# uibk_ss22_arm
Exercises - Summer semester 2022 - Advanced Regression Models (attended at the University of Innsbruck) 

The course content is freely available at https://discdown.org/flexregression

## Setup (Ubuntu)
### Conda
Use conda to install all required modules (default environment: `ss22_arm`):\
```
conda env create -f environment.yml
```

In case you already got the environment and only need to update to the latest `environment.yml` use:\
```
conda activate ss22_arm
conda env update --file environment.yml --prune
```

After manually adding a package, update the `environment.yml` using this command:\
```
conda env export --name ss22_arm > environment.yml
```

## Usage (Energy price forecast challenge)
The related files are found in `exercise_epftoolbox`.

First activate the environment (see previous point).

Then run `grab_data.csv` to create `DE_train.csv`, `DE_test.csv`, `DE_test_forecasts.csv` (just once). Run `etl.R` to transform the data for later use. This file creates `DE_fulldata.hdf5` (used by `models.py`) and stores the dataframes in memory (used by `models.R`). 

In `models.R` and `models.py` you can choose the used model by modifying `model_choice` variable. The possible choices are documented in the source code.