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