# Policy experiment aversion: Survey and embedded randomized trial conducted during a national debate about a real-life policy experiment on the effectiveness of an in-work tax credit

Contact Ingeborg Hess Elgersma, inel@fhi.no for questions.

The analysis was prespecified in http://dx.doi.org/10.2139/ssrn.5408584

## How to run the analysis

1. Check out this repository to you local machine.
2. Install the `remotes` package (version 2.5.0).
3. Change the R working directory to that containing the repository.
4. Run `source("run_analysis.R")`.

Results tables will be saved to the directory `results`.

## Explanation R-scrips

### run_analysis.R
Installs the neccessary packages and runs the analysis

### data_cleaning.R
Cleans datasets

### produce_targets.R
Prepares the marginal distribution of population charachteristics that are used in weighing

### weigh_data_2.R
Weighs the data

### table1.R
Produces table 1 as prespecified in http://dx.doi.org/10.2139/ssrn.5408584

### table2.R
Produces table 2 as prespecified in http://dx.doi.org/10.2139/ssrn.5408584

### table3.R
Produces table 3 as prespecified in http://dx.doi.org/10.2139/ssrn.5408584


### sensitivity_analysis_missingparty.R
Produces sensitivity analysis in table 1.