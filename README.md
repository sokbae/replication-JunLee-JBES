# Replication: Jun and Lee (2023, JBES, forthcoming)

Replication files for all numerical results in Jun and Lee (2023, JBES).

## Overview

This folder contains files to replicate results from the following paper:

Sung Jae Jun and Sokbae Lee. 2023. Causal Inference under Outcome-Based Sampling with Monotonicity Assumptions. <https://arxiv.org/abs/2004.08318>. Accepted for publication in _Journal of Business & Economic Statistics_.

The latest version of the paper is included in this Github repository
([see paper.pdf](https://github.com/sokbae/replication-JunLee-JBES/blob/main/paper.pdf)).


## Replication Code

- The numerical results are produced using R. 

- First, [download this repository](https://github.com/sokbae/replication-JunLee-JBES/archive/main.zip). 

- Second, you need to install the `ciccr` package from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools") # uncomment this line if devtools is not installed yet 
devtools::install_github("sokbae/ciccr")
```

- Note that the dataset used in Carvalho and Soares (2016) cannot be shared publicly for the privacy of individuals that participated in the study. Hence, "gangs_cleaned.dta" is not provided in this repo. To run the code without error, the data file needs to be located under the subdirectory "R_code/data".

- All the numerical results can be obtained by running `run_all.R`. Before running it, create the subdirectory "R_code/results". Once it is run, the results are saved under the subdirectory "R_code/results". To avoid the error message, two R files that use "gangs_cleaned.dta" are commented out.

## Reference

Sung Jae Jun and Sokbae Lee. 2023. Causal Inference under Outcome-Based Sampling with Monotonicity Assumptions. <https://arxiv.org/abs/2004.08318>. Accepted for publication in _Journal of Business & Economic Statistics_.
