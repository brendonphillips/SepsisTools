# SEPSIS Tools
For SEPSIS data sets, we realised that the Wald test fails when including groups 
with all-zeros (excluding these groups bring a host of problems, including 
questionable interpretation of the results), so we've gone with permutation 
tests, which we do both globally, and for pairwise comparisons between IP groups 
and placebo.

For the permutation test, the test statistic that Eleanor has suggested we use 
is the sum of square differences; this is not a popular choice of test statistic, 
so we cannot use many of the existing R packages (though the closest is the 
`coin` package on CRAN, with the quadratic test statistic). To provide 
flexibility (and to avoid code duplication), we've put our implementation of the 
permutation test in the file `rothlab_permtest`. The code in this file may be 
updated, but the interface will not be changed (as much as possible).

# Install
Install the package as `devtools::install("brendonphillips/SepsisTools")` (please install the `devtools` package if you haven't already downloaded it).

-----
## Dependencies
The `rothlab_permtest` library requires (and will autoinstall) the following 
packages:
<to be completed>

### `perm_test_statistic`
this function calculates the test statistic - called as `perm_test_statistic(events, groups)`, 
where `events` is an ordered vector of participant observations and `groups` is 
a corresponding vector of IP groups. For example, say we had the following set 
of events

| Participant | IP group | Diarrheal episode (Ever/Never) | Diarrhea_Num |
|:-----------:|:--------:|:------------------------------:|:------------:|
|      A      |    IP2   |               Yes              |       1      |
|      B      |   IP2+Y  |               Yes              |       1      |
|      C      |   IP2+Y  |               No               |       0      |
|      D      |    IP1   |               Yes              |       1      |
|      E      |   IP7+Y  |               No               |       0      |
|      F      |  Placebo |               No               |       0      |
|      G      |    IP7   |               Yes              |       1      |
|      H      |    IP2   |               Yes              |       1      |
|      K      |    IP7   |               No               |       0      |

then `events={1,1,0,1,0,...}` and 
`groups={"IP2", "IP2+Y", "IP2+Y", "IP1", "IP7+Y", ...}`. Note that `groups` can 
be a vector of either numbers or strings, but `events` must be a vector of 
numbers

### `permgp_fn`
function to permute the groups

### `events_to_num`

### `global_permutation_test`

### `pairwise_permutation_tests`

### `step_down_test`

### `uniquify_staffer`


usethis::edit_r_environ()
GITHUBTOKEN=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

 devtools::install_github("brendinho/SepsisTools")

