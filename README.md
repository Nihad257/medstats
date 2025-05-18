# medstats

An R package for basic statistical analysis in medical research, developed at the National Workshop on R Package Development in Thiruvananthapuram.

## Installation

```R
devtools::install_github('Nihad257/medstats')
library(medstats)
```

## Features

- `calc_ttest`: Perform two-sample t-tests to compare group means.
- `corr_analysis`: Calculate Pearson correlation between variables.

## Example

```R
data(mtcars)
mtcars$group <- ifelse(mtcars$am == 0, 'Automatic', 'Manual')
calc_ttest(mtcars, mpg, group)
```

See the vignette for more details: `vignette('medstats-intro')`
