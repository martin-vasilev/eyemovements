
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eyemovements

<!-- badges: start -->

<!-- badges: end -->

Tools for processing raw eye-tracking samples and detecting fixations
and saccades.

## Installation

You can install the package from Github by running the following code:

``` r
# Check if "devtools" is installed:
if('devtools' %in% rownames(installed.packages())==FALSE){
    install.packages('devtools')
    library(devtools)
  }else{
    library(devtools)
  }
devtools::install_github("martin-vasilev/eyemovements")
```

## Sample data:

The package has some in-built datasets to use as an example:

1.  `"Oz" dataset`, containing the raw samples of a participant reading
    a page from the “Little Wizard Stories of Oz” (Slattery & Vasilev,
    2019). Data was recorded with an Eyelink 1000+ at 1000 Hz
    (monocular, from the right eye):

``` r
library(eyemovements)
dat= data_Oz
str(dat)
#> 'data.frame':    32586 obs. of  4 variables:
#>  $ time : num  1322956 1322957 1322958 1322959 1322960 ...
#>  $ x    : num  962 962 962 962 962 ...
#>  $ y    : num  541 541 541 540 539 ...
#>  $ pupil: num  1753 1752 1749 1747 1746 ...
```

When using your own data, note that it should contain the same columns
(`time`, `x`, and `y`). At the moment, binocular data is not supported,
but as a temporary solution, you can average out the x and y positions
of the two eyes, take just the right eye, or run the data separately for
each eye.

## Event detection using the Identification by Velocity Threshold (I-VT) method

Some saccade detection algorithms such as I-VT require the use of
degrees of visual angle (dva). For these to work, the degrees need to be
calculated for your design. For this you need to know:

- The physical monitor size in cm, provided as c(x, y) vector
- Screen resolution in pixels, provided as c(x, y) vector
- Viewing distance (in cm)- distance between the participant’s eye and
  the monitor

These can then be used in the `VisualAngle()` function to calculate the
dva per pixel:

``` r

deg<- VisualAngle(screen_size_cm = c(x = 53, y = 30),
  resolution_px = c(x = 1920, y = 1080),
  viewing_dist_cm = 82)
deg
#>   axis screen_cm resolution_px deg_per_px size_px   size_deg
#> 1    x        53          1920 0.01865554       1 0.01865554
#> 2    y        30          1080 0.01919689       1 0.01919689
```

Using the dva values above, we can parse the data for fixations/
saccades using the `IVT()` function. The function outputs fixations by
default (`return_saccade= FALSE`). However, saccade data can be
alternatively outputted by setting `return_saccades= TRUE`. The
sensitivity of the saccade detection algorithm is determined by the
`vel_threshold` argument (in degrees per second). By default, this is
set to 30 deg/s (same default that Eyelink uses for “cognitive
research”). If working with more oculomotor tasks (e.g.,
pro-saccade/anti-saccade), this can be set a bit higher (e.g., 35-40
deg/s). However, users can change it to any value. Please note that some
data smoothing is recommended before parsing the data to improve
performance of the algorithm. Sometimes the algorithm can detect very
short fixations, mostly due to noise. Users can specify the minimum
fixation duration to be detected (`min_fix_dur`). By default, this is
set to 50 ms (i.e., all fixations 0-49 ms are discarded). However, users
can turn it off if they prefer (`min_fix_dur= NULL`).

To detect fixations, we can put the desired arguments together with the
Oz data:

``` r
fix<- IVT(data = dat,
          dva_x = 0.01865554,
          dva_y = 0.01919689,
          vel_threshold = 30,
          min_fix_dur = 50)

head(fix)
#> # A tibble: 6 × 8
#>   fix_id fix_start fix_end     x     y fix_dur avg_velocity n_samples
#>    <int>     <dbl>   <dbl> <dbl> <dbl>   <dbl>        <dbl>     <int>
#> 1      1   1322956 1323246  962.  538.     290         9.88       291
#> 2      2   1323324 1323528  517.  217.     204         9.45       205
#> 3      3   1323553 1323749  589.  199.     196         8.92       197
#> 4      4   1323757 1323832  587.  193.      75        11.2         76
#> 5      5   1323835 1323926  588.  195.      91         9.45        92
#> 6      6   1323947 1324166  680.  196.     219         9.76       220
```

The function outputs the following variables for fixations:

- `fix_id`: fixation ID in the detected sequence
- `fix_start`: start of fixation timestamp
- `fix_end`: end of fixation timestamp
- `x`: mean x pixel coordinate of the samples making up the fixation
- `y`: mean y pixel coordinate of the samples making up the fixation
- `fix_dur`: fixation duration (defined as `fix_end`- `fix_start`)
- `avg_velocity`: average velocity (in deg/s) of the samples making up
  the fixation
- `n_samples`: number of samples making up the fixation

To detect saccades, we can use the same arguments but set
`return_saccades` to `TRUE`. Note that the function still performs
filtering of very short saccades that are likely noise. This is done
with the `min_sacc_amplitude` argument. By default, saccades that are
shorter than 0.15 deg in amplitude are discarded (same default that the
Eyelink parser uses). However, this can be turned off by setting
`min_sacc_amplitude` to `NULL` or `0`.

``` r

sacc<- IVT(data = dat,
          dva_x = 0.01865554,
          dva_y = 0.01919689,
          vel_threshold = 30, 
          return_saccades = TRUE)

head(sacc)
#> # A tibble: 6 × 12
#>   sacc_id sacc_start sacc_end sacc_dur x_start y_start x_end y_end
#>     <int>      <dbl>    <dbl>    <dbl>   <dbl>   <dbl> <dbl> <dbl>
#> 1       1    1323247  1323293       46    960.    535.  513.  190.
#> 2       2    1323311  1323315        4    505     190.  509.  197.
#> 3       3    1323538  1323549       11    526.    222.  582.  204.
#> 4       4    1323927  1323946       19    590.    195.  669.  198.
#> 5       5    1324167  1324189       22    679.    195.  776.  198.
#> 6       6    1324370  1324386       16    784.    206.  846.  203.
#>   sacc_amplitude_deg avg_velocity_deg peak_velocity_deg n_samples
#>                <dbl>            <dbl>             <dbl>     <int>
#> 1             10.7              230.              377.         47
#> 2              0.150             37.9              42.4         5
#> 3              1.09              98.9             135.         12
#> 4              1.46              78.0             140.         20
#> 5              1.81              82.5             147.         23
#> 6              1.16              70.6             123.         17
```

The function outputs the following variables for fixations:

- `sacc_id`: saccade ID in the detected sequence
- `sacc_start`: start of saccade timestamp
- `sacc_end`: end of saccade timestamp
- `sacc_dur`: saccade duration (defined as `sacc_end`- `sacc_start`)
- `x_start`: horizontal gaze position at saccade onset, in pixels.
- `y_start`: vertical gaze position at saccade onset, in pixels
- `x_end`: horizontal gaze position at saccade offset, in pixels
- `y_end`: vertical gaze position at saccade offset, in pixels
- `sacc_amplitude_deg`: saccade amplitude in degrees of visual angle,
  computed from the horizontal and vertical displacement between saccade
  onset and offset
- `avg_velocity_deg`: mean sample-to-sample velocity during the saccade,
  in deg/s
- `peak_velocity_deg`: maximum sample-to-sample velocity during the
  saccade, in degrees/second
- `n_samples`: number of samples contributing to the saccade
