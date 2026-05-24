# StarVZ Kiter Visualization

This module provides tools for visualizing execution traces using **Kiter** plots, a technique designed to represent iterative applications (like those using StarPU) by aggregating load data over time slices.

## Features

* **Custom ggplot2 layers:** Easily integrate kiter plots into your existing `ggplot2` workflows.
* **Performance-optimized:** Includes C++ integration via `Rcpp` for efficient time-aggregation calculations.
* **Ready-to-use Environment:** Includes a Singularity definition file for quick deployment of the full StarPU/StarVZ analysis environment.

## Getting Started

### Prerequisites

* R (with `tidyverse` and `Rcpp` packages)
* [StarVZ](https://github.com/schnorr/starvz)
* For containerized usage: [Singularity/Apptainer](https://apptainer.org/)

### Building the Environment

If you are using the provided container definition, you can build the image with:

```bash
sudo singularity build starvz.sif starvz-starpu.def
```

## Usage

The module exposes `geom_kiter` and `scale_kiter` as custom ggplot2 layers. Below is a minimal example:

```r
library(ggplot2)
library(starvz)
source("src/kiter.r")

# Load your traces
df <- starvz_read("./profs", selective = FALSE)

# Visualize
ggplot(df$Application) +
  geom_kiter(factor = 1, slice_size = 10) +
  geom_kiter_makespan(size = 5) +
  scale_kiter()
```

## Project Structure

* `src/kiter.r`: Main R logic for data transformation and ggplot2 layer definitions.
* `src/integrate_step.cpp`: C++ back-end for performant time-series integration.
* `starvz-starpu.def`: Singularity definition file for environment reproducibility.

