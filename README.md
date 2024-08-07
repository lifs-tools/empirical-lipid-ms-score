# EPoS-MoL

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12632087.svg)](https://doi.org/10.5281/zenodo.12632087)
[![R-CMD-check](https://github.com/lifs-tools/empirical-lipid-ms-score/actions/workflows/check-standard.yaml/badge.svg?branch=main)](https://github.com/lifs-tools/empirical-lipid-ms-score/actions/workflows/check-standard.yaml)

While the official shorthand nomenclature of lipids is a first and important step towards a reporting quality tool, an additional point score reflects the quality of reported data at an even more detailed granularity. Thus, the **E**mpirical **Po**int **S**core **Mo**del for MS-based **L**ipidomics (EPoS-MoL) is a lipidomics scoring scheme which takes into account all the different layers of analytical information to be obtained by mass spectrometry, chromatography and ion mobility spectrometry and awards scoring points for each of them.

EPoS-MoL is an R-package and also provides a Shiny app for the calculation of the EPoS-MoL score.

## Installing the package

The package can be installed from GitHub using the following command:

```devtools::install_github("lifs-tools/empirical-lipid-ms-score")```

If you want to install a particular release version, use the following command:

```devtools::install_github("lifs-tools/empirical-lipid-ms-score@v0.9.10")```

in this case for the tag version v0.9.10

## Using the package

Please see the package vignette for a detailed description of the package functionality.

```vignette("eposmol", package = "eposmol")```

**Documentation** is also available at [https://lifs-tools.github.io/empirical-lipid-ms-score/](https://lifs-tools.github.io/empirical-lipid-ms-score/)

## Accessing the Shiny Webapp

The EpOs-MoL Shiny app can be accessed at the following URL:

```https://apps.lifs-tools.org/p/app/eposmol```

## Running the app from RStudio

In order to load the packages, without the need to build and install it:

```
devtools::load_all()
```

You can then use the regular "Run App" button within RStudio to launch the Shiny app.

Alternatively, you can install the package, as shown above, then load it and run:

```
library(eposmol)
run_eposmol_app()
```

After that, the app is accessible from your browser. Please check your R console output for the exact URL and port. The following is just an example. The port may be different on your system: 

```
Listening on http://127.0.0.1:6097
```

## Building the Docker image

The Docker image can be built using the following command:

```docker build -t eposmol .```

If you use buildx, you will need to instruct the image builder to load the created image into the local docker:

```docker build --load -t eposmol .```

## Running the Docker image

The Docker image can be run using the following command:

```docker run -p 3838:3838 eposmol```

The Shiny app will then be available at http://localhost:3838/eposmol/

Alternatively, you can use the provided compose file to automatically sync local changes during development to the RShiny application. You will need to reload your browser for changes to become effective:

```./run.sh```

Or use docker compose directly:

```docker compose -f docker-compose-eposmol.yml up --watch```

The Shiny app will then be available at http://localhost:3838/eposmol/
