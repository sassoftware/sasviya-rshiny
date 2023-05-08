# R Shiny app with SAS Viya

The repository supports the SAS Support Community Library article [Creating an R Shiny app with SAS Viya](https://communities.sas.com/t5/SAS-Communities-Library/Creating-an-R-Shiny-app-with-SAS-Viya/ta-p/649029). Please refer to the article for the details of how to set up and run the application in your environment.

## Overview

There is not one solution or programming language that will deliver everything you need. That is why customizing the platform you already master is where you find value. In order to accomplish this, I've seen customers move directly to coding, in lieu of SAS user interfaces. This provides freedom and full control on how things get executed. This holds true for analytics and SAS Viya.

This app, written in R Shiny, is a low-code solution that displays SAS integration through APIs with open source languages.

The interface developed in this repository will allow you to:

1. import data to CAS
2. build features
3. train models

At the end, the user will have an end-to-end pipeline to test different approaches.

### Prerequisites

You will need to have the following packages installed to run the R script that launches the app:  

* [{shiny}](https://cran.r-project.org/web/packages/shiny/index.html)
* [{dplyr}](https://cran.r-project.org/web/packages/dplyr/index.html)
* [{ggplot2}](https://cran.r-project.org/web/packages/ggplot2/index.html)
* [{plotly}](https://cran.r-project.org/web/packages/plotly/index.html)
* [{reshape2}](https://cran.r-project.org/web/packages/reshape2/index.html)
* [{fs}](https://cran.r-project.org/web/packages/fs/index.html)
* [{swat}](https://github.com/sassoftware/R-swat#installation) *(Note: this package is not on CRAN)*

If you don't have the first six packges installed already, you can do so by running the following commands in your RStudio console:

`install.packages(c("shiny", "dplyr", "dplyr", "ggplot2", "plotly", "reshape2", "fs"))`  

Then, to install the {swat} package, check for the latest release on [Github](https://github.com/sassoftware/R-swat/releases/latest), and substitute that version for the *"X.X.X"* in the following command:

`install.packages('https://github.com/sassoftware/R-swat/archive/vX.X.X.tar.gz', repos = NULL, type = 'file')`

### Installation

Clone this repo to your machine.

### Running

Edit the *hostname / port / username / password* in the [app.R](app.R) script's server code. Then run the app script by either clicking "Run App" in RStudio while in the **app.R** script, or running `shiny::run_app("app.R")` from the console.

## Contributing

> We welcome your contributions! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to submit contributions to this project.

## License

> This project is licensed under the [Apache 2.0 License](LICENSE).
