# **R Shiny app with SAS Viya**
The repository supports the SAS Support Community Library article Creating an R Shiny app with SAS Viya (link coming soon). Please refer to the article for the details of how to set up and run the application in your environment.

## Overview

There is not one solution or programing language that will deliver everything you need. That is why customizing the platform you already master is where you find value. In order to accomplish this, I've seen customers move directly to coding, in lieu of SAS user interfaces. This provides freedom and full control on how things get executed. This holds true for analytics and SAS Viya. 

This app, written in R Shiny, is a low-code solution that displays SAS integration through APIs with open source languages.

The interface developed in this repository will allow you to:<br/>
1. import data to CAS <br/>
2. build features <br/>
3. train models.<br/>

At the end, the user will have an end-to-end pipeline to test different approaches.

### Prerequisites

R shiny package https://cran.r-project.org/web/packages/shiny/index.html <br/>
R-swat package https://github.com/sassoftware/R-swat/releases <br/>
Dplyr package https://cran.r-project.org/web/packages/dplyr/index.html <br/>
ggplot2 package https://cran.r-project.org/web/packages/ggplot2/index.html <br/>
Plotly package https://cran.r-project.org/web/packages/plotly/index.html <br/>
### Installation

Clone this repo to your machine.

Edit the hostname / port / username / password on line 23 to match your CAS engine.

### Running

On RStudio, run the code.

Shiny will expose the app either on the RStudio interface or on your browser.


## Contributing

> We welcome your contributions! Please read [CONTRIBUTING.md](CONTRIBUTING.md) for details on how to submit contributions to this project. 

## License

> This project is licensed under the [Apache 2.0 License](LICENSE).
