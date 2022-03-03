# Simulations of simple ODE models in biology

These Shiny apps were written with sole purpose of teaching modeling in Biology. 

The models provided are:

1. Simple bistable system [*Bistability.R*]
2. Glycolysis model of Selkov (oscillations) [*Selkov_Glycolysis.R*]
3. The Goodwin oscillator [*Goodwin.R*]
4. Gene driven by oscillatory transcription factor [*Clock_driven_gene.R*]

### Installation and execution

1. Install R from [https://www.r-project.org](https://www.r-project.org)

2. Install RStudio Desktop from [www.rstudio.com](https://www.rstudio.com/products/rstudio/)

3. Download the model files using one of the following two options:

    a. In a terminal, if you have ``git`` installed, 
    run ``git clone https://github.com/bharathananth/models_for_teaching.git``
       
    b. Click on the **green** button marked Code on the github page and select **Download ZIP**. Then unzip it.
    
4. Run the commands below inside RStudio to access the models
```r
install.packages(c("deSolve","tidyverse","magrittr","gridExtra","shiny"))
runApp('<Name of model .R file>')
# It is often convenient to the click on 'Open in Browser' button in the new window to view the app in your browser.
```

#### License
This work is shared under a GPLv3 license.
