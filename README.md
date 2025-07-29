# StudentInfoAnalytics
# Binghamton University Student Survey Analytics

Interactive Shiny application for analyzing student survey responses across demographic categories.

## Features

- **Bar Graph**: Single population analysis
- **Diverging Bar Chart**: Two population comparison  
- **Demographic Answer Analysis**: Demographic breakdown ordered by response rates
- **Flexible Analysis**: Choose any demographic, question, or response to analyze

## Quick Start

### Required packages
```r
install.packages(c("shiny", "bslib", "fontawesome", "ggplot2", 
                   "shinyWidgets", "shinyjs", "dplyr", "tidyr", 
                   "gridExtra", "readxl"))
```

## Deployment (Posit)

### shinyapps.io (Free)
```r
library(rsconnect)

rsconnect::setAccountInfo(name='your-account', 
                         token='your-token',
                         secret='your-secret')

rsconnect::deployApp()
```

### Posit Connect (Enterprise)
```r
library(rsconnect)

rsconnect::addServer("https://your-posit-connect-server.com")

rsconnect::deployApp(server = "your-server")
```

## Usage

1. **Select Questions** from a category
2. **Choose Graph Type**
3. **Filter Students** using sliders
4. **Analyze Results**

## Author
Bryan Luke Shabroski

