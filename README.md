# Replication Code for "Gender Convergence in Earnigns and Changing Pathways towards Intergenerational Income Persistence"

## Nino Cricco

These files contain the replication code for the article "Gender Convergence in Earnigns and Changing Pathways towards Intergenerational Income Persistence", the first article from a broader dissertation project. I use publicly available data from the Panel Study of Income Dynamics (PSID), which are available to researchers at <https://psidonline.isr.umich.edu/>. This work is ongoing and files are continually updated. 

## Code Files

### Setup
- `0-libraries.R`: Read in the required libraries
- `0-functions.R`: Creates custom data cleaning functions that are used across the processing and analysis files

### Data Processing
- `0-raw.R`: This file produces respondent-by-year data from the raw PSID data. Instructions on how to download the requisite PSID data cart from the website are includede in the file. 
- `1-generate-analysis-data.R`: This file creates a function that generates the cleaned analysis data. The function includes arguments that allows the user to customize certain sample specifications and analytical choices

### Data Analysis
- `2-estimate-parameters.R`: This file creates a function that estimates all of the requisite parameters for the analysis, either pooled or by marital status subgroup
- `3-tables-function.R`: This file creates functions that generate data objects and LaTeX code for all of the tables in the paper
- `4-figures-function.R`: This file creates functions that create all of the figures in the paper, as well as their associated values
- `5-generate-figures.R`: This file generates all of the figures in the paper and the appendix by mapping the figure generating functions to data with different sample restrictions and analytical specifications. 