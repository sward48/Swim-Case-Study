# Swim-Case-Study

In order to run this app and render the pdf files, you must follow these instructions:

First, in RStudio, set your working directory to "your_path/swim-case-study". Then ensure you have all the required packages by checking libraries.R. Once you have all the packages installed, you should be good to run and render. (If you have all the libraries and are still having trouble rendering the .qmd files, you should be able to render as HTML, then from there save as a PDF).

In order to run the app, go to app.R and click the run button in RStudio. 

The only other step required is to have the proper folder structure.

The folder structure is as follows:


```text
swim-case-study/
├── app.R
├── R/ 
│   ├── libraries.R
│   ├── prepdata.R
│   ├── analysis.R
├── data/      
│   ├── Rawdata.csv
│   ├── Data.rds
├── swimming_report.qmd
└── executive_summary.qmd
```
