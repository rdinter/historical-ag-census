# United States Censuses of Agriculture for the years 1840 to 2012

Data are available from [ICSPR Study 35206](https://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/35206) and this repository is meant to deal with the `Delimited` version of the data. The .zip file is over 1.1 gigabytes and needs to be placed in the [0-data/raw](0-data/raw) folder.



The [historical_farmland.R](historical_farmland.R) is one example of extracting data from the zip files. The variables which are to be extracted have been identified in the [vars-wide.csv](vars-wide.csv) file, although it took a considerable amount of time to identify the variable names in the documentation. Codebooks are found within each .zip file to be downloaded off of the ICPSR site.

## Citation

Haines, Michael, Fishback, Price, and Rhode, Paul. United States Agriculture Data, 1840 - 2012. Ann Arbor, MI: Inter-university Consortium for Political and Social Research [distributor], 2018-08-20. https://doi.org/10.3886/ICPSR35206.v4