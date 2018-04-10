# front_line_workforce
Summary data and replication scripts for Edwards, Frank and Christopher Wildeman, 2018,"Characteristics of the front-line child welfare workforce", *Children and Youth Services Review*. [doi:10.1016/j.childyouth.2018.04.013](https://doi.org/10.1016/j.childyouth.2018.04.013)

In the folder .\data we have created a series of state and state-year level summary files using a restricted-access version of the [National Child Abuse and Neglect Data System](https://www.acf.hhs.gov/cb/research-data-technology/reporting-systems/ncands), made available by the US Children's Bureau, in collaboration with the [National Data Archive on Child Abuse and Neglect](https://ndacan.cornell.edu). 

For more detailed descriptions of the measures, please see the paper. For those without library access, a pre-print is available here SOCARXIV LINK.

## DATA FILES

### state_capacity.csv

Data used to calculate the capacity of front-line child welfare workforces at the state-year level

- StaTerr: Two letter state abbreviation
- year: Year of data submission
- chpov: Child population below the federal poverty line (source, Census [SAIPE API](https://www.census.gov/programs-surveys/saipe/data/api.html))
- caseworkers: count of unique valid caseworker identifiers in NCANDS Child File (see paper for inclusion criteria) 
- supervisors: count of unique valid supervisor identifiers in NCANDS Child File
- screened_in_reports: count of unique reports that recieved a child welfare agency response

### state_wrkr_data.csv

Summary of caseworker-level data at the state-level. Values in this file are summaries of all observed caseworkers for all valid years of the NCANDS data between 2003 and 2015 for each state. See the paper for descriptions of how measures are calculated.

- StaTerr: Two letter state abbreviation
- year: Year of data submission
- type: Type of worker (Caseworker, Supervisor)
- cases_median: Total cases assigned to workers, median 
- cases_75thPct: Total cases assigned to workers, 75th percentile
- cases_25thPct: Total cases assigned to workers, 25th percentile
- cases_mean: Total cases assigned to workers, mean
- cases_sd: Total cases assigned to workers, standard deviation
- tenure_median_yrs: Length of time (in years) between last and first assigned case, median
- tenure_75thPct: Length of time (in years) between last and first assigned case, 75th percentile	
- tenure_25thPct: Length of time (in years) between last and first assigned case, 25th percentile
- tenure_mean: Length of time (in years) between last and first assigned case, mean
- tenure_sd: Length of time (in years) between last and first assigned case, standard deviation	
- caseload_median: Average annual number of cases assigned (cases/tenure), median
- caseload_75thPct: Average annual number of cases assigned (cases/tenure), 75th percentile
- caseload_25thPct: Average annual number of cases assigned (cases/tenure), 25th percentile
- caseload_mean: Average annual number of cases assigned (cases/tenure), mean
- caseload_sd: Average annual number of cases assigned (cases/tenure), standard deviation

### turnover_state_year.csv
- StaTerr: Two letter state abbreviation
- year: Year of data submission
- lower.turnover.caseworker: Annual caseworker turnover (lower-bound), based on maximum promotion assumption (see paper for more detail)
- upper.turnover.caseworker: Annual observed caseworker turnover (upper-bound)
- turnover.supervisor: Annual supervisor turnover (no management-level employment data available)

## Replication scripts

These scripts are provided for the purpose of replication, reference, and peer-review. They depend upon access to restricted version of the NCANDS Child File.

### read_ncands.r

Processes full NCANDS Child File, selects variables needed for analysis and ouputs annual child file with select variables.

### transform_data.r

Processes output of read_ncands.r, screens worker identifiers based on exclusion criteria, computes all measures reported in the paper.

### visuals.r

Produces all figures for the paper, produces all descriptive statistics included in the paper. Produces summary files for public release.
