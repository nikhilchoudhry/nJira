# nJira

SQL like query interface for [JIRA](https://www.atlassian.com/software/jira) in R language

This package provides a SQL like query interface to fetch data from any [JIRA](https://www.atlassian.com/software/jira) installation. The data is fetched into R dataframe (using [JIRA REST API](https://developer.atlassian.com/cloud/jira/platform/rest/v2/))

Key features:
  * Data is fetched using [JIRA REST API](https://developer.atlassian.com/cloud/jira/platform/rest/v2/) therefore can be used over web without needing to connect to any database.
  * Function to authenticate into JIRA instance using `jira.login()`
  * Function to fetch METADATA from JIRA instance using `jira.metadata()`
  * Function to QUERY jira data with SQL like syntax using `jira.query()`
  
  
## Installation

To get the current development version from github:

```R
# install.packages("devtools")
devtools::install_github("nikhilchoudhry/nJira")
```

## Code of Conduct