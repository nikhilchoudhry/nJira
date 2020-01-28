##############################################################################
# Functions to get data and push into JIRA using REST API
##############################################################################

# Declaring global variables
pkg.globals <- new.env()
pkg.globals$.jiraEnv <- ""
pkg.globals$.jiraUser <- ""
pkg.globals$.jiraPwd <- ""
pkg.globals$.jiraVal <- ""
pkg.globals$.issueFields <- ""
pkg.globals$.logs <- F

#' Jira Login Function
#'
#' This function authenticates the user to fetch data from respective JIRA environment.
#'
#' @param jira.env Web address of JIRA environment (e.g. https://issues.apache.org/jira)
#' @param jira.user Jira User Name
#' @param jira.pwd Jira Password
#' @param jira.val 0/1 how should the list values be returned in the query results.
#' @return The function autheticates into JIRA environment..
#' @examples
#' jira.login(jira.env="https://issues.apache.org/jira", 
#' jira.user="jiraTestUser", jira.pwd="jiraTestPwd")

jira.login <- function(jira.env = NULL, jira.user = NULL, jira.pwd = NULL, jira.val = 0, .logs = F) {
  
  options(warn = -1)
  pkg.globals$.jiraEnv <- jira.env
  pkg.globals$.jiraUser <- jira.user
  pkg.globals$.jiraPwd <- jira.pwd
  pkg.globals$.jiraVal <- jira.val
  pkg.globals$.logs <- .logs
  
  # Return if blank value is passed in global variables
  if (pkg.globals$.jiraEnv == "") {return(.logTrace("You have not yet authenticated into Jira Environment using Jira.Login() function"))}

  ## Check if live JIRA session exists
  resp <- GET(paste(pkg.globals$.jiraEnv, "/rest/auth/1/session", sep = ""))
  if(resp$status_code == 401) {
    # Clear any previous issueFields cache
    if (exists("pkg.globals$.issueFields")) {rm(pkg.globals$.issueFields)}
    .logTrace("JIRA session inactive or expired. Sending login request")
    resp <- POST(paste(pkg.globals$.jiraEnv, "/rest/auth/1/session", sep = ""), authenticate(pkg.globals$.jiraUser, pkg.globals$.jiraPwd), add_headers("Content-Type" = "application/json"))
    if(resp$status_code == 400) {.logTrace("JIRA Login Done")} else {.logTrace("JIRA Login Failed")}
  } else if(resp$status_code == 200) {.logTrace("Jira session active.")}

  ## Cache the Jira Issue Fields that is used in various function
  if (!exists("pkg.globals$.issueFields")) {
    pkg.globals$.issueFields <- .jira.issues.fields()
    .logTrace("Jira fields cached", pr = F)
  }
}

#' Jira Tables and Field Details
#'
#' The function returns the list of tables, fields, and their descriptions.
#'
#' @param table Name of the Jira tables. If not specified, all the tables of the given interface are returned.
#' @param fields List of field names whose details are required. If not specified, all the fields of the specified tables are returned.
#' @return The function returns the jira table details.
#' @examples
#' fields <- jira.metadata()
#' fields <- jira.metadata(table = "history")
#' fields <- jira.metadata(table = "issues")
#' fields <- jira.metadata(table = "issues", fields = c("Created", "Date Required", "Dev Status"))

jira.metadata <- function(table = NULL, fields = NULL) {
  jira.login(pkg.globals$.jiraEnv, pkg.globals$.jiraUser, pkg.globals$.jiraPwd, pkg.globals$.jiraVal)
  return(rk.metadata(table = table, fields = fields, gettabs = .jira.tables, getflds = .jira.fields, infofile = "jira"))
}

#' Jira Query Interface
#'
#' The function returns the query data from Jira as a dataframe.
#'
#' For querying the JIRA 'history' table, the where clause must specify the issue 'id' \cr
#' Example : \code{where = "id = 'HIVE-22692'"}
#'
#' @param table Name of Jira table from which data will be fetched.
#' @param fields Comma separated names of the fields from the specified table whose values will be fetched.
#' @param where specifies the where clause of the query. You can also pass your JIRA JQL as-is in the where clause.
#' @param groupby specifies the list of fields on which the data is grouped.
#' @return The function returns the Jira query result as a dataframe.
#' @examples
#' issues <- jira.query(table = "issues", fields = "id AS IssueId, Created, Status, Priority", 
#' where = "project = 'HIVE' AND created >= '2019-01-01' AND created <= '2019-12-31' AND 
#' Status IN ('Open', 'Closed', 'Resolved')")
#'
#' issues <- jira.query(table = "issues", fields = "id AS IssueId, Created", 
#' where = "'cf[10021]' = 'ABCD' AND Created > '2019-01-01'")
#'
#' history <- jira.query(table = "history", where = "id = 'HIVE-22692'")
#'
#' history <- jira.query(table = "history", fields = "id AS IssueId, toString AS Status, 
#' COUNT(fromString) AS Count", where = "id = 'HIVE-22692' AND field = 'status'", 
#' groupby = "id,toString")

jira.query <- function(table, fields = NULL, where = NULL, groupby = NULL) {
  jira.login(pkg.globals$.jiraEnv, pkg.globals$.jiraUser, pkg.globals$.jiraPwd, pkg.globals$.jiraVal)
  result <- data.frame()
  if (table == "issues") {
    if (is.null(where) ) {
      stop("The where clause condition is mandatory to fetch data from JIRA 'issues' table")
    }
    if (is.null(fields)) {flds = "ALL"} else {flds <- gsub("'", "", rk.fields(fields, mode = ""))}
    result <- .jira.search.issue(query = rk.where(where, "~"), fields = flds, maxresults = 10000000)
    if (nrow(result) & !is.null(fields)) {
      if (flds != "ALL") {
        ## Renme column names as the JIRA query function changes the user supplied field names into alias names from JIRA and it would not work with rk.query
        nord <- .jira.fields.map(unlist(strsplit(flds, ",")), toAlias = T)
        ### (to be removed) Incase if some selective fields in the query are not returned in the result, we add the corrosponding column names with null value
          l <- nord[!nord %in% names(result)]
          if (length(l) > 0) {
           tmpdf <- data.frame(matrix(ncol=length(l), nrow = nrow(result)))
           names(tmpdf) <- l
           tmpdf[is.na(tmpdf)] <- "NULL"
           result <- cbind(result, tmpdf)
          }
        ####
        result <- result[, nord]
        names(result) <- strsplit(flds, ",")[[1]]
        ##
        if (nrow(result) > 0) {result <- rk.query(result, fields, where = NULL, groupby)}
      }
    }

    return(result)
  }

  if (table == "history" || table == "comments") {
    if (is.null(where) || (k <- regexpr("^\\s*id\\s*=\\s*'[^']+'(\\s*AND)?\\s*", where, ignore.case = F, perl = T)) <= 0) {
      stop(paste("The where clause of JIRA '", table, "' table must select the 'id' of the issue for which details are required.", sep = ""))
    }
    qwhere <- rk.where(where, "=", .jira.searchable(table))
    id <- unlist(strsplit(rk.where(where, "=", .jira.searchable(table)), "="))[2]
    if (table == "history")
      result <- .jira.issue.changelog(id)
    else
      result <- .jira.issue.comments(id)

    n <- attr(k, "match.length")
    where <- substr(where, n + 1, nchar(where))
    if (nrow(result)) {result <- rk.query(result, fields, where = where, groupby)}
    return(result)
  }
}
