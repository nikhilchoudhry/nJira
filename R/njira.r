##############################################################################
# Functions to get data and push into JIRA using REST API
##############################################################################

## Initialize the nJira Library and resolve the dependencies
.jira.init <- function() {

  if (exists(".nJiraInit")) {return()}

  options(warn = -1)
  if (!require("httr")) {
    install.packages("httr", quiet = TRUE, repos = "http://cran.rstudio.com")
    library("httr")
  }
  if (!require("rjson")) {
    install.packages("rjson", quiet = TRUE, repos = "http://cran.rstudio.com")
    library("rjson")
  }
  if (!require("plyr")) {
    install.packages("plyr", quiet = TRUE, repos = "http://cran.rstudio.com")
    library("plyr")
  }
  .logTrace(paste("jira: Jira library initialized"))
  .nJiraInit <<- 1
}

# Internal nJira function to fetch the list of Jira Tables
.jira.tables <- function() {
  # Specify virtual tables (represented by a leading '.')
  stdtab <- c("issues", "history", "comments")
  return(sort(unique(c(stdtab))))
}

# Internal nJira function to fetch the list of fields by Jira tables
.jira.fields <- function(table) {
  if (table == "history") {
    return(sort(unique(c("field", "fieldtype", "from", "fromString", "to", "toString", "author", "name", "created", "id"))))
  }
  if (table == "issues") {
    return(sort(unique(c(append(.issueFields$name, "id")))))
  }
  if (table == "comments") {
    return(sort(unique(c("author", "comment", "createdDate"))))
  }
}

# Internal nJira function to define the list of searchable fields by table
.jira.searchable <- function(table) {
  if (table == "history" || table == "comments") {
    return(c("id"))
  }
}

# Internal nJira function to fetch the fields list from issues table of Jira
.jira.issues.fields <- function(default=F) {
  .logTrace(paste("jira: Fetch the fields list of isuues table"))
  .logTrace(paste("jira: Running Fields Query: ", .jiraEnv, "/rest/api/2/field", sep=""), pr = F)
  resp <- GET(paste(.jiraEnv, "/rest/api/2/field", sep=""), add_headers("Content-Type" = "application/json"))
  if (length(content(resp)) <= 0) {return(NULL)}
  df <- data.frame(do.call(rbind,content(resp)))
  df <- df[, !(colnames(df) %in% c("clauseNames", "schema"))]
  df <- as.data.frame(sapply(df, function(x) as.character(x)), stringsAsFactors = F)
  if(default == T){
    resp <- GET(paste(.jiraEnv, "/rest/api/2/user/columns", sep = ""), add_headers("Content-Type" = "application/json"))
    if (length(content(resp)) <= 0) {return(NULL)}
    ddf <- as.data.frame(do.call(rbind, content(resp)))
    df <- df[df$id %in% ddf$value,]
  }
  return(df)
}

# Internal nJira native and custom fields mapping function
.jira.fields.map <- function(fields, toAlias = F) {
  df <- .issueFields
  fieldsNew <- character(0)
  for (fld in fields) {
    if(toAlias == F) {
      if (is.element(fld, df$id)) {fieldsNew <- append(fieldsNew, fld)}
      else if (is.element(fld, df$name)) {
        ## If two fields of same alias name exists and one of them is native field then return native else return all matches
        if (length(df[df$name == fld, "id"]) > 1 & length(df[df$name == fld & df$custom == "FALSE", "id"]) == 1) {
          fieldsNew <- append(fieldsNew, df[df$name == fld & df$custom == "FALSE", "id"])
        } else {fieldsNew <- append(fieldsNew, df[df$name == fld, "id"])}
      }
      else {.logTrace(paste("jira: Following field doesn't exist in JIRA fields -", fld), pr = F)}
    } else {
      if (!is.element(fld, df$id)) {fieldsNew <- append(fieldsNew, fld)}
      else {fieldsNew <- append(fieldsNew, df[df$id == fld, "name"])}
    }
  }
  return(fieldsNew)
}

# Internal nJira Changelog fetch function
.jira.changelogdf <- function(resp) {
  if (length(content(resp)$errorMessages[[1]]) > 0) {
    .logTrace("jira: No Issues found")
    .logTrace(paste("jira: ", content(resp)$errorMessages[[1]]))
    return()
  } else {
    id <- content(resp)$key
    df <- as.data.frame(do.call(rbind, content(resp)$changelog$histories))
    for (r in 1:nrow(df)) {
      cdf <- as.data.frame(do.call(rbind, df$items[[r]]))
      cdf$author <- unlist(df$author[[r]])["name"]
      cdf$name <- unlist(df$author[[r]])["displayName"]
      cdf$created <- df$created[[r]]
      if (!exists("chlog")) {chlog <- cdf} else {chlog <- rbind(chlog, cdf)}
    }
    chlog$id <- id
    chlog <- as.data.frame(lapply(chlog, function(x) as.character(x)), stringsAsFactors = F)
    return(chlog)
  }
}

# Internal nJira function takes an issue Id as an argument and returns its complete changelog/history in a dataframe.
.jira.issue.changelog <- function(id) {
  .logTrace(paste("jira: Fetching changelog of Issue -", id), pr = F)
  .logTrace(paste("jira: Running Changelog Query: ", .jiraEnv, "/rest/api/2/issue/", id, "?expand=changelog", sep=""), pr = F)
  resp <- GET(paste(.jiraEnv, "/rest/api/2/issue/", id, "?expand=changelog", sep=""), add_headers("Content-Type" = "application/json"))
  df <- .jira.changelogdf(resp)
  return(df)
}

# Internal nJira function to fetch comments
.jira.commentsdf <- function(resp) {
  if (length(content(resp)$errorMessages[[1]]) > 0) {
    .logTrace("jira: No comments on the issue found")
    .logTrace(paste("jira: ", content(resp)$errorMessages[[1]]))
    return()
  } else {
    df <- as.data.frame(do.call(rbind, content(resp)$comments))
    df <- df[,c('author','body','created')]
    df$created <- as.character(df$created)
    df$body <- as.character(df$body)

    colnames(df)[colnames(df) == 'body'] <- 'comment'

    df$author <- sapply(df$author, function(x) {
      return(x$name)
    })

    return(df)
  }
}

# Internal nJira function takes an issue Id as an argument and returns its complete comments in a dataframe.
.jira.issue.comments <- function(id) {
  .logTrace(paste("jira: Fetching comments of Issue -", id), pr = F)
  .logTrace(paste("jira: Running comments Query: ", .jiraEnv, "/rest/api/2/issue/", id, "/comment", sep=""), pr = F)
  resp <- GET(paste(.jiraEnv, "/rest/api/2/issue/", id, "/comment", sep=""), add_headers("Content-Type" = "application/json"))
  df <- .jira.commentsdf(resp)
  return(df)
}

# Internal nJira issue search query
.jira.searchqry <- function(query, clean = F) {
  .logTrace(paste("jira: Running Search Query: ", .jiraEnv, "/rest/api/2/search?jql=", query, sep = ""), pr = F)
  resp <- GET(paste(.jiraEnv, "/rest/api/2/search?jql=", query, sep = ""), add_headers("Content-Type" = "application/json"))

  if (length(content(resp)$errorMessages[[1]]) > 0) {
    .logTrace("jira: Error in Query")
    .logTrace(content(resp)$errorMessages[[1]])
    return(NULL)
  }

  if (content(resp)$total == 0 || length(content(resp)$issues) == 0) {
    .logTrace("jira: No Issues found", pr = F)
    return(NULL)
  }

  dm <- as.data.frame(do.call(rbind, content(resp)$issues))
  df <- list()
  for (n in 1:length(dm$fields)) {
    df[[n]] <- as.data.frame(do.call(rbind, dm$fields[n]))
  }
  df <- rbind.fill(df)
  df <- sapply(df, function(clm) {
    lapply(clm, function(x) {
      x <- unlist(x)
      if (.jiraVal == "1") {if ("displayName" %in% names(x)) {if (x[["displayName"]] != "") {return (x[["displayName"]])}}}
      if ("name" %in% names(x)) {
        rval = x[["name"]]
        if ("child.name" %in% names(x)) {rval <- paste(rval, x[["child.name"]], sep = " - ")}
        return (rval)
      }
      if ("value" %in% names(x)) {
        rval = x[["value"]]
        if ("child.value" %in% names(x)) {rval <- paste(rval, x[["child.value"]], sep = " - ")}
        return (rval)
      }
      return(x)
    })
  })

  if (is.vector(df)) {df <- t(as.matrix(df))}
  df <- as.data.frame(df, stringsAsFactors = F)
  df <- as.data.frame(lapply(df, function(x) as.character(x)), stringsAsFactors = F)
  if (clean == T) {df <- Filter(function(x)!all(x == "NULL"), df)}
  colnames(df) <- .jira.fields.map(colnames(df), toAlias = T)
  df$id <- as.character(dm$key)
  return(df)
}

# Internal nJira function takes JIRA search queries related to issues (As you pass them on JIRA) and returns the response in a dataframe.
.jira.search.issue <- function(query, startAt=0, maxresults=NULL, fields = NULL, clean = F) {

  # Replace space in query with %20
  query <- gsub(" ", "%20", query)

  # which fields to fetch in search query
  if (is.null(fields)) {fields <- paste(.jira.issues.fields(default = T)$id, collapse=",")}
  else if (fields == "ALL") {fields <- "*all,-comment" ; clean <- TRUE}
  else {fields <- paste(.jira.fields.map(unlist(strsplit(fields, ","))), collapse=",")}

  results <- data.frame()
  if (!is.null(maxresults)) {
    if (maxresults > 1000) {
      n <- startAt

      # For more than 1000 results, break query into chunks of 1000
      while (n < maxresults) {
        if (maxresults - n <= 1000) {cnt <- maxresults - n} else {cnt <- 1000}
        querytmp <- paste(query, "&startAt=", n, "&maxResults=", cnt, "&fields=", fields, sep="")
        df <- .jira.searchqry(querytmp, clean)
        if (is.null(df)) {break}
        results <- rbind.fill(results, df)
        n <- as.integer(n + 1000)
      }

      # For less than = 1000 max results, get all results in a shot
    } else {
      query <- paste(query, "&startAt=", startAt, "&maxResults=", maxresults, "&fields=", fields, sep="")
      results <- .jira.searchqry(query, clean)
    }
  } else {
    # If maxresult is not provided then get 1000 rows by default
    query <- paste(query, "&startAt=", startAt, "&maxResults=", 1000, "&fields=", fields, sep="")
    results <- .jira.searchqry(query, clean)
  }
  return(results)
}

#' Jira Login Function
#'
#' This function authenticates the user to fetch data from respective JIRA environment.
#'
#' @param jira.env Web address of JIRA environment where data will be queried.
#' @param jira.user Jira User Name.
#' @param jira.pwd Jira Password
#' @param jira.val 0/1 how should the list values be returned in the query results.
#' @return The function autheticates into JIRA environment..
#' @examples
#' jira.login(jira.env = "https://issues.apache.org/jira", jira.user = "TestUser", jira.pwd = "TestPwd")
#'
jira.login <- function(jira.env = NULL, jira.user = NULL, jira.pwd = NULL, jira.val = 0) {

  .jira.init()
  .jiraEnv <<- jira.env
  .jiraUser <<- jira.user
  .jiraPwd <<- jira.pwd
  .jiraVal <<- jira.val

  ## Check if live JIRA session exists
  resp <- GET(paste(.jiraEnv, "/rest/auth/1/session", sep = ""))
  if(resp$status_code == 401) {
    # Clear any previous issueFields cache
    if (exists(".issueFields")) {rm(.issueFields)}
    .logTrace("JIRA session inactive or expired. Sending login request")
    resp <- POST(paste(.jiraEnv, "/rest/auth/1/session", sep = ""), authenticate(.jiraUser, .jiraPwd), add_headers("Content-Type" = "application/json"))
    if(resp$status_code == 400) {.logTrace("JIRA Login Done")} else {.logTrace("JIRA Login Failed")}
  } else if(resp$status_code == 200) {.logTrace("Jira session active.")}

  ## Cache the Jira Issue Fields that is used in various function
  if (!exists(".issueFields")) {
    .issueFields <<- .jira.issues.fields()
    .logTrace("Jira fields cached")
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
  if (!exists(".jiraEnv")) {return(.logTrace("You have not yet authenticated into Jira Environment using Jira.Login() function"))}
  jira.login(.jiraEnv, .jiraUser, .jiraPwd, .jiraVal)
  return(rk.metadata(table = table, fields = fields, gettabs = .jira.tables, getflds = .jira.fields, infofile = "jira"))
}

#' Jira Query Interface
#'
#' The function returns the query data from Jira as a dataframe.
#'
#' For querying the JIRA 'history' table, the where clause must specify the issue 'id' \cr
#' Example : \code{where = "id = 'HIVE-22692'"}
#'
#' @param table Name of Jira Tables from which data is fetched.
#' @param fields Comma separated names of the fields from the specified table whose values are fetched.
#' @param where specifies the where clause of the query. You can pass your JIRA JQL as is in the where clause.
#' @param groupby specifies the list of fields on which the data is grouped.
#' @return The function returns the Jira query result as a dataframe.
#' @examples
#'
#' Fetch Issues from JIRA 'issues' table
#' issuesData <- jira.query(table = "issues", fields = "id, Created, Status, Priority AS IssuePriority",
#' where = "project = 'HIVE' AND created >= '2016-01-01'AND created <= '2019-01-01' AND Status in ('Open', 'Closed', 'Resolved'))
#'
#' jiraData <- jira.query(table = "issues", fields = "id AS IssueId, Created", where = "'cf[10021]' = 'ABCD'
#' AND Created > '2017-01-01'")
#'
#' Fetch Issue History from JIRA 'history' table
#' issueHistory <- jira.query(table = "history", where = "id = 'HIVE-22692'")
#'
#' issueHistory <- jira.query(table = "history", fields = "id AS IssueId,
#' toString AS Status, COUNT(fromString) AS Count", where = "id = 'HIVE-22692'
#' AND field = 'status'", groupby = "id,toString")

jira.query <- function(table, fields = NULL, where = NULL, groupby = NULL) {
  if (!exists(".jiraEnv")) {return(.logTrace("You have not yet authenticated into Jira Environment using Jira.Login() function"))}
  jira.login(.jiraEnv, .jiraUser, .jiraPwd, .jiraVal)
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
