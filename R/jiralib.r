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
    return(sort(unique(c(append(pkg.globals$.issueFields$name, "id")))))
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
.jira.issues.fields <- function(default=FALSE) {
  .logTrace(paste("jira: Fetch the fields list of isuues table"), pr = FALSE)
  .logTrace(paste("jira: Running Fields Query: ", pkg.globals$.jiraEnv, "/rest/api/2/field", sep=""), pr = FALSE)
  resp <- GET(paste(pkg.globals$.jiraEnv, "/rest/api/2/field", sep=""), add_headers("Content-Type" = "application/json"))
  if (length(content(resp)) <= 0) {return(NULL)}
  df <- data.frame(do.call(rbind,content(resp)))
  df <- df[, !(colnames(df) %in% c("clauseNames", "schema"))]
  df <- as.data.frame(sapply(df, function(x) as.character(x)), stringsAsFactors = FALSE)
  if(default == TRUE){
    resp <- GET(paste(pkg.globals$.jiraEnv, "/rest/api/2/user/columns", sep = ""), add_headers("Content-Type" = "application/json"))
    if (length(content(resp)) <= 0) {return(NULL)}
    ddf <- as.data.frame(do.call(rbind, content(resp)))
    df <- df[df$id %in% ddf$value,]
  }
  return(df)
}

# Internal nJira native and custom fields mapping function
.jira.fields.map <- function(fields, toAlias = FALSE) {
  df <- pkg.globals$.issueFields
  fieldsNew <- character(0)
  for (fld in fields) {
    if(toAlias == FALSE) {
      if (is.element(fld, df$id)) {fieldsNew <- append(fieldsNew, fld)}
      else if (is.element(fld, df$name)) {
        ## If two fields of same alias name exists and one of them is native field then return native else return all matches
        if (length(df[df$name == fld, "id"]) > 1 & length(df[df$name == fld & df$custom == "FALSE", "id"]) == 1) {
          fieldsNew <- append(fieldsNew, df[df$name == fld & df$custom == "FALSE", "id"])
        } else {fieldsNew <- append(fieldsNew, df[df$name == fld, "id"])}
      }
      else {.logTrace(paste("jira: Following field doesn't exist in JIRA fields -", fld), pr = FALSE)}
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
    chlog <- as.data.frame(lapply(chlog, function(x) as.character(x)), stringsAsFactors = FALSE)
    return(chlog)
  }
}

# Internal nJira function takes an issue Id as an argument and returns its complete changelog/history in a dataframe.
.jira.issue.changelog <- function(id) {
  .logTrace(paste("jira: Fetching changelog of Issue -", id), pr = FALSE)
  .logTrace(paste("jira: Running Changelog Query: ", pkg.globals$.jiraEnv, "/rest/api/2/issue/", id, "?expand=changelog", sep=""), pr = FALSE)
  resp <- GET(paste(pkg.globals$.jiraEnv, "/rest/api/2/issue/", id, "?expand=changelog", sep=""), add_headers("Content-Type" = "application/json"))
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
  .logTrace(paste("jira: Fetching comments of Issue -", id), pr = FALSE)
  .logTrace(paste("jira: Running comments Query: ", pkg.globals$.jiraEnv, "/rest/api/2/issue/", id, "/comment", sep=""), pr = FALSE)
  resp <- GET(paste(pkg.globals$.jiraEnv, "/rest/api/2/issue/", id, "/comment", sep=""), add_headers("Content-Type" = "application/json"))
  df <- .jira.commentsdf(resp)
  return(df)
}

# Internal nJira issue search query
.jira.searchqry <- function(query, clean = FALSE) {
  .logTrace(paste("jira: Running Search Query: ", pkg.globals$.jiraEnv, "/rest/api/2/search?jql=", query, sep = ""), pr = FALSE)
  resp <- GET(paste(pkg.globals$.jiraEnv, "/rest/api/2/search?jql=", query, sep = ""), add_headers("Content-Type" = "application/json"))
  
  if (length(content(resp)$errorMessages[[1]]) > 0) {
    .logTrace("jira: Error in Query")
    .logTrace(content(resp)$errorMessages[[1]])
    return(NULL)
  }
  
  if (content(resp)$total == 0 || length(content(resp)$issues) == 0) {
    .logTrace("jira: No Issues found", pr = FALSE)
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
      if (pkg.globals$.jiraVal == "1") {if ("displayName" %in% names(x)) {if (x[["displayName"]] != "") {return (x[["displayName"]])}}}
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
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- as.data.frame(lapply(df, function(x) as.character(x)), stringsAsFactors = FALSE)
  if (clean == TRUE) {df <- Filter(function(x)!all(x == "NULL"), df)}
  colnames(df) <- .jira.fields.map(colnames(df), toAlias = TRUE)
  df$id <- as.character(dm$key)
  return(df)
}

# Internal nJira function takes JIRA search queries related to issues (As you pass them on JIRA) and returns the response in a dataframe.
.jira.search.issue <- function(query, startAt=0, maxresults=NULL, fields = NULL, clean = FALSE) {
  
  # Replace space in query with %20
  query <- gsub(" ", "%20", query)
  
  # which fields to fetch in search query
  if (is.null(fields)) {fields <- paste(.jira.issues.fields(default = TRUE)$id, collapse=",")}
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