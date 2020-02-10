# function for timestamping
.tStamp <- function() {
  return(format(Sys.time(), "%d-%m-%y %H:%M:%S"))
}

# function for logging
.logTrace <- function(msg = NULL, pr = TRUE, fl = NULL) {

  if(pr) {print(paste(.tStamp(), ":", msg))}
  if (pkg.globals$.logs) {
    if (is.null(fl)) {
      fldr = paste(getwd(), "Logs", sep = "/")
      if (!dir.exists(fldr)) {dir.create(fldr, recursive = TRUE)}
      fl = paste(fldr, "/nJira_log_", format(Sys.time(), "%d-%m-%y.txt"), sep = "")
    }
  
    if (!file.exists(fl)) {
      file.create(fl)
      write.table(paste(.tStamp(), ": This package provides a SQL query like interface to fetch data from JIRA (using JIRA REST API)"),fl, append = TRUE, row.names = FALSE, col.names = FALSE, sep = "")
    }
    write.table(paste(.tStamp(), ":", msg), fl, append = TRUE, row.names = FALSE, col.names = FALSE, sep = "")
  }
}

##############################################################################
# query processing logic
##############################################################################

#' Processing Meta Data
#'
#' The function returns the list of tables, fields, and their descriptions.
#'
#' @param table name of the interface tables. If not specified, all the tables of the given interface are returned.
#' @param fields is the list of field names whose details are required. If not specified, all the fields of the specified tables are returned.
#' @param gettabs is a function that returns the list of all the tables for the given interface.
#' @param getflds is a function that returns the list of all the fields for the given table.
#' @param infofile is the name of the file containing information about different tables and fields of the given interface.
#' @return The function returns the resulting data frame.

rk.metadata <- function(table = NULL, fields = NULL, gettabs, getflds, infofile = NULL) {
  tabfld <- data.frame(Table = character(0), Field = character(0), stringsAsFactors = FALSE)

  if (!is.null(table) && length(table) > 1)
    stop("Table parameter accepts a single table name")

  if (is.null(fields)) {
    # List down all the fields of the specified table(s)

    k <- 1
    if (is.null(table))
      table <- gettabs()
    for (tab in table) {
      for (fld in getflds(tab)) {
        tabfld[k,] <- c(tab, fld)
        k <- k + 1
      }
    }
  } else if (is.null(table)) {
    stop("Table parameter is required, if fields are specified")
  } else {
    # Return the details of the specified fields of the given table

    k <- 1
    tfields <- getflds(table)
    if (length(fields))
      for (fld in intersect(fields, tfields)) {
        tabfld[k,] <- c(table, fld)
        k <- k + 1
      }
  }

  if (nrow(tabfld) && !is.null(infofile)) {
    fname <- infofile
    if (file.exists(fname)) {
      mdata <- read.csv(fname)

      tabfld <- merge(tabfld, mdata, by = c("Table", "Field"), all.x = TRUE)
      tabfld[is.na(tabfld$Type),]$Type <- "Str"
      tabfld[is.na(tabfld$Format),]$Format <- ""
    } else {
      .logTrace(paste("Metadata description file doesn't exist -", fname), pr = FALSE)
    }
  }

  return(tabfld)
}

#' Process Fields Clause
#'
#' The function parses the fields clause and returns the modified string as per the specified mode.
#' The fields clause supported format is represented by the following BNF:
#' \preformatted{
#'	<field.list> := <field.expr> ( DELIMIT.COMMA <field.expr> ) *
#'	<field.expr> := ( FIELD.NAME | <aggr.func> LEFT.PAREN FIELD.NAME RIGHT.PAREN ) [ AS.ALIAS FIELD.NAME ]
#'	<aggr.func> := FUNC.MIN | FUNC.MEDIAN | FUNC.AVG | FUNC.MAX | FUNC.COUNT | FUNC.SUM
#' }
#' @param fields clause following simplified sql syntax.
#' @param mode specifies the parsing logic. The default value '@' returns the field list in perfmeter query format. The '+' value returns a field list used for grouping the dataframe with alias names. The '=' value returns a field list used for grouping the dataframe with original names. The '*' value returns the alias list used for renaming the columns. Any other value returns a field list used for selecting columns from a dataframe.
#' @return The function returns the processed fields clause.

rk.fields <- function(fields, mode = "@") {
  mcomma <- FALSE
  mfields <- ""

  fields <- unlist(strsplit(fields, ","))
  for (fld in fields) {
    if ((sfld <- sub("^\\s*([\\w\\.]+|'((?:.(?!(?<![\\\\])'))*.?)')\\s*$", "~,\\1,~", fld, ignore.case = TRUE, perl = TRUE)) == fld)
      if ((sfld <- sub("^\\s*([\\w\\.]+|'((?:.(?!(?<![\\\\])'))*.?)')\\s+AS\\s+([\\w\\.]+|'((?:.(?!(?<![\\\\])'))*.?)')\\s*$", "~,\\1,\\3", fld, ignore.case = TRUE, perl = TRUE)) == fld)
        if ((sfld <- sub("^\\s*(MIN|MEDIAN|AVG|MAX|COUNT|SUM)\\(\\s*([\\w\\.]+|'((?:.(?!(?<![\\\\])'))*.?)')\\s*\\)\\s*$", "\\1,\\2,~", fld, ignore.case = TRUE, perl = TRUE)) == fld)
          if ((sfld <- sub("^\\s*(MIN|MEDIAN|AVG|MAX|COUNT|SUM)\\(\\s*([\\w\\.]+|'((?:.(?!(?<![\\\\])'))*.?)')\\s*\\)\\s+AS\\s+([\\w\\.]+|'((?:.(?!(?<![\\\\])'))*.?)')\\s*$", "\\1,\\2,\\4", fld, ignore.case = TRUE, perl = TRUE)) == fld)
            stop(paste("Invalid field expression '", sub("^\\s*", "", sub("\\s*$", "", fld)), "' specified", sep = ""))

    toks <- unlist(strsplit(sfld, ","))
    if (mode == "@") {
      s <- paste("@", toks[2], sep = "")
      if (toks[1] != "~")
        s <- paste(toupper(toks[1]), "(", s, ")", sep = "")
      mfields <- paste(mfields, ifelse(mcomma, ",", ""), s, sep = "")
      mcomma <- TRUE
    } else if (mode == "+" || mode == "=") {
      if (toks[1] != "~") {
        toks[1] = tolower(toks[1])
        if (toks[1] == "avg") toks[1] <- "mean"
        if (toks[1] == "count") toks[1] <- "length"
        mfields <- paste(mfields, ifelse(mcomma, ",", ""), ifelse(mode == "=" || toks[3] == "~", toks[2], toks[3]), " = ", toks[1], "(", toks[2], ")", sep = "")
        mcomma <- TRUE
      }
    } else if (mode == "*") {
      id <- ifelse(toks[3] == "~", toks[2], toks[3])
      if (substr(id, 1, 1) == "'" && substr(id, nchar(id), nchar(id)) == "'")
        id <- substr(id, 2, nchar(id) - 1)
      mfields <- paste(mfields, ifelse(mcomma, ",", ""), id, sep = "")
      mcomma <- TRUE
    } else {
      id <- toks[2]
      if (substr(id, 1, 1) == "'" && substr(id, nchar(id), nchar(id)) == "'")
        id <- substr(id, 2, nchar(id) - 1)
      mfields <- paste(mfields, ifelse(mcomma, ",", ""), "'", id, "'", sep = "")
      mcomma <- TRUE
    }
  }

  return(mfields)
}

#' Process Where Clause
#'
#' The function parses the where clause and returns the modified string as per the specified mode.
#' The where clause supported format is represented by the following BNF:
#' \preformatted{
#'	<where.cond> := <where.and> [ LOGICAL.OR <where.cond> ]
#'	<where.and> := <where.not> [ LOGICAL.AND <where.and> ]
#'	<where.not> := [ LOGICAL.NOT ] <where.clause>
#'	<where.clause> := LEFT.PAREN <where.cond> RIGHT.PAREN | <where.expr>
#'	<where.expr> := ( IDENTIFIER | QUOTE.STR ) ( [ LOGICAL.NOT ] ( OPERATOR.IN <value.list> | OPERATOR.LIKE <value.const> ) | OPERATOR.IS [ LOGICAL.NOT ] VALUE.NULL | <logic.cond> )
#'	<logic.cond> := ( EQUAL.TO | NOT.EQUAL | LESS.THAN | GREATER.THAN | LESS.EQUAL | GREATER.EQUAL ) <value.const>
#'	<value.list> := LEFT.PAREN <value.const> ( DELIMIT.COMMA <value.const> ) * RIGHT.PAREN
#'	<value.const> := | QUOTE.STR | NUMBER
#' }
#' @param where clause following simplified sql syntax.
#' @param mode specifies the parsing logic. The default value '@' returns the where clause in perfmeter format. The '=' value returns the where clause in IOD format. The '~' value returns the where clause in Jira format. The '' (empty string) value returns a where clause used with a sql statement. If a dataframe name is passed, the function returns the where clause for use with a dataframe.
#' @param fields fields to be filtered.
#' @return The function returns the processed where clause.

rk.where <- function(where = NULL, mode = "@", fields = NULL) {
  if (is.null(where) || !nchar(where)) return("")

  e <- new.env()

  e$k <- 1
  e$toks <- c()
  e$tokv <- c()

  # Tokens and corresponding regular expressions

  ntok <- 0
  retok <- c()
  retok[LOGICAL.OR	<- (ntok <- ntok + 1)] <- "^OR(?!\\w)"
  retok[LOGICAL.AND	<- (ntok <- ntok + 1)] <- "^AND(?!\\w)"
  retok[LOGICAL.NOT	<- (ntok <- ntok + 1)] <- "^NOT(?!\\w)"
  retok[OPERATOR.IN	<- (ntok <- ntok + 1)] <- "^IN(?!\\w)"
  retok[OPERATOR.LIKE	<- (ntok <- ntok + 1)] <- "^LIKE(?!\\w)"
  retok[OPERATOR.IS	<- (ntok <- ntok + 1)] <- "^IS(?!\\w)"
  retok[VALUE.NULL	<- (ntok <- ntok + 1)] <- "^NULL(?!\\w)"
  retok[NUMBER		<- (ntok <- ntok + 1)] <- "^[\\d\\.]+"
  retok[IDENTIFIER	<- (ntok <- ntok + 1)] <- "^[\\w\\.]+"
  retok[QUOTE.STR		<- (ntok <- ntok + 1)] <- "^'((?:.(?!(?<![\\\\])'))*.?)'"
  retok[LEFT.PAREN	<- (ntok <- ntok + 1)] <- "^\\("
  retok[RIGHT.PAREN	<- (ntok <- ntok + 1)] <- "^\\)"
  retok[DELIMIT.COMMA	<- (ntok <- ntok + 1)] <- "^,"
  retok[GREATER.EQUAL	<- (ntok <- ntok + 1)] <- "^>="
  retok[LESS.EQUAL	<- (ntok <- ntok + 1)] <- "^<="
  retok[NOT.EQUAL		<- (ntok <- ntok + 1)] <- "^<>|^!="
  retok[LESS.THAN		<- (ntok <- ntok + 1)] <- "^<"
  retok[GREATER.THAN	<- (ntok <- ntok + 1)] <- "^>"
  retok[EQUAL.TO		<- (ntok <- ntok + 1)] <- "^="
  retok[END.OF.EXPR	<- (ntok <- ntok + 1)] <- "^"

  iod <- mode == "="
  jira <- mode == "~"
  sql <- mode == "@" || mode == "~" ||  mode == ""

  generate.tokens <- function() {
    # Remove trailing spaces

    where <- sub("\\s+$", "", where)

    while (nchar(where)) {
      # Remove leading spaces

      where <- sub("^\\s+", "", where)

      for (n in 1:ntok)
        if ((k <- regexpr(retok[n], where, ignore.case = TRUE, perl = TRUE)) > 0)
          break

      if (n == END.OF.EXPR)
        stop(paste("Invalid where sub-clause at '", where, "'", sep = ""))

      e$toks <- c(e$toks, n)
      n <- attr(k, "match.length")
      e$tokv <- c(e$tokv, substr(where, 1, n))
      where <- substr(where, n + 1, nchar(where))
    }

    e$toks <- c(e$toks, END.OF.EXPR)
    e$tokv <- c(e$tokv, "")

    return(e$tokv)
  }

  where.cond <- function() {
    s <- where.and()
    if (e$toks[e$k] == LOGICAL.OR) {
      e$k <- e$k + 1
      r <- where.cond()
      if (iod)
        return("")
      if (!nchar(s))
        return(r)
      if (!nchar(r))
        return(s)
      return(paste(s, ifelse(sql, "OR", "|"), r))
    }
    return(s)
  }

  where.and <- function() {
    s <- where.not()
    if (e$toks[e$k] == LOGICAL.AND) {
      e$k <- e$k + 1
      r <- where.and()
      if (!nchar(s))
        return(r)
      if (!nchar(r))
        return(s)
      return(paste(s, ifelse(sql, "AND", "&"), r))
    }
    return(s)
  }

  where.not <- function() {
    if (e$toks[e$k] == LOGICAL.NOT) {
      e$k <- e$k + 1
      s <- where.clause()
      return(ifelse(iod || !nchar(s), "", paste(ifelse(sql, "NOT", "!"), s)))
    }
    return(where.clause())
  }

  where.clause <- function() {
    if (e$toks[e$k] == LEFT.PAREN) {
      e$k <- e$k + 1
      s <- where.cond()
      if (e$toks[e$k] != RIGHT.PAREN)
        stop(paste("Missing right parenthesis after '", s, "' at token number ", e$k, sep = ""))
      e$k <- e$k + 1
      return(ifelse(!nchar(s), "", ifelse(iod, s, paste("(", s, ")", sep = ""))))
    }

    return(where.expr())
  }

  where.expr <- function() {
    if (e$toks[e$k] != IDENTIFIER && e$toks[e$k] != QUOTE.STR)
      stop(paste("Expecting identifier and got '", e$tokv[e$k], "' at token number ", e$k, sep = ""))

    eskip <- !is.null(fields) && !e$tokv[e$k] %in% fields

    id <- paste(ifelse(iod || jira, "", mode), ifelse(sql || iod, "", "$"), e$tokv[e$k], sep = "")
    e$k <- e$k + 1

    if (e$toks[e$k] == LOGICAL.NOT || e$toks[e$k] == OPERATOR.IN || e$toks[e$k] == OPERATOR.LIKE) {
      if (not <- e$toks[e$k] == LOGICAL.NOT)
        e$k <- e$k + 1

      if (e$toks[e$k] == OPERATOR.IN) {
        e$k <- e$k + 1

        s <- value.list(id)
        if (iod)
          return("")
        if (sql)
          return(ifelse(eskip, "", paste(id, ifelse(not, " NOT", ""), " IN ", s, sep = "")))
        s <- paste(id, " %in% c", s, sep = "")
        return(ifelse(eskip, "", ifelse(not, paste("!(", s, ")", sep = ""), s)))
      }

      if (e$toks[e$k] == OPERATOR.LIKE) {
        e$k <- e$k + 1

        s <- value.const(id)
        if (iod)
          return("")
        if (sql)
          return(ifelse(eskip, "", paste(id, ifelse(not, " NOT", ""), " ", ifelse(jira, "~", "LIKE"), " ", s, sep = "")))
        return(ifelse(eskip, "", paste(ifelse(not, "!", ""), "grep(", s, ", ", id, ", perl = TRUE)", sep = "")))
      }

      stop(paste("Invalid 'IN' or 'LIKE' clause for identifier '", id, "' at token number ", e$k, sep = ""))
    }

    if (e$toks[e$k] == OPERATOR.IS) {
      e$k <- e$k + 1

      if (not <- e$toks[e$k] == LOGICAL.NOT)
        e$k <- e$k + 1

      if (e$toks[e$k] == VALUE.NULL) {
        e$k <- e$k + 1

        if (iod)
          return("")
        if (sql)
          return(ifelse(eskip, "", paste(id, " IS", ifelse(not, " NOT", ""), " NULL", sep = "")))
        return(ifelse(eskip, "", paste(ifelse(not, "!", ""), "is.na(", id, ")", sep = "")))
      }

      stop(paste("Invalid 'IS' clause for identifier '", id, "' at token number ", e$k, sep = ""))
    }

    s <- logic.cond(id)
    return(ifelse(eskip || !nchar(s), "", paste(id, s)))
  }

  logic.cond <- function(id) {
    tok <- e$toks[e$k]
    if (tok == EQUAL.TO)
      op <- ifelse(sql || iod, "=", "==")
    else if (tok == NOT.EQUAL || tok == LESS.THAN || tok == GREATER.THAN || tok == LESS.EQUAL || tok == GREATER.EQUAL)
      op <- e$tokv[e$k]
    else
      stop(paste("Missing logical condition for identifier '", id, "' at token number ", e$k, sep = ""))
    e$k <- e$k + 1

    s <- value.const(id)
    return(ifelse(iod && tok != EQUAL.TO, "", paste(op, s)))
  }

  value.list <- function(id) {
    if (e$toks[e$k] != LEFT.PAREN)
      stop(paste("Missing left parenthesis for the 'IN' value list of identifier '", id, "' at token number ", e$k, sep = ""))
    e$k <- e$k + 1

    s <- paste("(", value.const(id), sep = "")

    while (e$toks[e$k] == DELIMIT.COMMA) {
      e$k <- e$k + 1
      s <- paste(s, ",", value.const(id), sep = "")
    }

    if (e$toks[e$k] != RIGHT.PAREN)
      stop(paste("Missing right parenthesis for the 'IN' value list of identifier '", id, "' at token number ", e$k, sep = ""))
    e$k <- e$k + 1

    return(paste(s, ")", sep = ""))
  }

  value.const <- function(id) {
    if (e$toks[e$k] == QUOTE.STR || e$toks[e$k] == NUMBER) {
      s <- e$tokv[e$k]
      s <- ifelse(iod && e$toks[e$k] == QUOTE.STR, gsub("\\s+", "+", substr(s, 2, nchar(s) - 1)), s)
      e$k <- e$k + 1
      return(s)
    }

    stop(paste("Missing value for the identifier '", id, "' at token number ", e$k, sep = ""))
  }

  generate.tokens()
  s <- where.cond()

  if (e$toks[e$k] != END.OF.EXPR)
    stop(paste("Unexpected token '", e$tokv[e$k], "' at token number ", e$k, sep = ""))

  return(ifelse(iod, gsub("\\s", "", s), s))
}

#' Process GroupBy Clause
#'
#' The function parses the groupby clause and returns the modified string as per the specified mode.
#'
#' @param groupby clause following simplified sql syntax.
#' @param mode specifies the parsing logic. The default value '@' returns the groupby clause in perfmeter format. Any other value returns the groupby fields used for aggregation.
#' @return The function returns the processed groupby clause.

rk.groupby <- function(groupby = NULL, mode = "@") {
  if (is.null(groupby) || !nchar(groupby)) return("")

  mcomma <- FALSE
  mgrpby <- ""

  grpby <- unlist(strsplit(groupby, ","))
  for (grp in grpby) {
    if (!length(grep("^\\s*([\\w\\.]+|'((?:.(?!(?<![\\\\])'))*.?)')\\s*$", grp, ignore.case = TRUE, perl = TRUE)))
      stop(paste("Invalid group expression '", sub("^\\s*", "", sub("\\s*$", "", grp)), "' specified", sep = ""))
    grp <- sub("^\\s*", "", sub("\\s*$", "", grp))

    if (mode == "@")
      mgrpby <- paste(mgrpby, ifelse(mcomma, ",", ""), "@", grp, sep = "")
    else {
      if (substr(grp, 1, 1) == "'" && substr(grp, nchar(grp), nchar(grp)) == "'")
        grp <- substr(grp, 2, nchar(grp) - 1)
      mgrpby <- paste(mgrpby, ifelse(mcomma, ",", ""), "'`", grp, "`'", sep = "")
    }
    mcomma <- TRUE
  }

  return(mgrpby)
}

#' Data Processing Query
#'
#' The function applies the given fields, where clause, and group by fields on the specified data frame.
#'
#' @param dframe data frame to be processed.
#' @param fields fields to be filtered.
#' @param where clause applied on the data.
#' @param groupby used to aggregate the fields.
#' @return The function returns the resulting data frame.

rk.query <- function(dframe, fields = NULL, where = NULL, groupby = NULL) {
  .logTrace(paste("Query Fields '", fields, "' Where '", where, "' GroupBy '", groupby, "'", sep = ""), pr = FALSE)

  if (is.null(fields))
    fields <- paste(names(dframe), collapse = ",")

  flds <- ifelse(is.null(groupby), fields, paste(groupby, fields, sep = ","))
  eval(parse(text = paste("dframe <- dframe[", rk.where(where, "dframe"), ", unique(c(", rk.fields(flds, ""), "))]", sep = "")))

  if (nchar(gby <- rk.groupby(groupby, ""))) {

    eval(parse(text = paste("dframe <- ddply(dframe, c(", gby, "), summarise, ", rk.fields(fields, "="), ")", sep = "")))

    # Rename the dataframe field names from the original names to the alias names

    .qstov <- function(str) {
      vec <- c()
      for (s in unlist(strsplit(str, ","))) {
        if (substr(s, 1, 1) == "'" && substr(s, nchar(s), nchar(s)) == "'")
          s <- substr(s, 2, nchar(s) - 1)
        vec <- c(vec, s)
      }
      return(vec)
    }

    forg <- .qstov(rk.fields(fields, ""))
    fnew <- .qstov(rk.fields(fields, "*"))

    fren <- c()
    for (fnam in names(dframe)) {
      if (!is.na(k <- match(fnam, forg)))
        fnam <- fnew[k]
      fren <- c(fren, fnam)
    }
    names(dframe) <- fren
  } else
    names(dframe) <- unlist(strsplit(rk.fields(fields, "*"), ","))

  return(dframe)
}
