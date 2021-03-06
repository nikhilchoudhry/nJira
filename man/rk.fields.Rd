% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rklib.r
\name{rk.fields}
\alias{rk.fields}
\title{Process Fields Clause}
\usage{
rk.fields(fields, mode = "@")
}
\arguments{
\item{fields}{clause following simplified sql syntax.}

\item{mode}{specifies the parsing logic. The default value '@' returns the field list in perfmeter query format. The '+' value returns a field list used for grouping the dataframe with alias names. The '=' value returns a field list used for grouping the dataframe with original names. The '*' value returns the alias list used for renaming the columns. Any other value returns a field list used for selecting columns from a dataframe.}
}
\value{
The function returns the processed fields clause.
}
\description{
The function parses the fields clause and returns the modified string as per the specified mode.
The fields clause supported format is represented by the following BNF:
\preformatted{
<field.list> := <field.expr> ( DELIMIT.COMMA <field.expr> ) *
<field.expr> := ( FIELD.NAME | <aggr.func> LEFT.PAREN FIELD.NAME RIGHT.PAREN ) [ AS.ALIAS FIELD.NAME ]
<aggr.func> := FUNC.MIN | FUNC.MEDIAN | FUNC.AVG | FUNC.MAX | FUNC.COUNT | FUNC.SUM
}
}
