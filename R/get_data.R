# Custom Functions for App


# Open connection to CAS and import main functions
connect <- function(username, password, lib) {
  
  conn <<- CAS('hostname', port=8777, caslib = 'casuser',   username = username,   password = password, protocol = "http")
  
  return(cas.builtins.serverStatus(conn))
}


# List tables from a specific CASLIB
list_tables <- function(lib) {
  tbls <- cas.table.tableInfo(conn, caslib=lib)
  names <- tbls$TableInfo[1:3]
  return(names)
}


# Generate summay statistics for a specific CAS Table
explore_tbl <- function(tbl) {
  cols <- c("Column", "Min", "Max", "N", "NMiss", "Mean",	"Sum")
  summary_tbl <- data.frame(cas.simple.summary(conn, table={name=tbl}))[1:7]
  colnames(summary_tbl) <- cols
  return(summary_tbl)
}


# Get table from CAS to R dataframe
get_table <- function(tbl) {
  tbl <- defCasTable(conn, tbl)
  tbl <- to.casDataFrame(tbl)
  return(tbl)
}


# Upload SAS dataset to CAS
upload_tbl <- function(tbl_path) {
  str1 <- gsub("\\","/", tbl_path["datapath"], fixed=TRUE)
  
  name = unlist(strsplit(as.character(tbl_path["name"]), split='.', fixed=TRUE))[1]
  
  cas.table.dropTable(conn, caslib="casuser", name=name, quite=TRUE)
  
  tbl <- cas.read.sas7bdat(conn, str1, casOut=list(name=name, caslib="casuser", replace = TRUE))
  tbl <- defCasTable(conn, table=name)
  tbl_sum <- explore_tbl(name)
  return(tbl_sum)
}

