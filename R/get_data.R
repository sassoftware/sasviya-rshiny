# Custom Database Functions for App


# Open connection to CAS and import main functions;
# Using an 'authinfo' file to hold your secure database credentials is highly 
# recommended, instead of hardcoding the credentials into your R scripts. 
# You can read more on 'authinfo' files at the links below:

# https://documentation.sas.com/?cdcId=pgmsascdc&cdcVersion=9.4_3.5&docsetId=authinfo&docsetTarget=n0xo6z7e98y63dn1fj0g9l2j7oyq.htm&locale=en 
# https://github.com/sassoftware/R-swat#authinfo-file

# Though the functionality to bypass the username/password screen in the app is 
# not part of the code in the app.R file, we included logic in this function to 
# show how to conditionally capture either the authinfo or username/password
connect <- function(hostname, port, protocol = "http", authinfo_path = NULL, username = NULL, password = NULL) {
  
  # If the user is on a Windows machine, look for the '_authinfo' file in its
  # default location
  if (grepl("Windows", Sys.getenv("OS"))) {
    
    authinfo_path_def <- paste0(
      Sys.getenv("HOMEDRIVE"), 
      Sys.getenv("HOMEPATH"), 
      "_authinfo"
    )
    
    authinfo_exists <- fs::file_exists(authinfo_path_def)
    
    if (authinfo_exists) {
      
      authinfo_path <- fs::fs_path(authinfo_path_def)
      
    }
    
  } else {
    
    # Otherwise, look for the '.authinfo' file in the UNIX $HOME directory 
    authinfo_path_def <- paste0(
      Sys.getenv("HOME"), 
      "/.authinfo"
    )
    
    authinfo_exists <- fs::file_exists(authinfo_path_def)
    
    if (authinfo_exists) {
      
      authinfo_path <- fs::fs_path(authinfo_path_def)
      
    }
    
  }
  
  
  
  # If the 'authinfo_path' is not null, use it to connect
  if (!is.null(authinfo_path)) {
    
    conn <<- swat::CAS(
      hostname = hostname, 
      port = port, 
      protocol = protocol, 
      authinfo = authinfo_path
    )
    
  } else {
    
    # Otherwise, use the username and password to connect
    if (all(is.null(c(authinfo_path, username, password)))) {
      
      stop("Must supply either a valid \'authinfo\' path or a valid \'username\' and \'password\'")
      
    }
    
    conn <<- CAS(
      hostname = hostname, 
      port = port, 
      username = username, 
      password = password, 
      protocol = protocol
    )
    
  }
  
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

