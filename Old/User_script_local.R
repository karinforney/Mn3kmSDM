# Set user object based on computer signature
#   User path used for specifying which files paths to use

user <- switch(
  Sys.info()["nodename"], 
  "SWC-KFORNEY-L" = "KAF", 
  "SWC-SWOODMAN-L" = "SMW"
)