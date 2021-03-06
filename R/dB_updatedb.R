
# library("DataBaseAlpEnvEURAC")
# library(zoo)
# library(chron)


dB_updatedb <- function(stations = c("B0001","B0002","B0003","P0001","P0002","P0003","I0001","I0003",
                                     "M0001","M0002","M0003","M0004","M0005","M0006","M0007",
                                     "S0002", "S0004", "S0005", "XS0001", "XS0006", 
                                     "SF0001", "SF0002", "SF0003", "SF0004", "SF0005"), 
                        variables = "TOTAL",
                        path2data = "/media/alpenv/Projekte/HiResAlp/06_Workspace/BrJ/02_data/Station_data_Mazia", 
                        inCloud = "/home/jbre/ownCloud/data/SQL/",
                        write_csv = FALSE,
                        return_data = TRUE)
{

  for (j in variables)
  {
    # mkdir if not exists
    if (! dir.exists(inCloud)) dir.create(inCloud, FALSE, TRUE)
    
    # connect to db in data folder of project
    db = dbConnect(RSQLite::SQLite(), dbname=file.path(inCloud,paste(j,".sqlite",sep="")))
    
    out <- list()
    
    for (i in stations)
    {
      stationchr <- substr(i, 1, nchar(i)-4)
      stationchr_ <- stationchr
      
      if (stationchr == "XS") stationchr <- "S"
      
      stationnr  <- as.integer(substr(i, nchar(i)-3, nchar(i)))
      
      print(paste("updating ", j, " data of station", i, sep=" "))
      
      path2files = file.path(path2data,stationchr,i)
      header.file = file.path(path2data,stationchr,paste("header_",i,".txt",sep=""))
      
      if (j == "TOTAL") {
        data <- dB_readStationData(path2files, header.file, station = i)
      }
      
      if (j == "SWC") {
        data <- dB_getSWC(path2data, station = i, calibrate = F, 
                          minVALUE = 0, maxVALUE = 1, aggregation = "n")
        
        if(any(names(data)=="core5")) names(data)[which(names(data)=="core5")] <- "SWC_A_05"
        if(any(names(data)=="core20")) names(data)[which(names(data)=="core20")] <- "SWC_A_20"
      }
      
      if (j == "TSoil") {
        data <- dB_getSoilTemp(path2data, station = i,
                               minVALUE = -50, maxVALUE = 50, aggregation = "n")
        
        if(any(names(data)=="core5")) names(data)[which(names(data)=="core5")] <- "ST_A_05"
        if(any(names(data)=="core20")) names(data)[which(names(data)=="core20")] <- "ST_A_20"
      }
      
      if (j == "METEO") {
        data <- dB_getMETEO(path2data, station = i)
      }
      
      # remove data with NA date
      data <- data[!is.na(index(data))]
      
      # remove duplicate datetimes
      # data <- data[-anyDuplicated(time(data))]
      
      if(!is.regular(data, strict = TRUE))
      {
        # make regular
        g <- zoo(x = NA, seq(head(index(data),1),tail(index(data),1),by=times("00:15:00")))
        data <- merge(g,data)[,-1]
      }
      
      # insert also year - month - day - hour - min
      date  <- as.character(as.Date(index(data)))
      
      time  <- substr(index(data),13,20)
      
      datetime <- paste(date, time, sep=" ")
      
      df <- data.frame(TIMESTAMP=datetime, coredata(data))
      
      # update litesql
      dbWriteTable(conn=db, name=i,
                   value=df, row.names = NA, overwrite = TRUE, append = FALSE,
                   field.types = NULL)
      
      # write data to csv
      if (write_csv)
      {
        print(paste("save .csv for station", i, sep=" "))
        df <- format(df, scientific = FALSE)
        write.csv(x = df, file = file.path(inCloud, paste(j, "_", i, ".csv", sep="")), quote = F, row.names = F)
        #readr::write_csv(x = df, path = file.path(inCloud, paste(j, "_", i, ".csv", sep="")))
      }
      
      out[[i]] <- data
    }
    
    # list tables in db
    print("Tables in data base:")
    print(dbListTables(db))
    
    dbDisconnect(db)
    
    # if (j=="SWC")
    # {
    #   if (any(row.names(installed.packages())=="SMCcalibration") & !is.null(inCloud)) 
    #   {
    #     print("copy database swc.sqlite into data folder of the package SMCcalibration")
    #     require("SMCcalibration")
    #     pkg_path <- path.package("SMCcalibration") 
    #     system(paste("cp", file.path(inCloud,"SWC.sqlite"), file.path(pkg_path,"data","swc.sqlite")))
    #   }
    # }
  }
 
  if (return_data) return(out)
}
