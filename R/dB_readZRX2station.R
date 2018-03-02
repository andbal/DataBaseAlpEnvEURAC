# AUTHOR: Johannes Brenner, Institute for Alpine Environments
# DATE.VERSION: 19.08.2014 V1

# PURPOSE
# read ZRX data files (province Bozen, WISKI, batch download) with function readZRX
# general quality check | min - max variable dependent
# & hourly aggregation (mean - sum) possible
# AND
# return .csv file for each station with available variables

# required libraries: zoo
# required source:    readZRX.R

# ARGUMENTS
# files       ZRX file names (absolute paths)
# write.csv   logical, if TRUE .csv files for each station are written
# output_path path to which .csv files are writte
# do.hourly   logical, if TRUE data gets hourly aggregated
# do.quality  logical, if FALSE general quality check is performed (min - max)

# VALUES
# output .csv files containing available variables for each station
# list containing zoo objects for each station

dB_readZRX2station <- function(files, write_csv=FALSE, output_path, do.hourly=FALSE, do.quality=FALSE, chron=TRUE, 
                               multivar=FALSE, multistation=FALSE, saveRData=FALSE)
  
  {
    # source function readZRX
    #source("readZRX")
  
    # dummies for station names, data and meta data
    stnames <- c()
    out_data <- list()
    out_metadata <- list()
    
    # dummy for station data
    station_data <- list()
    
    # correct list to exclude void file
    empty_file <- list()
    for (f in files)
    {
      if (file.info(f)$size == 0)
          {
            empty_file[[length(empty_file)+1]] <- f
            #empty_file = list(empty_file, c=f)
            files <- files[files != f]
          }
    }
    
    if (write_csv) {
        write.table(as.data.frame(empty_file), file = file.path(output_path, paste0("empty_file_list",".csv")), 
                    sep = ",", dec = ".", quote=F, row.names = F, col.names = F)
    } else {
        print(as.character(empty_file), quote=T)
    }
    
    if (length(files) == 0)
    {
      print("All given files are empty. Execution interrupted.")
      stop()
      # Could give rise to error in case .zrx are split in some folders (i.e. if
      # they are separated by variable), and one folder contain only empty file.
    }
    
    # read data via loop over files
    for (i in files)
    {
      print(paste("file", i))
      if (multivar) {
        out <- dB_readZRX(i, do.hourly = do.hourly, do.quality = do.quality, chron = chron, multivar = multivar)
        for (st in unique(out$meta[,"st_id"]))
        {
          out_data[[paste0("st",st)]] <- out$data[out$meta[,1]%in%st]
          out_metadata[[paste0("st",st)]] <- out$meta[out$meta[,1]%in%st,]
          stnames <- c(stnames, paste0("st",st))
          
          # merge variables in one zoo object
          
          if (length(out_data[[paste0("st",st)]]) > 1) {
            dummy <- out_data[[paste0("st",st)]][[1]]
            
            for (t in 2:length( out_data[[paste0("st",st)]] ))
              dummy <- cbind(dummy, out_data[[paste0("st",st)]][[t]])
            
          } else {
            dummy <- out_data[[paste0("st",st)]][[1]]
          }
          # retain all parts of column name other than station name (all after first underscore)
          names(dummy) <- sub(x = names(out_data[[paste0("st",st)]]), pattern = "^.*?_", replacement = "")
          
          # write.csv
          # write .csv file containing station data
          if (write_csv)
          {
            #STinMetadata <- which(substr(i,3,nchar(i))==metadata[,"st_id"])
            if (do.hourly==T && as.integer(unique(out_metadata[[paste0("st",st)]][,"time_agg"])) <= 60){
              output_filename <- paste0("st", st, "_60")
            } else {
              output_filename <- paste0("st", st, "_", unique(out_metadata[[paste0("st",st)]][,"time_agg"]))
            }
            
            if ( all(as.integer(unique(out_metadata[[paste0("st",st)]][,"time_agg"])) <= 60) ) {
              df <- data.frame(date = format(time(dummy), "%Y-%m-%d %H:%M:%S"), coredata(dummy))
              write.table(x = df, file =file.path(output_path, paste0(output_filename,".csv")),
                          row.names=F, col.names=T, sep=",", dec = ".", quote=F)
            } else {
              write.zoo(x = dummy, file = file.path(output_path, paste0(output_filename,".csv")), 
                        row.names=F, col.names=T, sep=",", quote=F, index.name="date") 
            }
           
          }
          
          station_data[[paste0("st",st)]] <- dummy
        }
        
      } else {
        out <- dB_readZRX(i, do.hourly = do.hourly, do.quality = do.quality, chron = chron, multivar = multivar)
        out_data[[substr(i,1,nchar(i)-4)]] <- out[[1]]
        out_metadata[[substr(i,1,nchar(i)-4)]] <- out[[2]]
        stnames <- c(stnames, names(out_data[[substr(i,1,nchar(i)-4)]]))
      }
      
    }
  
    if (!multivar) {
      
      # get unique station IDs
      stations <- unique(stnames)

      # preperation for dummy with minimal time frame 
      t <- lapply(X = out_data, FUN = function(x){
        lapply(X = x, FUN = function(x){
          diff(range(time(x)))
        })
      })
      t <- lapply(t, unlist)
      min1 <- lapply(t, which.min)
      min2 <- which.min(unlist(lapply(t, which.min)))
      
      # loop over unique station vector
      for (i in stations)
      {
        # dummy for specific station and variable available for this station
        dummy <- zoo(NA, time(out_data[[min2]][[min1[[min2]]]]))
        name_spec <- c()
        
        # loop over variables
        for (dat in names(out_data))
        {
          #get meta data for variable dat
          metadata <- out_metadata[[dat]]
          
          #get data for variable dat and station i
          data <- out_data[[dat]]
          
          if ( any(names(data)==i) ){
            st_data <- data[[i]]
            
            dummy <- merge(dummy, st_data)
            name_spec <- c(name_spec, TRUE)
          } else {
            name_spec <- c(name_spec, FALSE)
          }
        }
        dummy <- dummy[,-1]
        
        # name coloums of zoo object
        names(dummy) <- names(out_data)[name_spec]
        
      # write .csv file containing station data
        if (write_csv)
        {
          #STinMetadata <- which(substr(i,3,nchar(i))==metadata[,"st_id"])
          if (do.hourly==T & as.integer(unique(metadata[,"time_agg"])) <= 60){
            output_filename <- paste(i, "60", sep="_")
          } else {
            output_filename <- paste(i, unique(metadata[,"time_agg"]), sep="_")
          }
          
        if (as.integer(unique(metadata[,"time_agg"])) <= 60) {
          df <- data.frame(date = format(time(dummy), "%Y-%m-%d %H:%M:%S"), coredata(dummy))
          write.table(x = df, file = file.path(output_path, paste0(output_filename,".csv")),
                      row.names=F, col.names=T, sep=",", dec = ".", quote=F)
        } else {
          write.zoo(x = dummy, file = file.path(output_path, paste0(output_filename,".csv")), 
                    row.names=F, col.names=T, sep=",", quote=F, index.name="date")
        }
 
        }
        
        # save data in station data list
        station_data[[i]] <- dummy
      }
    }
    
    # write meta files
    if (write_csv) 
    {
      if (length(files)==1 && multistation==FALSE) {
        filen <- paste0("meta_",stnames,".csv")
        write.table(out_metadata[[1]], file.path(output_path,filen), row.names = F, quote = F, sep = ",", dec = ".")
      } else {
        for (m in names(out_metadata))
        {
          filen <- paste0("meta_",out_metadata[[m]][1],"_",m,".csv")
          write.table(out_metadata[[m]], file.path(output_path,filen), row.names = F, quote = F, sep = ",", dec = ".")
        }
      }
    
    }
    else {
      # return function output
      return(station_data)
    }
    
    if (saveRData)
    {
      save(list = "station_data", file = "data.RData")
    }

  }
