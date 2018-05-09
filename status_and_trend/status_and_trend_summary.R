##This script will summarize the data aggregated for 2017 water quality status and trend reports. 
##A single table will be created for each report. 

##Maddee Rubenson
#Feburary 2018

library(RCurl)
library(XML)
library(dataRetrieval)
library(plyr)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(DT)
library(wq)
library(chron)
library(reshape)
library(reshape2)
library(zoo)
library(spatialEco)
library(dplyr)
library(lubridate)

#Necessary inputs to run query
input <- list(action_button = c(0))
input$parms <- c('Total Phosphorus', 'Total Suspended Solids',
                 'Total Suspended Solids', 'Bacteria', 'Temperature', 'pH', 'Dissolved Oxygen') #parameters
input$dates <- c("2000-01-01", "2017-12-01") #input date
input$db <- c('DEQ', 'Water Quality Portal')

hucs <- readOGR(dsn = '//deqhq1/TMDL/AgWQM/DataAnalysis/StatusAndTrendsRMarkdown/app/GIS', layer = 'WBD_HU8', verbose = FALSE)
tribal_lands <- readOGR(dsn = '//deqhq1/TMDL/AgWQM/DataAnalysis/StatusAndTrendsRMarkdown/app/GIS', layer = 'tl_2017_or_aiannh', verbose = FALSE)
HUClist <- read.csv('//deqhq1/TMDL/AgWQM/DataAnalysis/StatusAndTrendsRMarkdown/app/PlanHUC_LU.csv')
stations_huc <- read.csv('//deqhq1/TMDL/AgWQM/DataAnalysis/StatusAndTrendsRMarkdown/app/station_wbd_12132016.csv')
ph_crit <- read.csv('//deqhq1/TMDL/AgWQM/DataAnalysis/StatusAndTrendsRMarkdown/app/PlanOWRDBasinpH_LU.csv')
ph_crit <- merge(ph_crit, HUClist, by.x = 'plan_name', by.y = 'PlanName', all.x = TRUE)
lu_parms <- read.csv('//deqhq1/TMDL/AgWQM/DataAnalysis/StatusAndTrendsRMarkdown/app/WQP_Table3040_Names.csv', stringsAsFactors = FALSE)
Ben_use_LU <- read.csv("E:/GitHub/StatusAndTrendsRMarkdown/Lookups/stations.csv", na.strings = c("", "NA"))

#source the necessary functions
source('E:/GitHub/StatusAndTrendsRMarkdown/functions/Rmarkdown_query.R')
source('E:/GitHub/StatusAndTrendsRMarkdown/functions/01_DataQuery.R')
source('E:/GitHub/StatusAndTrendsRMarkdown/functions/funHelpers.R')
source('E:/GitHub/StatusAndTrendsRMarkdown/functions/funClean.R')
source('E:/GitHub/StatusAndTrendsRMarkdown/functions/funSeaKen.R')
# source('NPS_tableFUN.R')

setwd("//deqhq1/WQNPS/NPS_Annual_Reports/2017/R_NPS_WQStatusAndTrend/")

input$select <- as.character(unique(hucs$HUC_8))

##############################################
##############################################
##############################################
#This will loop through all HUC8s and output the entire dataframe into an Rdata file
## CAUTION: THIS LOOP WILL TAKE A LONG TIME ##
## ONLY NEED TO RUN ONCE ##
for(i in 1 :length(input$select)) {
  
  print(input$select[i])

  df.all<-query(select =  input$select[i],
                parms = input$parms,
                dates = input$dates,
                db = input$db)
  
  # input$select <- c("17050106")
  # print(input$select)
  # 
  # df.all<-query(select =  input$select,
  #               parms = input$parms,
  #               dates = input$dates,
  #               db = input$db)
  
  save(df.all, file = paste0("R_Outputs/HUC8_Dataframes/", input$select, "_df.all_NPS_", Sys.Date(), ".Rdata"))
  
  rm(df.all)
}
##############################################
##############################################
##############################################


files <- list.files("R_Outputs/HUC8_Dataframes/", pattern = "NPS") #only dataframes for NPS annual report

f <- 60

#Run necessary stats to create summary table
for (f in 1:length(files)) {
  tryCatch({
  load(paste0("R_Outputs/HUC8_Dataframes/", files[f]))
  
  #load("R_Outputs/HUC8_Dataframes/17050106_df.all_NPS_2018-03-06")
  
  inputHUC <- paste0("R_Outputs/HUC8_Dataframes/", files[f])
  inputHUC <- strsplit(x = inputHUC, split = c("_"))[[1]][3]
  inputHUC <- strsplit(x = inputHUC, split = c("/"))[[1]][2]
  
  print(inputHUC)
  
  df.all[df.all$Analyte == "Phosphorus", 'Analyte'] <- 'Total Phosphorus'
  df.all[df.all$Analyte == "Phosphate, Total as P", 'Analyte'] <- 'Total Phosphorus'
  df.all[df.all$Analyte == "Dissolved oxygen (DO)", 'Analyte'] <- 'Dissolved Oxygen'
  df.all[df.all$Analyte == "Total suspended solids", 'Analyte'] <- 'Total Suspended Solids'
  
  df.all <- df.all[!is.na(df.all$Station_ID), ]
  
  ##take out data from tribal land
  df.all <- df.all %>% filter(!grepl('[Tt]ribe', Client)) %>% filter(!grepl('CTG', Station_ID))
  
  df.all <- df.all %>% filter(!grepl('OREGONDEQ', Station_ID)) #duplicates from WQ portal
  
  df.all$Result <- clean(df.all$Result)
  df.all$Result <- as.numeric(df.all$Result)
  df.all <- MRLhandling(df.all)
  if ("Fecal Coliform" %in% df.all$Analyte) {
    df.all <- update_fc2ec(df.all)
  }
  
  
  if (any('Temperature' %in% df.all$Analyte)) {
    
    tempStns <- temp_sufficiency_analysis(df.all)
    
    qc.1.pass <- filter(attributes(tempStns)$day_test, result == "pass")
    
    df.all.sdadm <- df.all %>% mutate(date = date(Sampled)) %>% filter(Analyte == "Temperature") %>% merge(qc.1.pass[,c("Station_ID", "date", "result")], by=c("Station_ID", "date"), all=FALSE)
    
    sdadm <- Calculate.sdadm(df.all.sdadm, "Result", "Station_ID", "Sampled",
                             '%Y-%m-%d %H:%M:%S')
  } else {
    sdadm <- NULL
  }
  
  df.all <- remove_QAfail(df.all)
  
  #df.all$Sampled <- as.POSIXct(strptime(df.all$Sampled, format = '%Y-%m-%d')) 
  
  if (any(c('pH', 'E. Coli', "Enterococcus", "Dissolved Oxygen", 'Total Suspended Solids', 'Total Phosphorus') %in% df.all$Analyte)) {
    SeaKen <- run_seaKen(df.all)
  } else {
    SeaKen <- data.frame()
  }
  
  
  status <- Stations_Status(df.all) 
  
  trend <- Stations_Trend(df.all)
  
  stns <- All_stns_fit_Criteria(trend = trend, 
                                status = status,
                                df.all = df.all)
  
  if(any(status == "No stations meet Status criteria")) {
    status <- data.frame()
  }
  if(any(trend == "No Stations Meet Trend Criteria")) {
    trend <- data.frame()
  } else {
    trend <- trend
    if (any(c('pH', 'E. Coli', "Enterococcus", 'Dissolved Oxygen', 'Total Suspended Solids', 'Total Phosphorus') %in% trend$Analyte)) {
      SeaKen <- run_seaKen(df.all)
    }
    SeaKen <- SeaKen[SeaKen$Station_ID %in% trend$Station_ID & SeaKen$analyte %in% unique(trend$Analyte), ]
  }
  
  params <- if((nrow(trend) < 1) & nrow(status) < 1) {
    NULL  
  } else if ((nrow(trend) < 1) & nrow(status) > 0) {
    unique(c(status$Analyte)) 
  } else {
    unique(c(status$Analyte, trend$Analyte))
  }
  
  
  ###Evaluate data against water quality standard
  if('E. Coli' %in% params) {
    if('E. Coli' %in% unique(status$Analyte)) {
    Ecoli_eval <- list()
    E_stns <- unique(status[status$Analyte == "E. Coli", 'Station_ID'])
    #E_stns <- unique(df.all[df.all$Analyte == "E. Coli", "Station_ID"])
    for (s in 1:length(E_stns)) {
      tmp_df <- df.all[df.all$Station_ID == E_stns[s] & df.all$Analyte == "E. Coli" , ]
      # tmp_df$day <- substr(tmp_df$Sampled, 1, 10)
      # tmp_df$code <- paste(tmp_df$Station_ID, tmp_df$Analyte, tmp_df$day)
      # sub <- with(tmp_df, resolveMRLs(code, Detect, Result))
      # tmp_df_MRL <- tmp_df[sub,]
      # tmp_df <- remove.dups(tmp_df_MRL, max)
      
      if(nrow(tmp_df) > 2) {
        evaluate <- EvaluateEColiWQS(tmp_df)
        Ecoli_eval[[s]] <- evaluate
        rm(tmp_df)
      } else {
        Ecoli_eval <- NULL
      }
    }
    if(!is.null(Ecoli_eval)) {
      Ecoli_eval<-rbind.fill(Ecoli_eval[])
    }
    }
  }
  
  if('Total Phosphorus' %in% params) {
    TP_eval <- list()
    for (s in 1:length(stns$Station_ID)) {
      tmp_df <- df.all[df.all$Station_ID == stns$Station_ID[s] & df.all$Analyte == "Total Phosphorus" , ]
      # tmp_df$day <- substr(tmp_df$Sampled, 1, 10)
      # tmp_df$code <- paste(tmp_df$Station_ID, tmp_df$Analyte, tmp_df$day)
      # sub <- with(tmp_df, resolveMRLs(code, Detect, Result))
      # tmp_df_MRL <- tmp_df[sub,]
      # tmp_df <- remove.dups(tmp_df_MRL, max)

      if(nrow(tmp_df) > 2) {
        evaluate <- EvaluateTPWQS(tmp_df,
                                  selectWQSTP = 0)
        TP_eval[[s]] <- evaluate
        rm(tmp_df)
      } else {
        TP_eval <- NULL
      }
    }
    if(!is.null(TP_eval)) {
      TP_eval<-rbind.fill(TP_eval[])
    }
  }

  if('Total Suspended Solids' %in% params) {
    TSS_eval <- list()
    for (s in 1:length(stns$Station_ID)) {
      tmp_df <- df.all[df.all$Station_ID == stns$Station_ID[s] & df.all$Analyte == "Total Suspended Solids" , ]
      # tmp_df$day <- substr(tmp_df$Sampled, 1, 10)
      # tmp_df$code <- paste(tmp_df$Station_ID, tmp_df$Analyte, tmp_df$day)
      # sub <- with(tmp_df, resolveMRLs(code, Detect, Result))
      # tmp_df_MRL <- tmp_df[sub,]
      #tmp_df <- remove.dups(tmp_df_MRL, max)

      if(nrow(tmp_df) > 2) {
        evaluate <- EvaluateTSSWQS(tmp_df,
                                   selectWQSTSS = 0)
        TSS_eval[[s]] <- evaluate
        rm(tmp_df)
      } else {
        TSS_eval <- NULL
      }
    }
    if(!is.null(TSS_eval)) {
      TSS_eval<-rbind.fill(TSS_eval[])
    }
  }
  
  if(any(stns != "No Stations Meet Criteria for Status or Trends")) {
    # for(i in length(unique(df.all$Station_ID))) {
    #   n_year <- df.all[df.all$Station_ID == df.all$Station_ID[i] & ]
    # }
    df <- df.all %>% dplyr::group_by(Analyte) %>%
      dplyr::summarise('TotalStnsinHUC' = length(unique(Station_ID)))
    df$TotalStnsStatus <- NA
    df$TotalStnsTrend <- NA
    df$NumAchievWQS <- NA
    df$NumDegradingTrend <- NA
    df$NumImprovingTrend <- NA
    df$NumSteadyTrend <- NA
    df$NumNoTrend <- NA
    
    if(nrow(status) > 1) {
      for(i in 1:length(status$Analyte)) {
        df[df$Analyte == status$Analyte[i], ]$TotalStnsStatus <- length(unique(status[status$Analyte == status$Analyte[i], "Station_ID"]))
      } 
    }
    
    if(nrow(trend) > 1) {
      for(i in 1:length(trend$Analyte)) {
        df[df$Analyte == trend$Analyte[i], ]$TotalStnsTrend <- length(unique(trend[trend$Analyte == trend$Analyte[i], "Station_ID"]))
        
        if("Dissolved Oxygen" %in% unique(SeaKen$analyte)) {
          df[df$Analyte == "Dissolved Oxygen", ]$NumDegradingTrend <- length(unique(SeaKen$Analyte == "Dissolved Oxygen" & SeaKen$pvalue < 0.2 & SeaKen$slope < 0))
          df[df$Analyte == "Dissolved Oxygen", ]$NumImprovingTrend <- length(unique(SeaKen$Analyte == "Dissolved Oxygen" & SeaKen$pvalue < 0.2 & SeaKen$slope > 0))
          df[df$Analyte == "Dissolved Oxygen", ]$NumSteadyTrend <- length(unique(SeaKen$Analyte == "Dissolved Oxygen" & SeaKen$pvalue < 0.2 & SeaKen$slope == 0))
          df[df$Analyte == "Dissolved Oxygen", ]$NumNoTrend <- length(unique(SeaKen$Analyte == "Dissolved Oxygen" & SeaKen$pvalue > 0.2))
          
        }
        
        if("pH" %in% unique(SeaKen$analyte)) {
          df[df$Analyte == "pH", ]$NumDegradingTrend <- nrow(SeaKen[SeaKen$analyte == "pH" & SeaKen$pvalue < 0.2 & SeaKen$slope > 0, ])
          df[df$Analyte == "pH", ]$NumImprovingTrend <- nrow(SeaKen[SeaKen$analyte == "pH" & SeaKen$pvalue < 0.2 & SeaKen$slope < 0, ])
          df[df$Analyte == "pH", ]$NumSteadyTrend <- nrow(SeaKen[SeaKen$analyte == "pH" & SeaKen$pvalue < 0.2 & SeaKen$slope == 0, ])
          df[df$Analyte == "pH", ]$NumNoTrend <- nrow(SeaKen[SeaKen$analyte == "pH" & SeaKen$pvalue > 0.2, ])
          
        }
        
        if("Total Phosphorus" %in% unique(SeaKen$analyte)) {
          df[df$Analyte == "Total Phosphorus", ]$NumDegradingTrend <- nrow(SeaKen[SeaKen$analyte == 'Total Phosphorus' & SeaKen$pvalue < 0.2 & SeaKen$slope > 0, ])
          df[df$Analyte == "Total Phosphorus", ]$NumImprovingTrend <- nrow(SeaKen[SeaKen$analyte == "Total Phosphorus" & SeaKen$pvalue < 0.2 & SeaKen$slope < 0, ])
          df[df$Analyte == "Total Phosphorus", ]$NumSteadyTrend <- nrow(SeaKen[SeaKen$analyte == "Total Phosphorus" & SeaKen$pvalue < 0.2 & SeaKen$slope == 0,])
          df[df$Analyte == "Total Phosphorus", ]$NumNoTrend <- nrow(SeaKen[SeaKen$analyte == "Total Phosphorus" & SeaKen$pvalue > 0.2, ])
          
        }
        
        if("Total Suspended Solids" %in% unique(SeaKen$analyte)) {
          df[df$Analyte == "Total Suspended Solids", ]$NumDegradingTrend <- nrow(SeaKen[SeaKen$analyte == "Total Suspended Solids" & SeaKen$pvalue < 0.2 & SeaKen$slope > 0, ])
          df[df$Analyte == "Total Suspended Solids", ]$NumImprovingTrend <- nrow(SeaKen[SeaKen$analyte == "Total Suspended Solids" & SeaKen$pvalue < 0.2 & SeaKen$slope < 0, ])
          df[df$Analyte == "Total Suspended Solids", ]$NumSteadyTrend <- nrow(SeaKen[SeaKen$analyte == "Total Suspended Solids" & SeaKen$pvalue < 0.2 & SeaKen$slope == 0, ])
          df[df$Analyte == "Total Suspended Solids", ]$NumNoTrend <- nrow(SeaKen[SeaKen$analyte == "Total Suspended Solids" & SeaKen$pvalue > 0.2, ])
          
        }
        
        if("E. Coli" %in% unique(SeaKen$analyte)) {
          df[df$Analyte == "E. Coli", ]$NumDegradingTrend <- nrow(SeaKen[SeaKen$analyte == "E. Coli" & SeaKen$pvalue < 0.2 & SeaKen$slope > 0, ])
          df[df$Analyte == "E. Coli", ]$NumImprovingTrend <- nrow(SeaKen[SeaKen$analyte == "E. Coli" & SeaKen$pvalue < 0.2 & SeaKen$slope < 0, ])
          df[df$Analyte == "E. Coli", ]$NumSteadyTrend <- nrow(SeaKen[SeaKen$analyte == "E. Coli" & SeaKen$pvalue < 0.2 & SeaKen$slope == 0, ])
          df[df$Analyte == "E. Coli", ]$NumNoTrend <- nrow(SeaKen[SeaKen$analyte == "E. Coli" & SeaKen$pvalue > 0.2, ])
        }
        
      } 
    } else {
      df$TotalStnsTrend <- 0
      df$NumDegradingTrend <- 0
      df$NumImprovingTrend <- 0
      df$NumSteadyTrend <- 0
      df$NumNoTrend <- 0
    }
    
    #Num achieving wQS
    if('E. Coli' %in% params) {
      if(length(Ecoli_eval)>0){
        E_stns <- Ecoli_eval[unique(Ecoli_eval$Station_ID %in% status[status$Analyte == "E. Coli", "Station_ID"]), ]
        e_sum <- list()
        for(i in 1:length(unique(E_stns$Station_ID))) {
          e_eval <- Ecoli_eval[unique(Ecoli_eval$Station_ID %in% status[status$Analyte == "E. Coli", "Station_ID"]), ]
          e_eval$Sampled <- as.Date(e_eval$Sampled)  
          e_eval$year <- as.numeric(format(e_eval$Sampled, format="%Y"))
          e_eval<-e_eval[e_eval$Station_ID == E_stns$Station_ID[i], ]
          max_date <- max(e_eval$year)
          status_date <- seq(max_date-3, max_date, by = 1)
          
          e_eval <- e_eval %>% filter(year %in% status_date)
        
          sum <- e_eval[e_eval$Station_ID == e_eval$Station_ID[i], "exceed"]
          sum <- ifelse(sum > 0, "Exceeds", "Meets")
          
          if(any('Exceeds' %in% sum)) {
            sum <-c('Exceeds')
          } else {
            sum <- c('Meets')
          }
          
          e_sum[[i]] <- sum
          
          rm(e_eval, sum)
          
        }
        df[df$Analyte == "E. Coli", 'NumAchievWQS'] <- ifelse(any('Meets' %in% (unlist(e_sum[]))),  sum(str_count(unlist(e_sum[]), "Meets")), 0)
      }
    }
    
    df <- as.data.frame(df)
    print(df)
    save(df, file = paste0("R_Outputs/WQSandT_tables/", inputHUC, "_SummaryTable_NPS_", Sys.Date(), ".Rdata"))
    
    #rm(df, status, trend, tmp_df, e_eval, e_sum, Ecoli_eval, SeaKen, stns, TP_eval, params, inputHUC, df.all)
    
    
  } else {
    df <- df.all %>% dplyr::group_by(Analyte) %>%
      dplyr::summarise('TotalStnsinHUC' = length(unique(Station_ID)))
    df$TotalStnsStatus <- 0
    df$TotalStnsTrend <- 
    df$NumAchievWQS <- NA
    df$NumDegradingTrend <- NA
    df$NumImprovingTrend <- NA
    df$NumSteadyTrend <- NA
    df$NumNoTrend <- NA
    
    df <- as.data.frame(df)
    
    print(inputHUC)
    print(df)
    save(df, file = paste0("R_Outputs/WQSandT_tables/", inputHUC, "_SummaryTable_NPS_", Sys.Date(), ".Rdata"))
    
    #rm(df, status, trend, tmp_df, e_eval, e_sum, Ecoli_eval, SeaKen, stns, TP_eval, params, inputHUC, df.all)
    
  }
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }


# ##Testing
# tbls <- list.files("R_Outputs/WQSandT_tables/", pattern = "NPS")
# print(length(tbls))
# tbl_list <- list()
# for(i in 1:length(files)) {
#   file <- files[i]
#   load(paste0("R_Outputs/HUC8_Dataframes/", files[i]))
#   
#   inputHUC <- paste0("R_Outputs/HUC8_Dataframes/", files[i])
#   inputHUC <- strsplit(x = inputHUC, split = c("_"))[[1]][3]
#   inputHUC <- strsplit(x = inputHUC, split = c("/"))[[1]][2]
#   
#   print(inputHUC)
#   
#   print(head(df.all))
#   
#   print(nrow(df.all))
# 
# }
# 
# dframes<-rbind.fill(tbl_list[])
# 
# ###
# length(files)
# length(tbls)

