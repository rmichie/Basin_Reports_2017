
get.landuse <- function(nlcd_huc8_df, geo_unit, geo_unit_col, group_col) {
  # reclassess and summarizes landuse
  
  require(dplyr)
  require(tidyr)
  
  #geo_unit_col_en <- quo(geo_unit_col)
  #group_col_en <- quo(group_col)
  
  #Reclass the NLCD
  landuse.df <- nlcd_huc8_df %>% 
    filter_(geo_unit_col == geo_unit) %>% 
    group_by_(group_col) %>% 
    dplyr::summarise(WsAreaSqKm = round(sum(VALUE_11,VALUE_12,
                                            VALUE_21,VALUE_22,VALUE_23,VALUE_24,
                                            VALUE_31,VALUE_41,VALUE_42,VALUE_43,
                                            VALUE_52,VALUE_71,VALUE_81,VALUE_82,
                                            VALUE_90,VALUE_95) * 0.001),
                     PerUrbanWs = round(sum(VALUE_21,
                                            VALUE_22,
                                            VALUE_23,
                                            VALUE_24) * 0.001 / WsAreaSqKm * 100, 1),
                     PerForestWs = round(sum(VALUE_41,
                                             VALUE_42,
                                             VALUE_43,
                                             VALUE_90) * 0.001 / WsAreaSqKm * 100, 1),
                     PerAgWs = round(sum(VALUE_81,
                                         VALUE_82) * 0.001 / WsAreaSqKm * 100, 1),
                     PerRangeWs = round(sum(VALUE_52,
                                            VALUE_71) * 0.001 / WsAreaSqKm * 100, 1),
                     PerOtherWs = round(sum(VALUE_11,
                                            VALUE_12,
                                            VALUE_31,
                                            VALUE_95) * 0.001 / WsAreaSqKm * 100, 1))
  
  return(landuse.df)
}

map.basemap <-function() {
  # basemap for word/pdf documents
  # THIS DOES NOT WORK RIGHT NOW
  # Just a parking lot for old code
  
  require(OpenStreetMap)
  require(ggplot2)

  bb <- as.vector(bbox(owrd_basins_shp_dd))

  # Add 0.5 decial degrees to bounding box
  up.lt <- c(bb[4]+0.5, bb[1]-0.5)
  low.rt <- c(bb[2]-0.5, bb[3]+0.5)

  owrd_basins_df <- fortify(owrd_basins_shp_dd)
  owrd_basins_df$basin <- owrd_basin

  basemap_doc <- autoplot(openproj(openmap(upperLeft=up.lt, lowerRight=low.rt, zoom=10, type ="osm", mergeTiles = TRUE),projection = "+proj=longlat")) +
    geom_path(data=owrd_basins_df, aes(x=long,y=lat, group=basin),size=1) +
    xlab("Longitude") +
    ylab("Latitude")

  ggsave(file=paste0("basemap.png"),
         plot=basemap_doc,
         height=4,
         width=4,
         units="in")
  
}

map.landuse <- function() {
  
  # THIS DOES NOT WORK RIGHT NOW
  # Just a parking lot for old code
  
  require(leaflet)
  require(raster)
  require(rgdal)
  
  GetURL <- function(service, host = "basemap.nationalmap.gov") {
    sprintf("https://%s/arcgis/services/%s/MapServer/WmsServer", host, service)
  }
  
  
  # The input file geodatabase
  gis_dir <- "GIS"
  
  owrd_basins_shp <- readOGR(dsn=gis_dir,layer="owrd_admin_basins", integer64="warn.loss", verbose=FALSE) 
  
  owrd_basins_shp <- owrd_basins_shp[owrd_basins_shp$BASIN_NAME == owrd_basin,]
  owrd_basins_shp <-spTransform(owrd_basins_shp, CRS("+proj=longlat +datum=NAD83"))
  
  usgs.att <- paste0("<a href='https://www.usgs.gov/'>",
                     "U.S. Geological Survey</a> | ",
                     "<a href='https://www.usgs.gov/laws/policies_notices.html'>",
                     "Policies</a>")
  
  landusemap <- leaflet(width = "100%") %>%
    addPolygons(data=owrd_basins_shp,
                label=~BASIN_NAME,
                labelOptions= labelOptions(direction = 'auto'),
                stroke = TRUE, color = "black",  fillOpacity = 0,
                group="Basin") %>%
    addTiles() %>%
    addWMSTiles('https://raster.nationalmap.gov/arcgis/services/LandCover/USGS_EROS_LandCover_NLCD/MapServer/WMSServer?',
                group = "Land Use (NLCD 2011)",
                attribution = usgs.att,
                layers = '33',
                options = WMSTileOptions(format = 'image/png',
                                         version = '1.3.0',
                                         transparent = TRUE)) %>%
    addWMSTiles(GetURL("USGSHydroCached"), 
                group = "Hydrography", 
                options = WMSTileOptions(format = "image/png", 
                                         transparent = TRUE),
                layers = "0") %>%
    addEasyButton(easyButton(
      icon="fa-globe", title="Zoom to Basin",
      onClick=JS("function(btn, map){map.setView([lat=45.62795,lng=-123.4094],zoom=8); }")))  %>%
    hideGroup("Hydropgraphy")  %>%
    addLayersControl(overlayGroups = c("Hydrography"))
  landusemap
  
  
}

get.OWRI.huc8.names <-function(owri.mdb, complete.years) {
  # Retreives the huc8 names from the OWRI database for various completion years.
  
  # owri.mdb <- the path and name of the OWRI database in mdb format
  # complete.years <- vector of years to query cooresponding to the project completion year
  
  # Ryan Michie
  
  # --- Load required packages  -----------------
  library(RODBC)

  options(stringsAsFactors = FALSE)
  
  # --- Read OWRI data from access database -----------------
  
  # I'm keeping the dataframes the same name that OWEB uses
  channel <-odbcConnectAccess2007(owri.mdb)
  ProjectInfo <- sqlFetch(channel, "ProjectInfo")
  close(channel)
  
  ProjectInfo$SubbasinActual <- as.character(ProjectInfo$SubbasinActual)
  
  # get all the unique huc8 names
  owri.huc8.names <- unique(ProjectInfo[ProjectInfo$CompleteYear %in% complete.years,]$SubbasinActual)
  
  return(owri.huc8.names)
  
}

get.OWRI.projects <- function(owri.mdb, huc8.names, complete.years, cost.plot) {
  # Retreives and formats OWRI project data and returns a data frame with 
  # Suubasin, Project Name, Activity Type, Project Description, All participants, all reported results.
  # Also saves a bar chart of the total costs (TotalCash + TotalInKind) faceted by Subbasin where 
  # Y-axis= Total cost and X-axis = Activity Type 
  
  # owri.mdb <- the path and name of the OWRI database in mdb format
  # huc8.names <- vector of huc8 names to query projects
  # complete.years <- vector of years to query cooresponding to the project completion year
  # cost.plot <- path and file name of where to save the cost bar chart
  
  # Ryan Michie
  
  # --- Load required packages  -----------------
  library(RODBC)
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(scales)
  
  options(stringsAsFactors = FALSE)
  
  # --- Read OWRI data from access database -----------------
  
  # I'm keeping the dataframes the same name that OWEB uses
  channel <-odbcConnectAccess2007(owri.mdb)
  ActivityCost <- sqlFetch(channel, "ActivityCost")
  Codes <- sqlFetch(channel,"ActivityTypeLUXActivityLUXTreatmentLU")
  Participant <- sqlFetch(channel, "Participant")
  ProjectInfo <- sqlFetch(channel, "ProjectInfo")
  Result <- sqlFetch(channel, "Result")
  ResultLU <- sqlFetch(channel, "ResultLU")
  close(channel)
  
  # --- Get project info -----------------
  
  owri.df <- ProjectInfo %>% 
    filter(CompleteYear %in% complete.years & SubbasinActual %in% huc8.names) %>%
    dplyr::select(PROJNUM, ProjName, drvdProjDesc, SubbasinActual)
  
  # build results table
  result.df <- merge(x=Result[,c("PROJNUM","ActivityTypeLUID","Quantity","ResultLUID")], y=ResultLU[,c("ResultLUID", "Result")], by="ResultLUID", all.x=TRUE)
  
  # limit results table to projects in basin area
  result.df <- result.df[result.df$PROJNUM %in% unique(owri.df$PROJNUM),]
  
  # Add results to each project. There are many results for a single project.
  owri.df <- merge(x=owri.df, y=result.df, by='PROJNUM', all.y=TRUE)
  
  # This keeps the numerics from having endless precision when converting to character.
  owri.df$Quantity <- round(owri.df$Quantity,2)
  
  owri.df2 <- owri.df %>%
    unite(col="result2",c("Quantity","Result"), sep = " ", remove=FALSE) %>% 
    dplyr::select(Result=result2, PROJNUM, ProjName, SubbasinActual, ActivityTypeLUID, drvdProjDesc) %>%
    group_by(PROJNUM, ProjName, SubbasinActual, drvdProjDesc, ActivityTypeLUID) %>%
    summarise(Result = paste(Result, collapse = ", "))
  
  # fix result text
  owri.df2$Result <- gsub(owri.df2$Result, pattern="Total ", replacement="")
  owri.df2$Result <- gsub(owri.df2$Result, pattern="number of ", replacement="")
  owri.df2$Result <- tolower(owri.df2$Result)
  
  # Add in Activity Type
  owri.df2 <- merge(owri.df2, y=unique(Codes[,c("ActivityTypeLUID", "ActivityType")]), by="ActivityTypeLUID", all.x=TRUE)
  
  # --- Bring in participants  -----------------
  
  # Fix participant records
  Participant$Participant[Participant$Participant == "OWEB "] <- "OWEB"
  Participant$Participant[Participant$Participant == "USFWS "] <- "USFWS"
  Participant$Participant[Participant$Participant == "Metro "] <- "Metro"
  Participant$Participant[Participant$Participant == "Private Landowner"] <- "Private Landowners"
  Participant$Participant[Participant$Participant == "Private Landowner (neighbor/contributor)"] <- "Private Landowners"
  Participant$Participant[Participant$Participant == "Private Landowners (multiple)"] <- "Private Landowners"
  Participant$Participant[Participant$Participant == "volunteers"] <- "Volunteers"
  Participant$Participant[Participant$Participant == "volunteers: Calapooia Watershed volunteers"] <- "Volunteers"
  Participant$Participant[Participant$Participant == "volunteers: community"] <- "Volunteers"
  Participant$Participant[Participant$Participant == "volunteers: students and community"] <- "Volunteers"
  Participant$Participant[Participant$Participant == "Private Citizen"] <- "Volunteers"
  Participant$Participant[Participant$Participant == "Northwest Youth Corps "] <- "Northwest Youth Corps"
  Participant$Participant[Participant$Participant == "Oregon State University Environmental Conservation  (Geo 300) Class"] <- "Oregon State University"
  Participant$Participant[Participant$Participant == "Oregon State University Geo 300 class"] <- "Oregon State University"
  Participant$Participant[Participant$Participant == "Springfield Public Schools "] <- "Springfield Public Schools"
  Participant$Participant[Participant$Participant == "volunteers: Santiam Wilderness Academy"] <- "Santiam Wilderness Academy"
  
  # limit to just projects in basin area
  partic.df <- Participant[Participant$PROJNUM %in% unique(owri.df$PROJNUM),]
  
  #owri.df3 <- merge(x=owri.dfxx, y=partic.df[,c("PROJNUM", "Participant")], by='PROJNUM', all.y=TRUE)
  owri.df3 <- merge(x=owri.df2, y=partic.df[,c("PROJNUM", "Participant")], by='PROJNUM', all.y=TRUE)
  
  owri.df4 <- owri.df3 %>%
    dplyr::select(SubbasinActual, PROJNUM, ProjName, ActivityType, drvdProjDesc, Participant, Result) %>%
    group_by(SubbasinActual, PROJNUM, ProjName, drvdProjDesc, ActivityType, Result) %>%
    summarise(Participants = paste(sort(unique(Participant)), collapse = ", ")) %>%
    as.data.frame() %>%
    dplyr::select(SubbasinActual, ProjName, ActivityType, drvdProjDesc, Participants, Result)
  
  owri.df4p <- owri.df3 %>%
    dplyr::select(PROJNUM, ProjName, SubbasinActual, ActivityTypeLUID, ActivityType, drvdProjDesc, Participant, Result) %>%
    group_by(PROJNUM, ProjName, SubbasinActual, drvdProjDesc, ActivityTypeLUID, ActivityType, Result) %>%
    summarise(Participants = paste(sort(unique(Participant)), collapse = ", ")) %>%
    as.data.frame() %>%
    dplyr::select(PROJNUM, ProjName, SubbasinActual, ActivityTypeLUID, ActivityType, drvdProjDesc, Participants, Result)
  
  # --- Add cost info for plot ----
  owri.df5p <- merge(x=owri.df4p, y=ActivityCost[,c("PROJNUM","ActivityTypeLUID","Cash","InKind")], by=c("PROJNUM","ActivityTypeLUID"), all.x=TRUE)
  
  # combine participants, ActivityType, Results, and Cash/InKind
  owri.df6p <- owri.df5p %>%
    dplyr::select(PROJNUM, ProjName, SubbasinActual, ActivityType, drvdProjDesc, Cash, InKind, Participants, Result) %>%
    group_by(PROJNUM, ProjName, SubbasinActual, drvdProjDesc, Participants) %>%
    summarise(ActivityTypes = paste(unique(ActivityType), collapse = ", "), 
              TotalCash = sum(Cash), 
              TotalInKind = sum(InKind),
              Total = sum(TotalCash, TotalInKind),
              Results = paste(unique(Result), collapse = ", ")) %>%
    as.data.frame() %>%
    dplyr::select(ProjName, SubbasinActual, ActivityTypes, drvdProjDesc, Participants, Total, TotalCash, TotalInKind, Results)
  
  # aggregate cost by activity
  owri.df7p <- owri.df5p %>%
    dplyr::select(SubbasinActual, ActivityType, Cash, InKind) %>%
    group_by(SubbasinActual, ActivityType) %>%
    summarise(TotalCash = sum(Cash), 
              TotalInKind = sum(InKind),
              Total = sum(TotalCash, TotalInKind))
  
  # put a return char into labels
  owri.df7p$ActivityType <- factor(owri.df7p$ActivityType)
  levels(owri.df7p$ActivityType) <- gsub(" ", "\n", levels(owri.df7p$ActivityType))
  
  # bar plot
  p1 <- ggplot(data=owri.df7p, aes(x=ActivityType, y=Total)) +
    geom_bar(stat="identity", width = .95) +
    scale_y_continuous(labels = scales::dollar) +
    geom_text(aes(label=dollar(Total)), position=position_dodge(width=0.9), vjust=-0.25, size = 2) +
    xlab("Project Type") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.text.x=element_text(size=6)) +
    facet_wrap(~SubbasinActual, ncol=3 )
  
  # Save the plot
  ggsave(file=cost.plot,
         plot=p1,
         height=6,
         width=7.75,
         units="in")
  
  return(owri.df4)
  
}

get.wqst.summary <- function(huc8.list, df.dir) {
  
  huc8.list <- huc8.df$HUC_8
  df.dir <- wqst.dir
  
  wqst.list <- list()
  
  for (huc8 in huc8.list) {
    
    file.path <- list.files(path=df.dir, pattern = huc8 , full.names = TRUE, recursive = FALSE)
    
    load(file.path)
    
    colnames(df) <- c("Pollutant",
                      "Total Stations in Subbasin",
                      "Total Stations For Status Assessment", 
                      "Total Stations For Trend Assessment", 
                      "Stations Achieving Water Quality Criteria", 
                      "Stations Degrading Trend", 
                      "Stations Steady Trend", 
                      "Stations Improving Trend", 
                      "Stations No Trend")
    
    wqst.list[[huc8]] <- df
  }
  
  return(wqst.list)
  
}

