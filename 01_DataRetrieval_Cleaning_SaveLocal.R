# Retrieve and Clean Manuscript Datasets and Save as Local Copy


# This manuscript relies on a combination of publicly available (and
# downloadable) datasets and datasets that must be stored in this
# repository for access.


# Types of bit decay that can cause this script to throw an error:
# 1) May run into public datasets whose naming conventions have changed (i.e.,
# a shapefile that was called California_Counties.shp and is subsequently
# called i03_CaliforniaCounties.shp).
# 2) Alternatively, an agency might have changed its hosting service and the
# download link may have changed. In this case, finding the new link should be
# easy by checking the "accessed from" website.


# If desired, connect to Data Management System (DMS) Archive folder to
# save copies of each individual dataset (rather than one combined .Rdata environment).
# When downloading this set of spatial and tabular datasets for the first
# time, or updating them all, expect the script to take about 10-20 minutes to run,
# depending on your download speed.


# One workaround to avoid the long runtime, if updating only one dataset is desired:
# 1) In a new (empty-environment) session, load the manuscript_data.Rdata environment.
# 2) In this script, overwrite one of the dataset variables by running only the lines
# of code in which that dataset is downloaded or updated.
# 3) Run the final line of code in this script, in which the working environment is
# saved as manuscript_data.Rdata.


#_SPATIAL DATA ------------------------------------------------------------

# US States and Counties -----------------------------------------------------
us_states = gadm(country = "USA", level = 1, path = scratch_dir)
california = us_states[us_states$NAME_1 == "California",]

us_counties = gadm(country = "USA", level = 2, path = scratch_dir)
county = st_as_sf(us_counties[us_counties$NAME_1=="California" & 
                         us_counties$NAME_2=="Siskiyou",])
county = st_transform(x = county, crs = st_crs(3310))

# California Cities -------------------------------------------------------
# accessed from https://catalog.data.gov/dataset/tiger-line-shapefile-2016-state-california-current-place-state-based
cities_url = "https://www2.census.gov/geo/tiger/TIGER2016/PLACE/tl_2016_06_place.zip"

zipname = strsplit(cities_url, "/")[[1]][length(strsplit(cities_url, "/")[[1]])]
zipname = gsub("-","_",zipname)
cities_dl = GET(cities_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(zipfile = file.path(scratch_dir, zipname), exdir = scratch_dir)#, list = TRUE) # just lists files, does not unzip
#Read shapefile into R
# cities_all = st_read(file.path(scratch_dir, "CA_Places_TIGER2016.shp"))
layer_name = gsub(zipname, pattern = ".zip",replacement="")#
# layer_name = "ca_places_boundaries"
cities_all = st_read(file.path(scratch_dir, paste0(layer_name,".shp")))
cities = st_transform(x=cities_all, crs=st_crs(3310))
cities = cities[county,]

#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml", "sbn", "sbx", "shp.xml")
file.remove(file.path(scratch_dir,paste(layer_name, extension_list, sep = ".")))
# Calculate centroid points
cities_centroid = st_centroid(cities)

# Roads (TIGER) -----------------------------------------------------------
# Accessed from https://catalog.data.gov/dataset/tiger-line-shapefile-2018-county-siskiyou-county-ca-all-roads-county-based-shapefile
roads_url = "https://www2.census.gov/geo/tiger/TIGER2018/ROADS/tl_2018_06093_roads.zip"
zipname = strsplit(roads_url, "/")[[1]][length(strsplit(roads_url, "/")[[1]])]
layer_name = gsub(zipname, pattern = ".zip",replacement="")
roads_dl = GET(roads_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir))
#Read shapefiles into R

roads_all = st_read( file.path(scratch_dir, paste0(layer_name,".shp")))
roads_all = st_transform(roads_all, st_crs(3310))

#remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "shp.ea.iso.xml", "shp.iso.xml")
file.remove(file.path(scratch_dir,paste(layer_name, extension_list, sep = ".")))

# DWR Basin Boundaries ----------------------------------------------------
#Accessed at https://data.cnra.ca.gov/dataset/ca-bulletin-118-groundwater-basins
# basins_url = "http://atlas-dwr.opendata.arcgis.com/datasets/b5325164abf94d5cbeb48bb542fa616e_0.zip"
# basins_url = "https://gis.data.cnra.ca.gov/datasets/bdfc6550b4f3401a83ad1e2f468140ca_0.zip?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
basins_url = "https://gis.data.cnra.ca.gov/api/download/v1/items/49807a1fbc584631bdf88d9ca71dd083/shapefile?layers=0"
# zipname1 = strsplit(basins_url, "/")[[1]][length(strsplit(basins_url, "/")[[1]])]
zipname = paste0("B118_basins.zip")
#retrieve zip file from url
basins_dl = GET(basins_url, write_disk(file.path(scratch_dir,zipname), overwrite = TRUE))
#unzip in the scratch work directory
unzip(file.path(scratch_dir,zipname), exdir = file.path(scratch_dir))#, list = TRUE)
# Read shapefile
gw_basin_name = "i08_B118_CA_GroundwaterBasins"
basins_all = st_read(file.path(scratch_dir, paste0(gw_basin_name,".shp")))
#subset the Scott GSP basin and reproject to 3310
basin = basins_all[basins_all$Basin_Numb %in% c("1-005"),]
basin = st_transform(basin, st_crs(3310))
#Remove scratch work
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "xml")
file.remove(file.path(scratch_dir,paste(gw_basin_name, extension_list, sep = ".")))

# USGS Watersheds, waterbodies, and streams -------------------------------

# _Scott River watershed --------------------------------------------------
# Accessed at https://prd-tnm.s3.amazonaws.com/index.html?prefix=StagedProducts/Hydrography/NHD/HU8/Shape/
# Which was accessed via https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
HUC8_num = c(18010208)#, 18010207, 18010205) # scott, shasta, butte
valley_name = ("scott")#, "shasta", "butte")
# huc_table = data.frame(huc8 = HUC8_nums, basin = valley_names)

#Download the zipped files for each watershed, unzip, load into R, write to database (overwriting for each new basin)
# for(i in 1:3){
huc = HUC8_num # huc_table$huc8[i]
basin_name = valley_name #huc_table$basin[i]

# wsh_url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_",huc,"_HU8_Shape.zip")
wsh_url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/Shape/NHD_H_",huc,"_HU8_Shape.zip")
zipname = paste0("NHD_H_",huc,"_HU8_Shape.zip")
# Download from USGS site and write to the working drive (scratch_dir)
wsh_dl = GET(wsh_url, write_disk(file.path(scratch_dir, zipname), overwrite = TRUE))
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(file.path(scratch_dir, zipname), exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
#Read watershed and named stream shapefiles into R
wbdhu8 = st_read(file.path(scratch_dir, "Shape", "WBDHU8.shp"))
nhdwaterbody = st_read(file.path(scratch_dir, "Shape", "NHDWaterbody.shp"))
flowlines = st_zm(st_read(file.path(scratch_dir, "Shape", "NHDFlowline.shp")))

#Subset flowlines
#Note: keep named streams and unnamed ReachCode 18010208002394.
#This is a channelized ditch connecting Johnson Creek to Crystal Creek
named_streams = flowlines[(!is.na(flowlines$gnis_name)) | flowlines$reachcode == 18010208002394,]
#And, name this reach to make it part of Johnson Creek
named_streams$gnis_name[named_streams$reachcode == 18010208002394] = "Johnson Creek"

#reproject
watershed = st_transform(wbdhu8, st_crs(3310))
nhdwaterbody = st_transform(nhdwaterbody, st_crs(3310))
named_streams = st_transform(named_streams, st_crs(3310))

#Delete zipped files
file.remove(file.path(scratch_dir, "NHD_H_18010208_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "NHD_H_18010207_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "NHD_H_18010205_HU8_Shape.zip"))
file.remove(file.path(scratch_dir, "Shape", list.files(file.path(scratch_dir, "Shape")))) #deletes files in unzipped "Shape" folder
unlink(file.path(scratch_dir, "Shape"),recursive = TRUE) #Removes an empty file


# USGS Flow gages  -------------------------------------------------------------------


#find daily surface water data in CA
pCode = c("00060") #daily avg streamflow
swCA_dl = readNWISdata(stateCd="CA", parameterCd=pCode,
                       service="site", seriesCatalogOutput=TRUE)
swCA = swCA_dl[swCA_dl$parm_cd %in% pCode,]

HUC8_nums = c(18010208) #, 18010207, 18010205) # scott, shasta, butte
sw_scott = swCA[swCA$huc_cd %in% HUC8_nums,]

sw_scott_sp = st_as_sf(sw_scott, coords = c("dec_long_va","dec_lat_va"),
                       crs = crs("+init=epsg:4326")) #assign WGS84 projection to coordinates
sw_scott_sp = st_transform(sw_scott_sp, crs("+init=epsg:3310"))
usgs_gauges = sw_scott_sp[(watershed),]

# DEM layer (topography) ------------------------------------------------------

# USGS DEMs 1/3 arc-second
# Accessed via https://viewer.nationalmap.gov/basic/#productSearch

# old URLs
# dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/IMG/n42w123.zip"
# dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/IMG/n42w124.zip"
# dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/n42w123/USGS_13_n42w123_20210624.tif"
# dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/n42w124/USGS_13_n42w124_20210624.tif"
# Current URL as of May 2024
dem123_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/historical/n42w123/USGS_13_n42w123_20210623.tif"
dem124_url = "https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/historical/n42w124/USGS_13_n42w124_20210623.tif"
dem_urls = c(dem123_url, dem124_url)

dem_url = dem_urls[1]
# Download from USGS site and write to the working drive (scratch_dir)
tif_name = strsplit(dem_url, "/")[[1]][length(strsplit(dem_url, "/")[[1]])]
if(!file.exists(file.path(scratch_dir,tif_name))){
  dem_dl = GET(dem_url, write_disk(file.path(scratch_dir,tif_name), overwrite = TRUE))
}
dem123 = rast(file.path(scratch_dir, tif_name))

dem_url = dem_urls[2]
# Download from USGS site and write to the working drive (scratch_dir)
tif_name = strsplit(dem_url, "/")[[1]][length(strsplit(dem_url, "/")[[1]])]
if(!file.exists(file.path(scratch_dir,tif_name))){
  dem_dl = GET(dem_url, write_disk(file.path(scratch_dir,tif_name), overwrite = TRUE))
}
dem124 = rast(file.path(scratch_dir, tif_name))

m <- merge(dem123,
           dem124) # getting some weird errors but it still runs the merge.
# plot(m)
dem_3310 = terra::project(x = m, y = crs("+init=epsg:3310"))

# Generate hillshade
wsh_10km = st_buffer(watershed, 1e4) # give it a buffer for figure backgrounds
dem_watershed=terra::crop(dem_3310, wsh_10km)
dem_shade=gray(0:100 / 100)


if(!file.exists(file.path(scratch_dir, "hillshade_cropped_raster.tif"))){
  #generate hillshade (for smaller maps)
  # hill_basin = hillShade(slope = terrain(dem_basin, "slope"), aspect = terrain(dem_basin, "aspect"))
  #generate cropped hillshade
  dem_watershed_slope = terra::terrain(dem_watershed, v = "slope", unit = "radians")
  dem_watershed_asp = terra::terrain(dem_watershed, v = "aspect", unit = "radians")
  hill_wsh = shade(slope = dem_watershed_slope, aspect = dem_watershed_asp,
                   angle = 45, direction = 0)
  terra::writeRaster(hill_wsh, filename = file.path(data_dir, "hillshade_cropped_raster.tif"), overwrite=T)

}


#_TABULAR DATA ------------------------------------------------------------


# Fort Jones USGS flow ----------------------------------------------------

# Retrieve fort jones gage data
fj_num = "11519500"
fj_flow = readNWISdv(siteNumbers = fj_num, parameterCd="00060" )
fj_flow = renameNWISColumns(fj_flow)
fj_flow$wy = year(fj_flow$Date); fj_flow$wy[month(fj_flow$Date) > 9] = fj_flow$wy[month(fj_flow$Date) > 9]+1


# NOAA NCDC ---------------------------------------------------------------

#Subfunctions
get_bbox_string = function(poly, return_half = 0){
  poly = st_transform(poly, st_crs(4326)) #convert to WGS84)
  north = st_bbox(poly)["ymax"]; south = st_bbox(poly)["ymin"]
  east = st_bbox(poly)["xmax"]; west = st_bbox(poly)["xmin"]
  bbox_poly = paste(c(south, west, north, east), collapse = ",")
  if(return_half>0){ ns_midpoint = south + 0.5*(north-south)}
  if(return_half == 1){bbox_poly = paste(c(ns_midpoint, west, north, east), collapse = ",")}
  if(return_half == 2){bbox_poly = paste(c(south, west, ns_midpoint, east), collapse = ",")}
  return(bbox_poly)
}

get_noaa_stations = function(cmk_token = "scKXkYaFjbbLtxmSnNYjWKBKXDvOCoeU", #received from NOAA on 2019-08-09: https://www.ncdc.noaa.gov/cdo-web/token
                             list_of_bboxes){

  #Initialize station table
  colnames_station_table = c("results.elevation","results.mindate",
                             "results.maxdate","results.latitude","results.name",

                                                        "results.datacoverage","results.id",
                             "results.elevationUnit","results.longitude")
  station_table = data.frame(matrix(NA, nrow = 0, ncol = 9))
  colnames(station_table) = colnames_station_table

  for(i in 1:length(list_of_bboxes)){
    bbox_string = list_of_bboxes[[i]]
    base_url = "https://www.ncdc.noaa.gov/cdo-web/api/v2/"
    stations_query_url = paste0(base_url, "stations?extent=", bbox_string)
    header_token <- structure(cmk_token , names = "token")
    # header_limit <- structure(200 , names = "limit")
    stations_dl = GET(stations_query_url, httr::add_headers(header_token))#, header_limit))

    #Parse download
    stations_dl_unlisted = unlist(content(stations_dl, "parsed" ))
    metadata=stations_dl_unlisted[1:3]
    stations_dl_unlisted = stations_dl_unlisted[-(1:3)] # scrape off 3 metatata arguments
    stations_colnames = names(stations_dl_unlisted[1:9])
    noaa_stations = data.frame(matrix(stations_dl_unlisted, ncol = 9, byrow = T))
    colnames(noaa_stations) = stations_colnames

    station_table = rbind(station_table, noaa_stations)
  }
  #Eliminate duplicates
  # sum(duplicated(station_table))
  station_table = station_table[!duplicated(station_table),]
  #Split up results ID into dataset type and station ID
  results_id_split=matrix(unlist(strsplit(as.character(station_table$results.id), split=":")), ncol=2, byrow=T)
  station_table$results.type = results_id_split[,1]
  station_table$station.id = results_id_split[,2]

  #Make stations table spatial
  stations_sp = station_table
  stations_sp$results.latitude = as.numeric(as.character(stations_sp$results.latitude))
  stations_sp$results.longitude = as.numeric(as.character(stations_sp$results.longitude))
  stations_sp = st_as_sf(x = stations_sp,
                         coords = c("results.longitude","results.latitude"),
                         crs=st_crs(4326))
  # coordinates(stations_sp) = ~results.longitude + results.latitude
  # proj4string(stations_sp) <- CRS("+init=epsg:4326") #assign WGS84 projection to coordinates

  return(list(stations_sp, station_table))
}

get_noaa_data = function(station_list){

  base_url = "https://www.ncei.noaa.gov/access/services/data/v1"
  dataset = "daily-summaries"
  # station_list = c("USC00041316","USC00043182","USC00042899", "USC00043614", "USC00049866", "US1CASK0005")
  stations = paste(station_list,collapse = ",") # Callahan, Ft Jones, Etna, Greenview, Yreka, Yreka NW
  start_date = "1800-01-01"
  end_date = Sys.Date()
  data_types = paste(c("TMAX", "TMIN", "PRCP", "SNOW", "SNWD"), collapse=",")

  noaa_url = paste0(base_url, "?dataset=", dataset,
                    "&stations=", stations,
                    "&startDate=", start_date,
                    "&endDate=", end_date,
                    "&dataTypes=", data_types,
                    "&format=csv",
                    "&includeAttributes=0&includeStationName=true&includeStationLocation=true",
                    "&units=metric")

  noaa_dl = GET(noaa_url)
  noaa = as.data.frame(content(noaa_dl, "parsed"))
  return(noaa)
}

archive_noaa_data = function(noaa_data){

  #Update the live tables, and make an archive datestamped copy
  #Archive
  noaa_archive_name = paste0("noaa_daily_data_ghcnd_",format(Sys.Date(), "%Y.%m.%d"),".csv" )

  # #Overwrite copy on server
  # copy_to(dest = siskiyou_tables, df = noaa_data,
  #         name = "noaa_daily_data", overwrite=TRUE,
  #         temporary = FALSE, indexes = list("STATION","DATE"))
}


#Pull station list from noaa website
bbox_scott = get_bbox_string(watershed)
station_info = get_noaa_stations(list_of_bboxes = bbox_scott)


station_sp = station_info[[1]]
station_sp = st_transform(station_sp, st_crs(3310))
station_table = station_info[[2]]
#Only include GHCND stations. These are historical daily records.
ghcnd_stations = station_table$station.id[station_table$results.type == "GHCND"]
noaa_updated_dataset = get_noaa_data(station_list = ghcnd_stations)

# archive_noaa_data(noaa_data = noaa_updated_dataset)

noaa = noaa_updated_dataset
noaa_stations = station_table[station_table$results.type == "GHCND",]
noaa_station_sp = station_sp

rm(list = c("noaa_updated_dataset", "station_table", "station_sp")) # remove older variable names

# Notes: optional extra weather datasets (for scott; could be others in other basins)
# TMAX 	Maximum temperature
# TMIN 	Minimum temperature
# TOBS 	Temperature at the time of observation
# DAPR 	Number of days included in the multiday precipitation total (MDPR) 	1949-12-19 	2018-11-29
# MDPR 	Multiday precipitation total (use with DAPR and DWPR, if available) 	1949-12-19 	2018-11-29
# PRCP 	Precipitation #tenths of a mm
# SNOW 	Snowfall
# SNWD 	Snow depth
# WT01 	Fog, ice fog, or freezing fog (may include heavy fog)
# WT03 	Thunder
# WT04 	Ice pellets, sleet, snow pellets, or small hail"
# WT05 	Hail (may include small hail)
# WT06 	Glaze or rime
# WT08 	Smoke or haze
# WT09 	Blowing or drifting snow
# WT11 	High or damaging winds
# WT14 	Drizzle
# WT16  Rain (may include freezing rain, drizzle, and freezing drizzle)"


#_LOAD FROM LOCAL ---------------------------------------------------------


# CIMIS, ET ref -----------------------------------------------------------

# Update process: Log in to https://cimis.water.ca.gov, go to Data, request a daily CSV report, 1/1/2015-present day (record starts april 2015)
cimis = read.csv(file.path(data_dir, "CIMIS Stn 225 Daily 2015.04.19 to 2022.02.27.csv"))
cimis$Date=as.Date(cimis$Date, format = "%m/%d/%Y")
et_0 = cimis[,c("Date", "ETo..in.")]
colnames(et_0)=c("Date","ET_ref_in")

# Land Use DWR 2016 ----------------------------------------------------------------

# This is only for the Scott watershed

#Accessed from email sent from Flackus, Todd@DWR <Todd.Flackus@water.ca.gov>
lu_zip = file.path(data_dir,"Siskiyou2017_Final_WaterSourceDAU003Clip.shp.zip")

zipname = strsplit(lu_zip, "/")[[1]][length(strsplit(lu_zip, "/")[[1]])]
# Unzip file and save in the working directory (defaults to Documents folder)
unzip(lu_zip, exdir = file.path(scratch_dir)) #, list = TRUE) # just lists files, does not unzip
#Read adjudicated shapefiles into R
lu_all = st_read( file.path(scratch_dir, "Siskiyou2017_Final_WaterSourceDAU003Clip.shp"))
landuse_dwr_2016 = st_transform(lu_all, st_crs("+init=epsg:3310"))
# lu = lu_all[county,] # not necessary, this is already clipped to the watershed

  #remove files from scratch drive
file.remove(file.path(scratch_dir, zipname))
extension_list = c("cpg", "dbf", "prj", "shx", "shp", "sbn", "sbx", "shp.xml")
file.remove(file.path(scratch_dir,paste("Siskiyou2017_Final_WaterSourceDAU003Clip", extension_list, sep = ".")))

# Land Use Updated 2018

# SVIHM fields: MAR, ILR, Adjudicated Zone layers -----------------------------------------------------------------------

svihm_ref_dir = file.path(data_dir,"SVIHM Reference Data")
svihm_fields = st_read(dsn = svihm_ref_dir, layer = "Landuse_20190219")
adj_zone = st_read(dsn = svihm_ref_dir, layer = "Adjudicated Area")
svihm_fields = st_transform(svihm_fields, st_crs(adj_zone))


# Read in the Fields Attribute text file. Process SVIHM fields spatial layer
fields_column_classes = c(rep("integer",4),
                        "numeric","integer","numeric","numeric",
                        "integer","integer","character",
                        rep("NULL",16)) # get rid of empty columns in the text file
fields_tab = read.table(file.path(svihm_ref_dir,"polygons_table.txt"),
                      header = T, comment.char = "!",
                      fill = T, sep = "\t", colClasses = fields_column_classes)
colnames(fields_tab) = c("Field_ID",colnames(fields_tab)[2:11])
# MAR fields table
mar_fields = read.table(file.path(data_dir,"SVIHM Reference Data","MAR_Fields.txt"),
                        comment.char = "!", skip = 1, header = F)
names(mar_fields) = c("Field_poly_num", "Max_infil_rate_m_day")

# _Process SVIHM fields  ------------------
#water source, land use color, overlap with adjudicated zone

# 1. Calculate the fraction of each polygon *inside* the adjudicated zone.
# svihm_fields$fraction_in_adj = 0 # initialize new column
#
# for(i in 1:max(svihm_fields$Polynmbr)){
#   selector = svihm_fields$Polynmbr==i
#   field = svihm_fields[selector,]
#   if(!gIsValid(field)){
#     field = gBuffer(field, width = 0) # fix invalid geoms and warn about it
#     print(paste("polygon number",i,"invalid"))}
#
#   if(gIntersects(adj_zone, field)){
#     overlap_poly = raster::intersect(field, adj_zone)
#     svihm_fields$fraction_in_adj[selector] = round(gArea(overlap_poly) / gArea(field), digits = 3) # otherwise get leftover digit junk
#   }
# }
#
# # 2. Assign status as inside or outside adjudicated zone (based on overlap threshold)
# in_adj_threshold = 0.05 # lower numbers mean, just a sliver overlapping are included
# fields_inside_adj = svihm_fields$Polynmbr[svihm_fields$fraction_in_adj > in_adj_threshold]
# fields_outside_adj = svihm_fields$Polynmbr[!(svihm_fields$Polynmbr %in% fields_inside_adj)]
#
# fields_inside_adj = svihm_fields[svihm_fields$fraction_in_adj > in_adj_threshold,]
# fields_outside_adj =  svihm_fields[svihm_fields$fraction_in_adj <= in_adj_threshold,]

# 3. Process water source
# Color by water source - initialize columns
svihm_fields$wat_source_from_svihm = NA
svihm_fields$wat_source_from_svihm_color = NA

# make water source color table
wat_source = c(1,2,3,4,5,999)
wat_source_descrip = c("SW","GW","Mixed", "Sub-irrigated","Dry","Unknown")
wat_source_color = c("dodgerblue","firebrick2","darkorchid1","green","yellow","gray")
wat_source_df = data.frame(ws_code = wat_source,
                           descrip = wat_source_descrip,
                           color = wat_source_color)
#match water source codes and colors
svihm_fields$wat_source_from_svihm = fields_tab$Water_Source[match(svihm_fields$Polynmbr, fields_tab$Field_ID)]
svihm_fields$wat_source_desc_from_svihm = wat_source_df$descrip[match(svihm_fields$wat_source_from_svihm, wat_source_df$ws_code)]
svihm_fields$wat_source_from_svihm_color = wat_source_df$color[match(svihm_fields$wat_source_from_svihm, wat_source_df$ws_code)]


# 4. Process land use
alf_col = "forestgreen"; pasture_col = "darkolivegreen2"
natveg_col = "khaki"; noet_noirr_col = "red" # CURRENTLY HERE
lu_descrip_in_shp = c("Alfalfa/Grain","Pasture",
                      "ET/No Irrigation","No ET/No Irrigation")
lu_color = c(alf_col, pasture_col, natveg_col,  noet_noirr_col)
# svihm_fields$landuse_color1 = lu_color[match(svihm_fields$LNDU_SIM1, lu_descrip_in_shp)] # only difference is more NAs

# Landuse key for fields table
# # alfalfa = 25; palture = 2; ET_noIrr = 3 (native veg, assumes kc of 0.6);  noET_noIrr = 4; water = 6
lu = c(25,2,3,4,6)
lu_descrip = c("Alfalfa","Pasture","ET_noIrr","noET_noIrr", "Water")
lu_descrip_in_shp = c("Alfalfa/Grain","Pasture",
                      "ET/No Irrigation","No ET/No Irrigation", "Water")
lu_color = c(alf_col, pasture_col, natveg_col, noet_noirr_col,"dodgerblue")
lu_df = data.frame(lu_code = lu,
                   descrip = lu_descrip,
                   descrip_shp = lu_descrip_in_shp,
                   color = lu_color)
# assign color based on land use in the shapefile
svihm_fields$landuse_color2b = lu_color[match(svihm_fields$LNDU_SIM2b, lu_descrip_in_shp)]

# assign the landuse in the fields table, check match to shapefile, and assign color to the fields table landuse
svihm_fields$lu_from_svihm = NA
svihm_fields$lu_from_svihm = fields_tab$Landuse[match(svihm_fields$Polynmbr, fields_tab$Field_ID)]
svihm_fields$lu_desc_from_svihm = lu_df$descrip_shp[match(svihm_fields$lu_from_svihm, lu_df$lu_code)]
# View(svihm_fields@data[svihm_fields$lu_desc_from_svihm != svihm_fields$LNDU_SIM2b,]) # mostly reassigning no et/no irr fields to be water surface
svihm_fields$lu_from_svihm_color = lu_df$color[match(svihm_fields$lu_from_svihm, lu_df$lu_code)]

# clean up columns to keep only ones that will get used
keep_cols = c("Polynmbr","Acres",#"fraction_in_adj",
              "wat_source_from_svihm","wat_source_desc_from_svihm","wat_source_from_svihm_color",
              "lu_from_svihm","lu_desc_from_svihm","lu_from_svihm_color",
              "geometry")
svihm_fields = svihm_fields[,keep_cols]
colnames(svihm_fields) = c("Field_ID", "Acres",#"fraction_in_adj",
                                "wat_source","wat_source_desc","wat_source_color",
                                "landuse","landuse_desc","landuse_color",
                           "geometry")

#5. Add MAR status
svihm_fields$mar_field = "No"
svihm_fields$mar_field[svihm_fields$Field_ID %in% mar_fields$Field_poly_num] = "Yes"



# SVIHM grid and Discharge Zone ----------------------------------------------------

svihm_ref_dir = file.path(data_dir,"SVIHM Reference Data")
discharge_zone = st_read(dsn = svihm_ref_dir, layer = "Discharge_Zone")
discharge_zone = st_transform(discharge_zone, st_crs(watershed))

# Model raster

# discharge_zone_cells = raster(file.path(data_dir,"SVIHM Reference Data","ET_Extinction_Depth_raster"))
# discharge_zone_cells = projectRaster(discharge_zone_cells, crs = crs(watershed))
# svihm_raster = discharge_zone_cells
# rm(list = "svihm_raster")

# grid_shp = st_read(dsn = svihm_ref_dir, layer = "100m_grid_UTM_20180126")
# grid_shp2 = st_transform(grid_shp, crs = st_crs(watershed))
# # ^ need to transform the .shp first to conserve the number of gridcells in the raster. rather than transforming the raster
# n_row = max(grid_shp2$row)
# n_col = max(grid_shp2$column)
# svihm_raster = raster(nrows = n_row, ncols = n_col,  crs = crs(grid_shp2),
#                      xmx = bbox(grid_shp2)["x","max"], xmn = bbox(grid_shp2)["x","min"],
#                      ymx = bbox(grid_shp2)["y","max"], ymn = bbox(grid_shp2)["y","min"])
# values(svihm_raster) = NA
# rm(list = "grid_shp")
# rm(list = "grid_shp2")

# # Well table
# hob_info = read.table(file.path(svihm_ref_dir,"hob_wells.txt"), header = F, skip = 4)
# colnames(hob_info) = c('OBSNAM', 'LAYER', 'ROW', 'COLUMN', 'IREFSP', 'TOFFSET', 'ROFF', 'COFF', 'HOBS', 'STATISTIC', 'STAT-FLAG', 'PLOT-SYMBOL')
# # read in longer names to match DWR_1 well abbrevs to wl_obs data
# mon_info = read.csv( file.path(svihm_ref_dir, "Monitoring_Wells_Names.csv"))
# dwr_in_model_short_names = c("DWR_1","DWR_2","DWR_3","DWR_4","DWR_5")
#
# long_name_wells = mon_info[mon_info$Well_ID %in% dwr_in_model_short_names,]
# # match 1: OBSNAM to list of short names (irrelevant unless they ever get reordered)
# hob_info$longname = ""
# longname_matcher = match(hob_info$OBSNAM, long_name_wells$Well_ID)
# hob_info$longname[hob_info$OBSNAM %in% dwr_in_model_short_names] =
#   long_name_wells$Well_ID_2[longname_matcher[!is.na(longname_matcher)]]
# # match 2: short names to long names
# # match 3: long names to well codes
#
# hob_info$well_code = #[hob_info$OBSNAM %in% dwr_in_model_short_names] =
#   wells$well_code[match(hob_info$longname, wells$well_name)]
# hob_info$well_code[is.na(hob_info$well_code)] = hob_info$OBSNAM[is.na(hob_info$well_code)]
# hob_info$well_code[hob_info$well_code == "A4_1"] = "A41"

# # this matching exercise is somehow wildly frustrating. Going to hardcode this bullshit.
# # actually, I was getting the right answer after all. DWR_1 and 3 have no match in the wells table.
# # hob_info$well_code = hob_info$OBSNAM
# # hob_info$well_code[hob_info$OBSNAM == "DWR_1"] = wells$well_code[wells$well_name == "43N09W02P002M" & !is.na(wells$well_name)]
# hob_info$well_code[hob_info$OBSNAM == "DWR_2"] = wells$well_code[wells$well_name == "44N09W25R001M" & !is.na(wells$well_name)]
# # hob_info$well_code[hob_info$OBSNAM == "DWR_3"] = wells$well_code[wells$well_name == "44N09W28P001M" & !is.na(wells$well_name)]
# hob_info$well_code[hob_info$OBSNAM == "DWR_4"] = wells$well_code[wells$well_name == "43N09W23F001M" & !is.na(wells$well_name)]
# hob_info$well_code[hob_info$OBSNAM == "DWR_5"] = wells$well_code[wells$well_name == "43N09W24F001M" & !is.na(wells$well_name)]



# other SVIHM results -----------------------------------------------------

# these are declared in Ch. 3 if they don't exist
# observed hydraulic gradient table, grad
# stream-aquifer exchange table, stream_aq_tab

# Flow regimes -------------------------------------------------------------------------

# CDFW 2017 interim instream flows
cdfw_tab = read.csv(file.path(data_dir,"cdfw_2017_instream_flows.csv"))
colnames(cdfw_tab) = c("start_date_month","start_date_day","end_date_month","end_date_day","rec_flow_cfs")
# Forest Service water right
fs_tab = read.csv(file.path(data_dir,"USFS Scott Water Right.csv"))
colnames(fs_tab) = c("start_date_month","start_date_day","end_date_month","end_date_day","rec_flow_cfs")
# CDFW 2021 emergency drought minimum flows
cdfw_2021 = read.csv(file.path(data_dir, "cdfw_2021c_emergency_drought_flows.csv"))
# State Board 2025 Emergency Flows Regime
eflow25_tab = read.csv(file.path(data_dir,"Scott River 2025 Drought Emergency Minimum Flows.csv"))


# _MODEL RESULTS ----------------------------------------------------------

svihm_dir = file.path(file.path(data_dir, "SVIHM Model Results_2025.10.13"))
# FJ flow out
scen_fj_out = read.csv(file.path(svihm_dir, "scen_combined_fj.csv"))
# Functional flows based on FJ flow out for each scenario
ff_scen = read.csv(file.path(svihm_dir, "Scenario functional flows from FJ Gauge.csv"))
# Scenario parameter summary
scen_params = read.csv(file.path(svihm_dir, "scenarios_param_summary.csv"))

get_svihm_budget_tabs = function(budget_dir){
  allfiles = list.files(budget_dir)
  swbm_filenames = allfiles[grep(pattern = "SWBM", x = allfiles)]
  swbm_scen_names = gsub(x=swbm_filenames, pattern = "_SWBM_monthly_Budget.csv",
                         replacement="")
  mf_filenames = allfiles[grep(pattern = "MODFLOW", x = allfiles)]
  mf_scen_names = gsub(x=mf_filenames, 
                       pattern = "_MODFLOW Budget m3 per month.csv",
                       replacement="")
  if(length(setdiff(swbm_scen_names, mf_scen_names))>0 ){print("Different number of SWBM vs Modflow scenario budgets in available files")}
  swbm_bud = vector("list", length = length(swbm_scen_names))
  mf_bud = vector("list", length = length(mf_scen_names))
  for(i in 1:length(swbm_scen_names)){
    # read swbm budget file and add to list
    swbm_file = swbm_filenames[i]
    scen_id = gsub(x=swbm_file, pattern = "_SWBM_monthly_Budget.csv",
                   replacement="")
    swbm_bud[[i]] = read.csv(file.path(budget_dir,swbm_file))
    names(swbm_bud)[i] = scen_id
    #read modflow budget file and add to list
    mf_file = mf_filenames[i]
    scen_id = gsub(x=mf_file, 
                   pattern = "_MODFLOW Budget m3 per month.csv",
                   replacement="")
    mf_bud[[i]]=read.csv(file.path(budget_dir, mf_file))
    names(mf_bud[i])=scen_id
  }
  return(list(swbm = swbm_bud, mf= mf_bud))
}


budgets = get_svihm_budget_tabs(budget_dir = file.path(svihm_dir, "Budgets"))

swbm_budgets = budgets$swbm
mf_budgets = budgets$mf
  

# read_modflow_heads = function(scenario_directory, num_stress_periods = 336){
#   start_time <- Sys.time()
# 
#   # User Inputs --
#   NLAY = 2                 # Number of layers in model
#   NSP = num_stress_periods                # Number of stress peridos for which heads are printed
#   filename = file.path(scenario_directory,'SVIHM.hds')   #Name of binary head file
#   No_Flow_Val = 9999       #Value of no flow cells
#   # output_dir = 'Results/'  #Output directory
# 
#   # Read Heads --
#   H_by_SP = list()
#   fid = file(filename, "rb")
#   bytes = 0                                     #Bytes counter
#   p=1
# 
#   for(k in 1:NSP){
#     print(paste0('Reading Heads for Stress Period ', k))
#     KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period
#     KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
#     PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
#     TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
#     DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
#                     "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
#     NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
#     NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
#     ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number
#     H = array(data = NA, dim = c(NROW, NCOL, NLAY))
#     H1 = matrix(readBin(fid, numeric(), n=92400, size = 4), nrow = NROW, ncol = NCOL, byrow = T)  #Read in head matrix
#     H[,,1] = H1
#     for (i in 2:NLAY){ # Read in data for remaining layers
#       KSPT = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Time step number in stress period
#       KPER = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Stress period number
#       PERTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                        #Time in the current stress period
#       TOTIM = readBin(fid, numeric(), n = 1, size = 4); bytes = bytes + 4                         #Total elapsed time
#       DESC =  readBin(readBin(fid, "raw", n=16L, size=1L, endian="little"),
#                       "character", n=1L, endian="little"); bytes = bytes + 16            #Description of the array
#       NCOL = readBin(fid, integer(), n = 1, size = 4); bytes = bytes + 4                          #Number of columns in the model
#       NROW = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Number of rows in the model
#       ILAY = readBin(fid, integer(), endian = "little", size = 4); bytes = bytes + 4              #Current layer number
#       H_temp = matrix(readBin(fid, numeric(), n=92400, size = 4), nrow = NROW, ncol = NCOL, byrow = T)  #Read in head matrix
#       eval(parse(text = paste0('H[,,NLAY] = H_temp')))
#     }
#     H[H==No_Flow_Val] = NaN
# 
#     #add to list
#     H_by_SP[[k]] = H
#   }
# 
#   closeAllConnections()
#   end_time <- Sys.time()
#   print(paste0('Total Run Time was ', round(end_time - start_time, digits = 1), ' seconds'))
# 
#   return(H_by_SP)
# }
# 
# 
# read_ground_surface_elev = function(scenario_directory){
#   filename = file.path(scenario_directory,'SVIHM.dis')   #Name of discretization file
# 
#   disLines = readLines(filename)
#   top_line = grep(pattern = "TOP of Model", x = disLines)+1
#   bottom_line = grep(pattern = "BOTTOM of Layer   1", x = disLines)-1
# 
#   gs_elev_lines = disLines[top_line:bottom_line]
# 
#   gs_elev_values = as.numeric(unlist(strsplit(trimws(gs_elev_lines), split = "  ")))
# 
#   gs_elev_matrix = matrix(gs_elev_values, nrow = 440, byrow=T)
#   # image(t(gs_elev_matrix)[,440:1])
#   return(gs_elev_matrix)
# }
# 
# if(read_in_model_results==T){
#   modflow_heads_bc = read_modflow_heads(scenario_directory = file.path(svihm_scenarios_dir, "basecase"))
#   ground_elev = read_ground_surface_elev(scenario_directory = file.path(svihm_scenarios_dir, "basecase"))
# 
# }




save.image(file = file.path(here::here(), "manuscript_data.RData"))

