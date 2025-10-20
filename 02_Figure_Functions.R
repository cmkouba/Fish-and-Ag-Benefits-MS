# 04_Ch2_Figure_Functions
# Figures for Ch. 2 manuscript

#Explanation:
# --Load from local disk if you have previously loaded from server and saved an .RData of layers.
# --Load from the server and save the workspace if you do not have a local RData of the layers saved.
# -- If you want to update an existing .RData file of layers, simply delete or rename the old one.
# -- Save workspace option provided in case of not wanting to overwrite existing .RData file



# setup -------------------------------------------------------------------

# Conversion factors
in_to_mm = 25.4
af_to_m3 = 1/3.2808 * (1/3.2808)^2 * 43560# m/ft * m^2/ft^2 * ft^2/acre
cfs_to_m3sec = 1 / 35.3147
cfs_to_m3day = 1 / 35.3147 * 60*60*24 # 1 cfs / cubic ft per cubic m * seconds per day

# Set colors for all figures
color_cities= "black"
color_adj = "black"
color_dz = "darkorchid"
sim_col = "black"
obs_col = "dodgerblue"
# water budget components
precip_col = "mediumpurple"#"royalblue3"
et_col = "darkgoldenrod2"
et_crop_col = "darkgoldenrod2"
et_natveg_col = "darkgoldenrod4" #"darkolivegreen1"
sw_irr_col = "dodgerblue4"
gw_irr_col = "dodgerblue2"
rch_col = "green4"
stor_col = "gray70"#"mistyrose"
alf_col = "forestgreen"
pasture_col = "darkolivegreen2"
natveg_col = "wheat"
noet_noirr_col = "red"
hbv_col = "royalblue"
basecase_col = "gray20"
marilr_col = "darkorange"
mar_col = "firebrick1"
ilr_col = "yellow1"
color_river = "blue"
color_gauges = "yellow"

cities_centroid$label_xmod2 = c(-1.7, 2.7, -2.7, 2.2)
cities_centroid$label_ymod2 = c(  0, -0.5,  0.2, 0.6)

#Quartile-based water year type table
year_type_colors = data.frame(type = c("Dry","Below Avg","Above Avg","Wet"),
                              color = c("orangered","darkgoldenrod2","palegreen","dodgerblue"))


hillshade_palette_yellow = colorRampPalette(c("lightgoldenrod2", "lightgoldenrodyellow")) # This washed-out hillshade palette allows legends to be plotted on top

# data processing --------------------------------------------------------
riv = named_streams[named_streams$gnis_name == "Scott River",]
fj_gauge = usgs_gauges[usgs_gauges$site_no=="11519500",]
fj_gauge$station_nm="FJ Gauge"

# subfunctions ------------------------------------------------------------

wtr_yr = function(date_vector){
  water_year_vector = year(date_vector)
  water_year_vector[month(date_vector) >9] = year(date_vector[month(date_vector) >9]) + 1
  return(water_year_vector)
}

fj_wy_type_tab = function(fjd,
                          start_date = as.Date("1940-10-01"),
                          end_date = as.Date("2021-10-01"),
                          return_table = T, show_barplot = F, show_pointplot=F){
  # fj_num = "11519500"
  # fjd = readNWISdv(siteNumbers = fj_num, parameterCd="00060" )
  # fjd = renameNWISColumns(fjd)
  # fjd$wy = year(fjd$Date); fjd$wy[month(fjd$Date) > 9] = fjd$wy[month(fjd$Date) > 9]+1

  # Calculate water year type from total river runoff
  fjd_for_wy = fjd[fjd$Date>=start_date & fjd$Date<end_date,] # can switch to full record if desired
  fjd_for_wy$TAF_per_day = fjd_for_wy$Flow*cfs_to_TAF_per_day
  TAF_per_wy = aggregate(fjd_for_wy$TAF_per_day, by=list(fjd_for_wy$wy), FUN=sum)
  colnames(TAF_per_wy)=c("wy","tot_flow_TAF")

  # plot(sort(TAF_per_wy$tot_flow_TAF), ylim = c(0,1200),
  # main = "wys 1941-2019")
  # abline(h=quantile(TAF_per_wy$tot_flow_TAF))
  # grid()

  # use a quartile water year type scheme
  quartiles = quantile(TAF_per_wy$tot_flow_TAF, probs = seq(0, 1, 0.25))
  TAF_per_wy$type = NA
  TAF_per_wy$type[TAF_per_wy$tot_flow_TAF<quartiles[2]] = "Dry"
  TAF_per_wy$type[TAF_per_wy$tot_flow_TAF>quartiles[2] &
                    TAF_per_wy$tot_flow_TAF<=quartiles[3]] = "Below Avg" #Include the median year in below avg category
  TAF_per_wy$type[TAF_per_wy$tot_flow_TAF>quartiles[3] &
                    TAF_per_wy$tot_flow_TAF<quartiles[4]] = "Above Avg"
  TAF_per_wy$type[TAF_per_wy$tot_flow_TAF>quartiles[4]] = "Wet"
  TAF_per_wy$color = NA
  #Assign colors based on water year type
  TAF_per_wy$color = year_type_colors$color[match(TAF_per_wy$type, year_type_colors$type)]
  year_type_colors$type_descrip = c("Driest 25%", "Below Avg 25%", "Above Avg 25%", "Wettest 25%")

  if(show_barplot == T){
    # plot(x = TAF_per_wy$wy, y=TAF_per_wy$tot_flow_TAF,
    #      ylim = c(0,1200), pch = 19, col = TAF_per_wy$color,
    #      xlab = "Water Year", ylab = "Total Annual FJ Flow (TAF)",
    #      main = "Water Years 1941-2020")
    bar_names = TAF_per_wy$wy
    bar_names[bar_names %% 5>0] = ""
    barplot(names = bar_names, height =TAF_per_wy$tot_flow_TAF,
            xlim = c(0,1200), pch = 19, col = TAF_per_wy$color, alpha = .5,
            ylab = "Water Year", xlab = "Total Annual FJ Flow (TAF)",
            main = "Water Years 1941-2020" , horiz = T, cex.names = .7,
            las = 2)
    # abline(h = 2020 - seq(from = 1900, to = 2050, by = 5), col = "black")
    abline(v=quantile(TAF_per_wy$tot_flow_TAF)[c(2,4)], lty = 2)
    abline(v = quantile(TAF_per_wy$tot_flow_TAF)[3], col = "brown", lty = 2, lwd = 3)
    # grid()
    legend(x = "topright", col = year_type_colors$color, cex = .9,
           legend = year_type_colors$type_descrip, pch = 19, bg="white")
    legend(x = "bottomright", lwd = c(1,2), lty = c(2,2), col = c("black","brown"),cex = .9,
           legend = c("Quartile Boundaries", "Median Value (355 TAF)"))
  }
  if(show_pointplot == T){
    plot(x = TAF_per_wy$wy, y=TAF_per_wy$tot_flow_TAF,
         ylim = c(0,1200), pch = 19, col = TAF_per_wy$color,
         xlab = "Water Year", ylab = "Total Annual FJ Flow (TAF)",
         main = "Water Years 1941-2020")
    abline(v = 2020 - seq(from = 1900, to = 2050, by = 5), col = "black")
    abline(h=quantile(TAF_per_wy$tot_flow_TAF)[c(1,2,4,5)], lty = 2)
    abline(h = quantile(TAF_per_wy$tot_flow_TAF)[3], col = "brown", lty = 2, lwd = 3)
    # grid()
    legend(x = "topright", col = year_type_colors$color,
           legend = year_type_colors$type_descrip, pch = 19, bg="white")
    legend(x = "topleft", lwd = c(1,2), lty = c(2,2), col = c("black","brown"),
           legend = c("Quartile Boundaries", "Median Value (355 TAF)"))
  }

  if(return_table==T){return(TAF_per_wy)}
}

get_swbm_budget_table= function(scenario_id = "basecase",
                                start_date = as.Date("1990-10-01"),
                                end_date = as.Date("2018-09-30"),
                                nstress = 336){
  #1. look for predigested budget tables on local disk. If found, read in table

  swbm_file_path = file.path(svihm_results_dir,
                             paste(scenario_id, "SWBM_Water_Budget.csv"))

  if(file.exists(swbm_file_path)){
    swbm_monthly = read.csv(swbm_file_path)
    swbm_monthly$Month = as.Date(swbm_monthly$Month)
  }

  #2. If not found, need to postprocess. look for scenario directory in local drive
  if (!file.exists(swbm_file_path)) {
    scenario_dir = file.path(svihm_scenarios_dir, scenario_id)
    #2a. If not found, look for scenario directory in external harddrive
    if(!file.exists(file.path(scenario_dir,'monthly_water_budget.dat'))){
      scenario_dir = file.path(svihm_scenarios_backup_dir, scenario_id)
    }
    #3. Digest water budgets from scenario directory and save to local drive
    # date values for postprocessing results
    start_wy = year(start_date); if(month(start_date)>9){start_wy = year(start_date)+1}
    end_wy = year(end_date); if(month(end_date)>9){end_wy = year(end_date)+1}

    SWBM_Terms = c('Precipitation', 'SW Irrigation', 'GW Irrigation', 'ET', 'Recharge', 'Storage')
    SWBM_Monthly_m3 = read.table(file.path(scenario_dir,'monthly_water_budget.dat'), header = T)
    names(SWBM_Monthly_m3) = c('Month',SWBM_Terms)
    SWBM_Monthly_m3$Month = seq(start_date, by = "month", length.out = nstress)
    SWBM_Monthly_m3$Water_Year = rep(seq(start_wy,end_wy),each = 12)

    write.csv(x = SWBM_Monthly_m3, file = swbm_file_path, quote = F, row.names = F)

    swbm_monthly = SWBM_Monthly_m3
  }

  return(swbm_monthly)
}

get_aet_by_landuse_table = function(scenario_id = "basecase",
                                    start_date = as.Date("1990-10-01"),
                                    end_date = as.Date("2018-09-30"),
                                    nstress = 336){

  #1. look for predigested budget tables on local disk. If found, read in table

  aet_file_path = file.path(svihm_results_dir,
                             paste(scenario_id, "aET_by_luse.csv"))

  if(file.exists(aet_file_path)){
    aet_monthly = read.csv(aet_file_path)
    aet_monthly$Month = as.Date(aet_monthly$Month)
  }

  #2. If not found, need to postprocess. look for scenario directory in local drive
  if (!file.exists(aet_file_path)) {
    scenario_dir = file.path(svihm_scenarios_dir, scenario_id)
    #2a. If not found, look for scenario directory in external harddrive
    if(!file.exists(file.path(scenario_dir,'monthly_water_budget.dat'))){
      scenario_dir = file.path(svihm_scenarios_backup_dir, scenario_id)
    }
    #3. Digest water budgets from scenario directory and save to local drive
    # date values for postprocessing results
    start_wy = year(start_date); if(month(start_date)>9){start_wy = year(start_date)+1}
    end_wy = year(end_date); if(month(end_date)>9){end_wy = year(end_date)+1}

    aet_monthly_m3 = read.table(file.path(scenario_dir,'monthly_aET_by_luse.dat'), header = T)
    aet_monthly_m3$Month = seq(start_date, by = "month", length.out = nstress)
    aet_monthly_m3$Water_Year = rep(seq(start_wy,end_wy),each = 12)
    aet_monthly_m3$Stress_Period=NULL
    aet_monthly_m3 = aet_monthly_m3[,c("Month",names(aet_monthly_m3)[names(aet_monthly_m3)!= "Month"])]

    write.csv(x = aet_monthly_m3, file = aet_file_path, quote = F, row.names = F)

    aet_monthly = aet_monthly_m3
  }

  return(aet_monthly)
}

get_swbm_tab_with_separated_et = function(scen_id,
                                          start_date = as.Date("1990-10-01")){
  swbm_monthly = swbm_budgets[[scen_id]]
  # convert stress period to dates
  swbm_monthly$Month = seq.Date(from = start_date, by = "month",
                                length.out = nrow(swbm_monthly))
  swbm_monthly$Water_Year = wtr_yr(swbm_monthly$Month)
  
  aet_monthly = aet_by_lu_budgets[[scen_id]]
  et_crop = aet_monthly$Alfalfa_Irrigated + 
    aet_monthly$Grain_Irrigated + 
    aet_monthly$Pasture_Irrigated
  et_natveg = aet_monthly$Native_Vegetation
  
  swbm_monthly$`Crop ET` = et_crop * -1 # negative for swbm outflow
  swbm_monthly$`Nat. Veg. ET` = et_natveg * -1 # negative for swbm outflow
  
  return(swbm_monthly)
}
get_simulated_fj_outflow = function(scenario_id = "basecase",
                                    start_date = as.Date("1990-10-01"), end_date = as.Date("2018-09-30"),
                                    start_wy = 1991, end_wy = 2018){

  # 3. if not found, look for scenario dir on external drive
  fj_flow_file_path = file.path(svihm_results_dir,
                                paste(scenario_id, "FJ outflow.csv"))

  # 1. look for files on local disk. if found, read csv.
  if(file.exists(fj_flow_file_path)){
    FJ_Outflow = read.csv(fj_flow_file_path)
    FJ_Outflow$Date = as.Date(FJ_Outflow$Date)
  }

  # 2. if not, need to digest files. look for scenario dir on local disk
  if(!file.exists(fj_flow_file_path)){
    scenario_dir = file.path(svihm_scenarios_dir, scenario_id)
    if(!file.exists(file.path(scenario_dir,'Streamflow_FJ_SVIHM.dat'))){
      scenario_dir = file.path(svihm_scenarios_backup_dir, scenario_id)
    }

    #3. Digest water budgets from scenario directory and save to local drive

    FJ_Outflow = read.table(file.path(scenario_dir,'Streamflow_FJ_SVIHM.dat'), skip = 2)[,3]
    cfs_to_m3d = 1/35.3147 * 86400 # 1 m3/x ft3 * x seconds/day
    FJ_Outflow = data.frame(Date = seq(start_date, end_date, by = 'day'),
                            fj_flow_m3d = FJ_Outflow,
                            Flow = FJ_Outflow / cfs_to_m3d)
    # add water year
    FJ_Outflow$wy = year(FJ_Outflow$Date)
    FJ_Outflow$wy[month(FJ_Outflow$Date) > 9] =
      year(FJ_Outflow$Date[month(FJ_Outflow$Date) > 9]) + 1

    write.csv(x = FJ_Outflow, file = fj_flow_file_path, quote = F, row.names = F)
  }
  return(FJ_Outflow)
}




get_interp_precip_record = function(record_period = "long term"){


  make_station_table = function(weather_table, make_dist = F){
    #station abbreviation, number in the NOAA dataset, and column number in the daily_precip table
    # station_table = data.frame(abbrev = c("cal", "fj", "et", "gv"),
    #                               station = c("USC00041316", "USC00043182", "USC00042899", "USC00043614"),
    #                               col_num = c(1,2,3,4) + 1)
    station_table = data.frame(abbrev = c("cal", "fj", "et", "gv","yr", "y2"),
                               station = c("USC00041316", "USC00043182", "USC00042899", "USC00043614","USC00049866","US1CASK0005"),
                               col_num = c(1,2,3,4,5,6) + 1,
                               lat = NA, lon = NA)

    if(make_dist==T){
      #Assign coordinates to Station Locations from NOAA data
      for(i in 1:dim(station_table)[1]){
        station = station_table$station[i]
        station_table$lat[i] = unique(weather_table$LATITUDE[weather_table$STATION==station])
        station_table$lon[i] = unique(weather_table$LONGITUDE[weather_table$STATION==station])
      }
      #Assign a color to each station, for later visual rep of which station is used for which precip gap filling
      station_table$color = topo.colors(n = dim(station_table)[1])
      #Make station_table a SpatialPointsDataFrame and add proj
      coordinates(station_table) = ~lon + lat
      proj4string(station_table)=crs("+init=epsg:4326")

      #Use coordinates to calculate distance between all the stations
      station_dist = pointDistance(station_table, lonlat=T,  allpairs = T)
      #Make the matrix easier to query later
      station_dist = as.matrix(forceSymmetric(station_dist, uplo = "L"))
      diag(station_dist) = NA
      station_dist = as.data.frame(station_dist)
      rownames(station_dist) = station_table$abbrev; colnames(station_dist) = station_table$abbrev

      return(list(station_table, station_dist))
    }

    if(make_dist == F){return(station_table)}

  }


  make_daily_precip = function(weather_table = noaa,
                               daily_precip_start_date = as.Date("1943-01-01"),
                               daily_precip_end_date = model_end_date){

    record_days = seq(from = daily_precip_start_date, to = daily_precip_end_date, by = "days")

    #Subset data into stations
    cal = subset(weather_table, STATION=="USC00041316" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
    cal = data.frame(DATE = cal$DATE, PRCP = cal$PRCP)
    fj = subset(weather_table, STATION=="USC00043182" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
    fj = data.frame(DATE = fj$DATE, PRCP = fj$PRCP)
    et = subset(weather_table, STATION == "USC00042899" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
    et =data.frame(DATE = et$DATE, PRCP = et$PRCP)
    gv = subset(weather_table, STATION == "USC00043614" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
    gv =data.frame(DATE = gv$DATE, PRCP = gv$PRCP)
    yr = subset(weather_table, STATION == "USC00049866" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
    yr =data.frame(DATE = yr$DATE, PRCP = yr$PRCP)
    y2 = subset(weather_table, STATION == "US1CASK0005" & DATE >= daily_precip_start_date & DATE <= daily_precip_end_date)
    y2 =data.frame(DATE = y2$DATE, PRCP = y2$PRCP)


    #read in original data (wys 1991-2011)
    # print(data_dir)
    daily_precip_orig = read.table(file.path(data_dir,"svihm_precip_input_1991_2011.txt"))
    colnames(daily_precip_orig) = c("PRCP", "Date")
    daily_precip_orig$Date = as.Date(daily_precip_orig$Date, format = "%d/%m/%Y")
    daily_precip_orig$PRCP = daily_precip_orig$PRCP*1000


    ### COMPARISON TABLE FOR 2 STATIONS AND ORIG DATA
    daily_precip = data.frame(record_days)
    daily_precip = merge(x = daily_precip, y = cal, by.x = "record_days", by.y = "DATE", all=TRUE)
    daily_precip = merge(x = daily_precip, y = fj, by.x = "record_days", by.y = "DATE", all=TRUE)
    daily_precip = merge(x = daily_precip, y = et, by.x = "record_days", by.y = "DATE", all=TRUE)
    daily_precip = merge(x = daily_precip, y = gv, by.x = "record_days", by.y = "DATE", all=TRUE)
    daily_precip = merge(x = daily_precip, y = yr, by.x = "record_days", by.y = "DATE", all=TRUE)
    daily_precip = merge(x = daily_precip, y = y2, by.x = "record_days", by.y = "DATE", all=TRUE)
    daily_precip = merge(x = daily_precip, y = daily_precip_orig, by.x = "record_days", by.y = "Date", all=TRUE)
    colnames(daily_precip)=c("Date","PRCP_mm_cal", "PRCP_mm_fj","PRCP_mm_et","PRCP_mm_gv",
                             "PRCP_mm_yr", "PRCP_mm_y2","PRCP_mm_orig")

    # Compare original data to FJ-Cal mean
    daily_precip$mean_PRCP_fjcal = apply(X = daily_precip[,2:3], MARGIN = 1, FUN = mean, na.rm=T)

    #Add aggregation columns
    daily_precip$month_day1 = floor_date(daily_precip$Date, "month")
    daily_precip$water_year = year(daily_precip$Date)
    daily_precip$water_year[month(daily_precip$Date) > 9] = daily_precip$water_year[month(daily_precip$Date) > 9]+1

    return(daily_precip)
  }

  make_model_coeffs_table = function(months = 1:12,
                                     station_table = station_table,
                                     daily_precip = daily_precip,
                                     ys = c("fj", "cal"),
                                     xs = c("fj", "cal", "gv", "et", "yr", "y2")){

    model_coeff = expand.grid(months = months, Y_var = ys, X_var = xs)
    #take out matching combinations (e.g. x = fj, y = fj)
    model_coeff = model_coeff[as.character(model_coeff$Y_var) != as.character(model_coeff$X_var),]
    #Add model parameter columns
    model_coeff$intercept=NA; model_coeff$coeff_m=NA; model_coeff$r2=NA

    for(mnth in months){
      for(y in ys){
        for(x in xs){
          if(y == x){next} #no need for autoregression
          #Find the output table row
          model_coeff_index = which(model_coeff$months==mnth & model_coeff$X_var==x & model_coeff$Y_var==y)
          #Find the daily precip column number for X and Y stations
          col_num_x = station_table$col_num[station_table$abbrev == x]
          col_num_y = station_table$col_num[station_table$abbrev == y]
          #Declare X and Y in daily precip
          X = daily_precip[month(daily_precip$Date) == mnth, col_num_x]
          Y = daily_precip[month(daily_precip$Date) == mnth, col_num_y]
          #
          model_name = paste(y, "on", x, "in", month.abb[mnth] )

          #check to see if there are 0 matching pairs
          if(sum(!is.na(X) & !is.na(Y))==0){next}

          model = lm(Y~ 0 + X)
          # CASE DESCRIPTION?
          if(length(coef(model)) ==2){print(paste("2 coeff of this model", mnth,X,Y))
            model_coeff[model_coeff_index, 4:5] = coef(model)}
          if(length(coef(model)) ==1){
            model_coeff[model_coeff_index, 4] = 0; model_coeff[model_coeff_index, 5] = coef(model)}
          model_coeff$r2[model_coeff_index] = summary(model)$r.sq
        }
      }
    }
    return(model_coeff)
  }

  fill_fj_cal_gv_gaps_regression_table = function(model_coeff,
                                                  station_table = station_table,
                                                  daily_precip = daily_precip,
                                                  start_date = model_start_date,
                                                  end_date = model_end_date){
    #Function: use best available regression to fill in gaps in precip records for FJ and Cal

    #Rename daily precip table and restrict dates
    p_record = daily_precip[daily_precip$Date >= start_date & daily_precip$Date <= end_date,]

    ### Fill in gaps in FJ
    # Initialize the interpolated record as the official FJ record (with gaps)
    p_record$fj_interp = p_record$PRCP_mm_fj
    # Assign colors for the station attribution plot, indicating that we got these values from the FJ station.
    p_record$fj_interp_color[!is.na(p_record$fj_interp)] = station_table$color[station_table$abbrev == "fj"]

    # For each NA daily precip value in the FJ record, predict the value in the FJ record using the regression
    # coefficients from the station with the best R^2 value for predicting FJ.
    # (If that station also has a gap on that day, use the one with the next-best R^2 value, until the gap is filled.)
    for(i in 1:length(p_record$fj_interp)){
      if(is.na(p_record$fj_interp[i])){
        #find the appropriate regression coefficients for this month of fj data
        coefs = model_coeff[model_coeff$Y_var == "fj" & model_coeff$months == month(p_record$Date[i]), ]
        index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst

        #Predict rainfall
        for(j in 1:length(index_of_ranks)){
          if(is.na(p_record$fj_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
            # Use coef matrix to assign X variable station for the regression, based on ranked R^2
            coef_index = index_of_ranks[j]
            coef_x = coefs$X_var[coef_index]
            # Assign the column number and indicator color of the relevant predictor station
            daily_precip_column_num = station_table$col_num[station_table$abbrev == coef_x]
            daily_precip_x_var_color = station_table$color[station_table$abbrev == coef_x]
            # If there's data for that X variable, use it to predict rainfall at FJ for that day. also assign color.
            if(!is.na(p_record[i,daily_precip_column_num])){
              p_record$fj_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
              p_record$fj_interp_color[i] = daily_precip_x_var_color
            }
            #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
          }
        }

      }
    }

    ### Fill in gaps in Cal
    # Initialize the interpolated record as the official Cal record (with gaps)
    p_record$cal_interp = p_record$PRCP_mm_cal
    # Assign colors for the station attribution plot, indicating that we got these values from the Cal station.
    p_record$cal_interp_color[!is.na(p_record$cal_interp)] = station_table$color[station_table$abbrev == "cal"]

    for(i in 1:length(p_record$cal_interp)){
      if(is.na(p_record$cal_interp[i])){
        #find the appropriate regression coefficients for this month of Cal data
        coefs = model_coeff[model_coeff$Y_var == "cal" & model_coeff$months == month(p_record$Date[i]), ]
        index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst

        #Predict rainfall
        for(j in 1:length(index_of_ranks)){
          if(is.na(p_record$cal_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
            # Use coef matrix to assign X variable station for the regression, based on ranked R^2
            coef_index = index_of_ranks[j]
            coef_x = coefs$X_var[coef_index]
            daily_precip_column_num = station_table$col_num[station_table$abbrev == coef_x]
            daily_precip_x_var_color = station_table$color[station_table$abbrev == coef_x]
            # If there's data for that X variable, use it to predict rainfall at cal for that day
            if(!is.na(p_record[i,daily_precip_column_num])){
              p_record$cal_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
              p_record$cal_interp_color[i] =daily_precip_x_var_color
            }
            #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
          }
        }

      }
    }

    ### Fill in gaps in Greenview
    # Initialize the interpolated record as the official Greenview record (with gaps)
    p_record$gv_interp = p_record$PRCP_mm_gv
    # Assign colors for the station attribution plot, indicating that we got these values from the Greenview station.
    p_record$gv_interp_color[!is.na(p_record$gv_interp)] = station_table$color[station_table$abbrev == "gv"]

    for(i in 1:length(p_record$gv_interp)){
      if(is.na(p_record$gv_interp[i])){
        #find the appropriate regression coefficients for this month of Greenview data
        coefs = model_coeff[model_coeff$Y_var == "gv" & model_coeff$months == month(p_record$Date[i]), ]
        index_of_ranks = rev(order(coefs$r2)) # test the regressions in order of best R2 to worst

        #Predict rainfall
        for(j in 1:length(index_of_ranks)){
          if(is.na(p_record$gv_interp[i])){ #If this daily value didn't get filled by a previous calc in this for loop
            # Use coef matrix to assign X variable station for the regression, based on ranked R^2
            coef_index = index_of_ranks[j]
            coef_x = coefs$X_var[coef_index]
            daily_precip_column_num = station_table$col_num[station_table$abbrev == coef_x]
            daily_precip_x_var_color = station_table$color[station_table$abbrev == coef_x]
            # If there's data for that X variable, use it to predict rainfall at gv for that day
            if(!is.na(p_record[i,daily_precip_column_num])){
              p_record$gv_interp[i] =  coefs$intercept[coef_index] + (p_record[i, daily_precip_column_num] * coefs$coeff_m[coef_index])
              p_record$gv_interp_color[i] =daily_precip_x_var_color
            }
            #If there's no data for the best-ranked variable, it will go back to the beginning of the for loop and try again with other stations
          }
        }

      }
    }

    return(p_record)
  }

  get_daily_precip_table=function(final_table_start_date=model_start_date,
                                  final_table_end_date = model_end_date){

    #Generate daily precip table (same daterange as in original methodology) to make model coefficients
    #Each station is a column (plus a couple extra columns); each row is a date, WY 1944-2021
    daily_precip_regression = make_daily_precip(weather_table = noaa,
                                                daily_precip_start_date = as.Date("1943-10-01"),
                                                daily_precip_end_date = as.Date("2021-09-30"))
    wx_station_table = make_station_table(weather_table = noaa)
    model_coeff = make_model_coeffs_table(months = 1:12,
                                          station_table = wx_station_table,
                                          daily_precip = daily_precip_regression,
                                          ys = c("fj", "cal", "gv"),
                                          xs = c("fj", "cal", "gv", "et", "yr", "y2"))
    #Generate daily precip table to make the gap-filled records
    daily_precip_p_record = make_daily_precip(weather_table = noaa,
                                              daily_precip_start_date = final_table_start_date,
                                              daily_precip_end_date = final_table_end_date)
    p_record = fill_fj_cal_gv_gaps_regression_table(model_coeff=model_coeff,
                                                    station_table = wx_station_table,
                                                    daily_precip = daily_precip_p_record,
                                                    start_date = final_table_start_date,
                                                    end_date = final_table_end_date)

    # average interp-fj and interp-cal records and compare to original
    # p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp, gv_interp),
    #                                              MARGIN = 1, FUN = mean, na.rm=T)

    # p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, PRCP_mm_fj, PRCP_mm_cal, PRCP_mm_gv),
    #                                         MARGIN = 1, FUN = mean, na.rm=T)
    # p_record$interp_cal_fj_mean =  apply(X = dplyr::select(p_record, PRCP_mm_fj, PRCP_mm_cal),
    #                                         MARGIN = 1, FUN = mean, na.rm=T)
    # p_record$interp_cal_gv_mean =  apply(X = dplyr::select(p_record, PRCP_mm_gv, PRCP_mm_cal),
    #                                      MARGIN = 1, FUN = mean, na.rm=T)

    p_record$interp_cal_fj_gv_mean =  apply(X = dplyr::select(p_record, fj_interp, cal_interp, gv_interp),
                                            MARGIN = 1, FUN = mean, na.rm=T)
    p_record$interp_cal_fj_mean =  apply(X = dplyr::select(p_record,fj_interp, cal_interp),
                                         MARGIN = 1, FUN = mean, na.rm=T)
    p_record$interp_cal_gv_mean =  apply(X = dplyr::select(p_record, gv_interp, cal_interp,),
                                         MARGIN = 1, FUN = mean, na.rm=T)

    # Prepare to combine original precip and new regressed gap-filled fj-cal average
    p_record$stitched = p_record$PRCP_mm_orig
    p_record$stitched[is.na(p_record$PRCP_mm_orig)] = p_record$interp_cal_fj_mean[is.na(p_record$PRCP_mm_orig)]

    #Note: this include filling the leap days missing in the original record :)

    # orig_record_end_date = as.Date("2011-09-30"); orig_record_start_date = as.Date("1990-10-01")
    # orig_record = p_record$Date <= orig_record_end_date & p_record$Date >= orig_record_start_date
    # updated_record = p_record$Date > orig_record_end_date
    #
    # #Fill in 5 leap days in the original record using the gap-filled cal-FJ record
    # leap_day_finder_pre2011 = orig_record & is.na(p_record$PRCP_mm_orig)
    # p_record$PRCP_mm_orig[leap_day_finder_pre2011] = p_record$interp_cal_fj_mean[leap_day_finder_pre2011]
    #
    # #Subset original record
    # p_record$stitched = NA
    # p_record$stitched[orig_record] = p_record$PRCP_mm_orig[orig_record]
    # p_record$stitched[updated_record] = p_record$interp_cal_fj_mean[updated_record]

    return(p_record)
  }

  # Get interpolated rainfall record for desired period
  if(record_period == "long term"){
    p_record_all = get_daily_precip_table(final_table_start_date = as.Date("1935-10-01"),
                                          final_table_end_date = as.Date("2021-09-30"))
  }
  if(record_period == "1991-2011"){
    p_record_all = get_daily_precip_table(final_table_start_date = as.Date("1990-10-01"),
                                          final_table_end_date = as.Date("2011-09-30"))
  }
  if(record_period == "1991-2018"){
    p_record_all = get_daily_precip_table(final_table_start_date = as.Date("1990-10-01"),
                                          final_table_end_date = as.Date("2018-09-30"))
  }
  if(record_period == "1991-2020"){
    p_record_all = get_daily_precip_table(final_table_start_date = as.Date("1990-10-01"),
                                          final_table_end_date = as.Date("2020-09-30"))
  }


  return(p_record_all)
}


make_daily_flow_regime = function(regime_tab, record_dates){
  dates = seq.Date(from = record_dates[1], to = record_dates[2], by = "day")
  output_tab = data.frame(Date = dates, Flow = NA)
  for(i in 1:nrow(regime_tab)){
    this_month = which(regime_tab$Month[i] == month.abb)
    output_indices = month(output_tab$Date) == this_month
    output_tab$Flow[output_indices] = regime_tab$Flow_cfs[i]
  }
  return(output_tab)
}



# figure functions --------------------------------------------------------



landuse_figure_2016=function(){
  lu_dwr2016 = landuse_dwr_2016

  #Process land use data
  lu_processed = lu_dwr2016[watershed,]
  basin = basin[1,] # eliminate weird duplicate identical polygons

  # Read in categories for Scott 2016 land use
  lu_code_table = read.csv(file.path(ms_dir, "Data","Landuse_Description_2016.csv"))
  lu_processed$crop_descrip = lu_code_table$Landuse.Description[match(lu_processed$CropType1, lu_code_table$Landuse.Code)]
  lu_processed$crop_category = lu_code_table$Category[match(lu_processed$CropType1, lu_code_table$Landuse.Code)]
  lu_processed$crop_category_2 = lu_processed$crop_category
  #Aggregate to higher levels and clean up water name
  lu_processed$crop_category_2[lu_processed$crop_category %in% c("Deciduous","Vineyard",
                                                                 "Truck","Rice","Field")] = "Other Crops"
  lu_processed$crop_category_2[lu_processed$crop_category %in% c("Semi-Ag") |
                                 lu_processed$crop_descrip=="Urban Res- Single and Multiple"] = "Residential"
  lu_processed$crop_category_2[lu_processed$crop_category == "Native Barren"] = "Native Vegetation"
  lu_processed$crop_category_2[lu_processed$crop_category == "Native Water"] = "Water"

  #Make legend and add colors to spatial layer
  lu_dwr2016_labels = c("Pasture",        "Alfalfa",     "Grain",     "Other Crops", "Idle",       "Urban", "Residential", "Water", "Native Vegetation")
  lu_dwr2016_colors = c("darkolivegreen2", "forestgreen", "darkorange", "darkorchid1",     "dodgerblue", "red",   "pink",  "blue",   "lemonchiffon")
  lu_dwr2016_legend = data.frame(descrip = lu_dwr2016_labels, color = lu_dwr2016_colors)
  # Assign color to each polygon
  lu_processed$color = lu_dwr2016_legend$color[match(lu_processed$crop_category_2,lu_dwr2016_legend$descrip)]

  lu_crop=st_buffer(lu_processed,byid=T,dist=0) # clean up invalid polygons
  basin_buffer = st_buffer(basin, dist = 1000) # for map extent

  main_map = tm_shape(basin_buffer, is.master=T) + tm_borders(col = NA, lwd = 0) +
    tm_shape(lu_crop) + tm_polygons(col = "color", legend.show = F, border.col = NA ) +
    # tm_shape(hill_wsh) + tm_raster(palette = hillshade_palette_faded(10), legend.show = F, alpha = .3) +
    # tm_shape(hill_wsh) + tm_raster(palette = hillshade_palette_yellow(10), legend.show = F, alpha = .3) +
    tm_shape(adj_zone) + tm_borders(color_adj, lwd = 1.5) +
    tm_shape(riv) + tm_lines(color_river, lwd = 2) +
    tm_shape(cities_centroid) + tm_symbols(color_cities, size = 0.7) +
    tm_text("NAME", fontface = "bold", xmod = cities_centroid$label_xmod2, ymod = cities_centroid$label_ymod2) +
    tm_shape(fj_gauge) + tm_symbols(color_gauges, size = 1.2, shape=22) +
    tm_text("station_nm", fontface="bold", xmod = 3, ymod = .7) +

    # tm_shape(basin, name = "Groundwater Basin", is.master=T ) + tm_borders(color_basin, lwd = 2) +
    tm_add_legend(type="symbol", col = c(color_cities, color_gauges), border.lwd = c(1.5, 1),
                  size = 1, shape = c(21, 22), labels = c("Town or Place", "USGS FJ Stream Gauge")) +
    tm_add_legend( type = "line", lwd = 2, col = c(color_river, color_adj),
                   labels = c("Scott River","Adjudicated Zone")) +
    tm_add_legend(type = "fill", labels = as.character(lu_dwr2016_legend$descrip),
                  col = as.character(lu_dwr2016_legend$color), title = "Land Use Categories") +

    tm_compass(position = c("right", "top"), type = "4star", size = 1.8)+
    tm_scale_bar(position = c("left", "TOP"))+
    tm_layout(legend.position = c("LEFT", "BOTTOM"),
              legend.width = 1.5,
              legend.bg.color = "white", legend.frame = T)

  return(main_map)

}


precip_temp_flow_figure = function(plot_panel = NA){

  #read in long-term interp record or generate it
  precip_file_path = file.path(data_dir,"obs_and_interp_precip_1935_2021.csv")
  if(file.exists(precip_file_path)){ppt = read.csv(precip_file_path)}
  if(!file.exists(precip_file_path)){
    ppt = get_interp_precip_record()
    write.csv(ppt, file = precip_file_path, row.names = F, quote = F)
  }

  # visualize interp records
  # dates = as.Date(c("2004-01-01", "2005-01-01"))
  # plot(ppt$Date, ppt$fj_interp, type = "l", col = rgb(0,0,1,.3), xlim = dates)
  # lines(ppt$Date, ppt$cal_interp, col = rgb(1,0,0,.3))
  # lines(ppt$Date, ppt$gv_interp, col = rgb(0,1,0,.3))

  # Pick your fighter: which is the representative rainfall record?
  rain_column = "interp_cal_fj_mean"


  # 1. Aggregate precip data to annual and monthly values

  ## Missing day check
  # ppt_annual = aggregate(ppt[,rain_column], by = list(ppt$water_year), FUN = sum)
  # 0 stations with data during a chunk of data (16 days) in June 1937
  # (and 1 day in feb). assume 0 precip on those days.
  # Also missing Jan 1 of 2020. weird. Assume 0 precip that day. so na.rm=T is valid.

  ppt_annual = aggregate(ppt[,rain_column], by = list(ppt$water_year), FUN = sum, na.rm=T)
  colnames(ppt_annual) = c("water_year", "PRCP_mm")

  # Aggregate monthly: first sum by indiv. months, then summarize monthly totals
  ppt_month_all <- aggregate(ppt[,rain_column], by = list(ppt$month_day1), FUN = sum, na.rm=T)
  ppt_month_mean = aggregate(ppt_month_all$x, by = list(month(ppt_month_all$Group.1)),
                             FUN = mean)
  ppt_month_sd = aggregate(ppt_month_all$x, by = list(month(ppt_month_all$Group.1)),
                           FUN = sd)
  ppt_month = merge(ppt_month_mean, ppt_month_sd, by = "Group.1")
  colnames(ppt_month) = c("month", "PRCP_mm_mean", "PRCP_mm_sd")
  #Calculate "truncated st. dev." to prevent error bars from falling below 0:
  ppt_month$sd_trunc=ppt_month$PRCP_mm_sd
  high_sd_selector=ppt_month$sd>ppt_month$PRCP_mm_mean
  ppt_month$sd_trunc[high_sd_selector]=ppt_month$PRCP_mm_mean[high_sd_selector]
  # order for water year
  ppt_month = ppt_month[c(10:12,1:9),]

  # 2. Aggregate ET data  by month
  # et_allmonths = aggregate(x = et_0$et0_mm,
  #                          by = list(year(et_0$Date), month(et_0$Date)), FUN = sum)
  et_allmonths = aggregate(x = et_0$ET_ref_in*in_to_mm, by = list(year(et_0$Date), month(et_0$Date)), FUN = sum)
  et_monthly_mean = aggregate(et_allmonths$x, by = list(et_allmonths$Group.2), FUN = mean)

  et_monthly_sd = aggregate(et_allmonths$x, by = list(et_allmonths$Group.2), FUN = sd)
  et_monthly = merge(et_monthly_mean, et_monthly_sd, by = "Group.1")
  colnames(et_monthly)= c("Month", "ET_0_mm_mean","ET_0_mm_sd")

  # 3. Aggregate temp data
  #Process NOAA data - select for single station, aggregate to month
  station_id_number = "USC00043182" # fort jones ranger station
  noaa_stn = noaa[noaa$STATION == station_id_number,]
  noaa_stn$water_year <- wtr_yr(noaa_stn$DATE)

  # Aggregate by month in record, then average monthly over whole record
  noaa_stn$month <- month(noaa_stn$DATE)
  noaa_stn_month_tmax <- aggregate(TMAX ~ water_year + month , noaa_stn , mean)
  noaa_stn_month_tmax_sd <- aggregate(TMAX ~ water_year + month , noaa_stn , sd)
  noaa_stn_month_tmin <- aggregate(TMIN ~ water_year + month , noaa_stn , mean)
  noaa_stn_month_tmin_sd <- aggregate(TMIN ~ water_year + month , noaa_stn , sd)

  noaa_stn_month_tmax <- na.omit(noaa_stn_month_tmax)
  noaa_stn_month_tmin <- na.omit(noaa_stn_month_tmin)
  noaa_stn_month_tmax_sd <- na.omit(noaa_stn_month_tmax_sd)
  colnames(noaa_stn_month_tmax_sd) =c("water_year", "month", "TMAX_sd")
  noaa_stn_month_tmin_sd <- na.omit(noaa_stn_month_tmin_sd)
  colnames(noaa_stn_month_tmin_sd) =c("water_year", "month", "TMIN_sd")

  noaa_stn_month_temp = merge(noaa_stn_month_tmax, noaa_stn_month_tmin,
                              by=c("water_year", "month"))
  noaa_stn_month_temp = merge(noaa_stn_month_temp, noaa_stn_month_tmax_sd, by=c("water_year", "month"))
  noaa_stn_month_temp = merge(noaa_stn_month_temp, noaa_stn_month_tmin_sd, by=c("water_year", "month"))
  noaa_stn_month_temp_agg <- ddply(noaa_stn_month_temp, c("month"), summarise,
                                   TMAX=mean(TMAX),
                                   TMIN=mean(TMIN),
                                   TMAX_sd = mean(TMAX_sd),
                                   TMIN_sd = mean(TMIN_sd))
  # order for water year
  noaa_stn_month_temp_agg = noaa_stn_month_temp_agg[c(10:12,1:9),]

  # Process flow data
  fj_flow_jday = format(fj_flow$Date, "%j")
  fj_flow_median = aggregate(x = fj_flow$Flow, by = list(fj_flow_jday), FUN = median)
  fj_flow_05 = aggregate(x = fj_flow$Flow, by = list(fj_flow_jday), function(x){quantile(x,.05)})
  fj_flow_25 = aggregate(x = fj_flow$Flow, by = list(fj_flow_jday), function(x){quantile(x,.25)})
  fj_flow_75 = aggregate(x = fj_flow$Flow, by = list(fj_flow_jday), function(x){quantile(x,.75)})
  fj_flow_95 = aggregate(x = fj_flow$Flow, by = list(fj_flow_jday), function(x){quantile(x,.95)})

  fj_jday = data.frame(jday = fj_flow_median$Group.1, pct05 = fj_flow_05$x,
                       pct25 = fj_flow_25$x, pct50 = fj_flow_median$x,
                       pct75 = fj_flow_75$x, pct95 = fj_flow_95$x)

  # 4. PLOTS
  if(is.na(plot_panel)){par(mfrow = c(3,1))}

  # Panel 1: Monthly Temp
  if(is.na(plot_panel) | plot_panel==1){
    #Plot monthly average daily max and min temps
    # ylab_tempC = expression(atop(paste("Air Temperature ( ", degree,"C)")))
    ylab_tempC = "Air Temperature (degrees C)"
    # ylab_tempF = expression(atop(paste("Air Temperature ( ", degree,"F)")))
    par(mar = c(5,4,3,2))
    plot(x = noaa_stn_month_temp_agg$month, y = noaa_stn_month_temp_agg$TMAX,
         col = NA, ylim = c(-10,37), xaxt = "n",
         ylab = ylab_tempC,
         xlab = paste("NOAA Fort Jones Ranger weather station (USC00043182),",
                      min(noaa_stn$water_year), "to", max(noaa_stn$water_year)),
         main = "Scott Valley monthly average daily maximum and minimum temperatures")
    grid()
    pt_offset = 0.05
    axis(side=1, at=1:12, labels = month.abb[1:12])
    points(x = noaa_stn_month_temp_agg$month - pt_offset,
           y = noaa_stn_month_temp_agg$TMAX,
           pch = 19, col = "red")
    # Add TMAX error bars
    arrows(x0 = noaa_stn_month_temp_agg$month-pt_offset,
           x1 = noaa_stn_month_temp_agg$month-pt_offset,
           y0 = noaa_stn_month_temp_agg$TMAX + noaa_stn_month_temp_agg$TMAX_sd,
           y1 = noaa_stn_month_temp_agg$TMAX - noaa_stn_month_temp_agg$TMAX_sd,
           code = 3, length = .05, angle =  90, col = "red")
    #TMIN
    points(x = noaa_stn_month_temp_agg$month- pt_offset,
           y = noaa_stn_month_temp_agg$TMIN,
           pch = 19, col = "blue")
    # Add TMIN error bars
    arrows(x0 = noaa_stn_month_temp_agg$month-pt_offset,
           x1 = noaa_stn_month_temp_agg$month-pt_offset,
           y0 = noaa_stn_month_temp_agg$TMIN + noaa_stn_month_temp_agg$TMIN_sd,
           y1 = noaa_stn_month_temp_agg$TMIN - noaa_stn_month_temp_agg$TMIN_sd,
           code = 3, length = .05, angle =  90, col = "blue")

    text(x = 1, y = 32, labels = "A", cex = 2) # label plot
    legend(x="topright", pch = 19, col = c("red", "blue"), legend = c("Air Temp Max", "Air Temp Min"))
  }

  #Panel 2: Monthly precip + ET
  if(is.na(plot_panel) | plot_panel==2){
    pt_offset = 0.05
    par(mar = c(5,4,3,2))
    plot(x = (ppt_month$month - pt_offset), xaxt = "n",
         y = ppt_month$PRCP_mm_mean, pch = 19, col = precip_col,
         ylim = c(0, et_monthly$ET_0_mm_mean[7] + et_monthly$ET_0_mm_sd[7]),
         main = "Precipitation and reference ET, monthly mean and standard deviation",
         ylab = "Monthly precip. and ref. ET (mm)",
         xlab = paste0("Precipitation data from gap-filled NOAA records at Fort Jones and Callahan (USC00043182 \n and USC00041316), ",
                       min(ppt_annual$water_year), " to ", max(ppt_annual$water_year),
                       "; ET data from CIMIS Stn. 225, ", min(year(et_0$Date), na.rm=T),
                       " to ", max(year(et_0$Date),na.rm=T)))
    axis(side = 1, at = ppt_month$month, labels = month.abb[ppt_month$month])
    grid()
    # Add Precip error bars
    arrows(x0 = ppt_month$month-pt_offset, x1 = ppt_month$month-pt_offset,
           y0 = ppt_month$PRCP_mm_mean + ppt_month$PRCP_mm_sd,
           y1 = ppt_month$PRCP_mm_mean - ppt_month$sd_trunc,
           code = 3, length = .05, angle =  90, col = precip_col)
    # Add ET data
    points(x = et_monthly$Month + pt_offset, y = et_monthly$ET_0_mm_mean, pch = 19, col = et_col)
    arrows(x0 = et_monthly$Month+pt_offset, x1 = et_monthly$Month+pt_offset,
           y0 = et_monthly$ET_0_mm_mean + et_monthly$ET_0_mm_sd,
           y1 = et_monthly$ET_0_mm_mean - et_monthly$ET_0_mm_sd,
           code = 3, length = .05, angle =  90, col = et_col)
    legend(x = "topleft", pch = 19, col = c(precip_col, et_col), legend = c("Precip.", "ET ref."))
    text(x = 11.5, y = 200, labels = "B", cex = 2) # label plot
    # add new axis and ET data
  }

  # # Panel 3: Annual precip
  # plot(ppt_annual$water_year, ppt_annual$PRCP_mm, pch = 19, col = precip_col,
  #      ylim = c(0,1000),
  #      main = "Scott Valley annual precipitation \n Averaged gap-filled Fort Jones and Callahan station records",
  #      xlab = "Water Year", ylab = "Annual precipitation (mm)")
  # grid()
  # lines(x = ppt_annual$water_year,
  #       y = rollmean(ppt_annual$PRCP_mm, k = 10, na.pad=T, na.rm=T, align = "right"),
  #       lwd = 2, col = precip_col)
  # abline(h=mean(ppt_annual$PRCP_mm), col = "black", lwd = 2, lty = 2)
  #
  # legend(x = "bottomright", pch = c(19, NA,NA), lwd = c(NA, 2,2),
  #        col = c(precip_col, precip_col, "black"), lty = c(NA, 1, 2),
  #        horiz = T,
  #        legend = c("Annual Rainfall", "10-year trailing average",
  #                   paste0("Long-term average (", round(mean(ppt_annual$PRCP_mm),1), " mm)")))
  # text(x = 1935, y = 950, labels = "C", cex = 2)

  # Panel 3: seasonal flow
  if(is.na(plot_panel) | plot_panel==3){
    par(mar = c(4,5,3,5) + 0.1) # add more space for 2nd axis
    fj_jday$jday = as.numeric(fj_jday$jday)

    plot(x = fj_jday$jday, y = rep(NA, nrow(fj_jday)),
         ylim = c(1, max(fj_jday$pct95)), xaxt = "n",
         ylab = expression(Flow~at~FJ~Gauge~(ft^3~"/"~sec)), # ylab = "FJ Flow (cfs)",
         main = "Scott River Daily Flow Summary",
         xlab = paste("Fort Jones Stream Gauge, USGS Station ID #11519500,",
                      paste(range(year(fj_flow$Date)), collapse = " to ")))
    grid(col = "gray60", lwd = 1.5, lty = 2)
    month_day1 = format(seq.Date(from = as.Date("2000-01-01"), to = as.Date("2000-12-31"), by = "month"), "%j")
    axis(side = 1, at = month_day1, labels = month.abb[1:12])
    polygon(x = c(fj_jday$jday, rev(fj_jday$jday)), y = c(fj_jday$pct05, rev(fj_jday$pct95)),
            col = "gray80", border = NA)
    polygon(x = c(fj_jday$jday, rev(fj_jday$jday)), y = c(fj_jday$pct25, rev(fj_jday$pct75)),
            col = "gray40", border = NA)
    lines(x = fj_jday$jday, y = fj_jday$pct50, lwd = 2)
    # label panel
    text(x = 10, y = 4700, labels = "C", cex = 2)
    # Add 2nd cms axis
    par(new=TRUE)
    # flow_labels = c("0.1","1","10","100","1,000","10,000","100,000")
    plot(x=c(1,1), y = c(1, max(fj_jday$pct95)) * cfs_to_m3sec,
         ylim = c(1, max(fj_jday$pct95)) * cfs_to_m3sec, col = NA,
         xaxt = "n", yaxt = "n", ylab = "", xlab = "") #log = "y",
    axis(side=4) #, at=10^(-1:5) )#, labels = flow_labels, las = 1)
    # axis(side=4, at=rep(1:9,7) * 10 ^ (rep(-1:5, each = 9)), labels = NA, tck = -0.01)
    mtext(expression(Flow~at~FJ~Gauge~(m^3~"/"~sec)), side = 4, line = 3, cex = 0.7)


    legend(x="top", col = c("gray80", "gray40", "black"), lwd = c(NA,NA, 2), bg="white",
           pch = c(15, 15, NA),pt.cex = c(2,2,NA), legend = c("90% variability", "50% variability", "Median flow"))
  }
}


avg_swbm_budget_fig=function(scen_id = "basecase"){

  swbm = get_swbm_tab_with_separated_et(scen_id = scen_id)

  # Clean colnames
  colnames(swbm)[colnames(swbm) == "SW.Irrigation"] = "SW Irrigation"
  colnames(swbm)[colnames(swbm) == "GW.Irrigation"] = "GW Irrigation"
  budget_comps = c("Precipitation", "Recharge",
                   "SW Irrigation", "GW Irrigation",
                   "Crop ET", "Nat. Veg. ET", "Storage")
  swbm = swbm[,c("Month","Water_Year", budget_comps)] # reorder for plotting order in legend
  # Melt data for aggregation and plotting
  swbm_melt = reshape2::melt(swbm, id.vars = c("Month", "Water_Year") ,value_name = "monthly_m3")
  # Make color scale for budget components
  swbm_melt$variable = factor(swbm_melt$variable, levels = budget_comps)
  comps = data.frame(component = budget_comps,
                     col= c(precip_col, rch_col,
                            sw_irr_col, gw_irr_col,
                            et_crop_col, et_natveg_col,
                            stor_col))
  # aggregate by month
  swbm_monthly_m3 = aggregate(x = swbm_melt$value,
                              by = list(month(swbm_melt$Month), swbm_melt$variable),
                              FUN = mean)
  # convert to million cubic meters
  swbm_monthly = swbm_monthly_m3
  swbm_monthly$x = swbm_monthly_m3$x / 10^6
  # Clean colnames for plot
  colnames(swbm_monthly) = c("Month", "Component", "Avg. Monthly Volume, million cubic m")
  # Convert month to factor to display starting in October
  swbm_monthly$Month = factor(x = swbm_monthly$Month, levels = c(10:12, 1:9))
  # Plot
  ggplot(data = swbm_monthly, aes(fill = Component, x = Month,
                                  y = `Avg. Monthly Volume, million cubic m`)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_fill_manual(labels = comps$component, values = comps$col) +
    scale_x_discrete(name = "Water Years 1991-2018", breaks = c(10:12,1:9), labels = month.abb[c(10:12, 1:9)]) +
    theme_bw() +
    theme(legend.position = "bottom")

}

annual_swbm_budget_fig = function(scen_id = "basecase", 
                                  start_date = as.Date("1990-10-01")){
  swbm_monthly = get_swbm_tab_with_separated_et(scen_id = scen_id)
  swbm_monthly$Month=NULL

  # # check sums
  # swbm_monthly$et_crop_plus_nv = swbm_monthly$et_crop + swbm_monthly$et_natveg
  # swbm_monthly$diff = swbm_monthly$ET + swbm_monthly$et_crop_plus_nv

  # aggregate to annual
  swbm_annual = aggregate(.~Water_Year, swbm_monthly, function(x){sum(x) / 1E6})


  # plot
  par(mar = c(5,4,2,2)+.1)
  plot(x = swbm_annual$Water_Year, y = rep(NA, nrow(swbm_annual)),
       col = et_col, pch=20, type = "o", cex = 2, lwd = 2, ylim = c(-230, 190),
       xlab = "Water Year", ylab = "Annual volume (million cubic m)",
       # main = "Simulated Scott Valley water budget values (soil zone)"
       )
  grid()
  abline(h=0)
  swbm_comp = data.frame( comp = c("Precipitation","Recharge",
                                   "Crop ET", "Nat. Veg. ET",
                                    "Irrigation"),#,"Storage"),
                          color = c(precip_col, rch_col,
                                    et_crop_col, et_natveg_col,
                                     gw_irr_col))#, stor_col))
  for(i in 1:nrow(swbm_comp)){
    comp = swbm_comp$comp[i]
    if(comp == "Irrigation"){
      points(x = swbm_annual$Water_Year, y=swbm_annual$SW.Irrigation+swbm_annual$GW.Irrigation,
             col = swbm_comp$color[i], pch = 19, type = "o", lwd = 2)
    } else {
      points(x = swbm_annual$Water_Year, y=swbm_annual[, comp],
             col = swbm_comp$color[i], pch = 19, type = "o", lwd = 2)
    }
  }

  legend(x = "bottomleft", pch = 19, lwd = 2, ncol=3, bg="white", cex = .8,
         col = swbm_comp$color, legend=swbm_comp$comp)
}

plot_obj_fn_by_wy = function(plot_scenarios, cols = NA, panel_labels = c("A","B"),
                             dry_years = c(1991, 1992, 1994, 2001,
                                           2009, 2013, 2014, 2018)) {
  n_scen = length(plot_scenarios)
  if(sum(is.na(cols))>1){cols = my_colors = colorblind_pal()(length(plot_scenarios))}
  cols[1] = basecase_col
  pchs = c(20,3:6,2)#21:25 #supports max 5 comparisons
  wys = min(obj_fn_wy$wy) : max(obj_fn_wy$wy)
  # plot value for dry year color
  dry_yr_col = rgb(.5,.4,.4,.2)
  rec_wid = .45

  # panel 1, hbv
  par(mar = c(3,4,1,2))
  y_range_hb = range(obj_fn_wy$hb_val, na.rm=T)
  dy = diff(y_range_hb)
  plot(x = NA, y = NA,
       ylim = y_range_hb,
       xlim = range(wys),
       # main = scen,
       ylab = "HB value (coho spf-equiv.)", xlab = "")
  grid()

  # add dry year highlights
  for(dry_yr in dry_years){
    xes = c(dry_yr + rec_wid, dry_yr - rec_wid,
            dry_yr - rec_wid, dry_yr + rec_wid, dry_yr + rec_wid)
    ys = c(y_range_hb[1]+dy, y_range_hb[1]+dy,
           y_range_hb[2]-dy, y_range_hb[2]-dy,
           y_range_hb[1]+dy) 
    polygon(x = xes, y = ys, col = dry_yr_col, border = NA)
  }
  # add scenario data
  for(i in 1:n_scen){
    scen = plot_scenarios[i]
    scen_selector = obj_fn_wy$scenario==scen
    points(x = obj_fn_wy$wy[scen_selector], y = obj_fn_wy$hb_val[scen_selector],
           col = cols[i], pch = pchs[i], cex = 1.8, lwd = 2)
    lines(x = obj_fn_wy$wy[scen_selector], y = obj_fn_wy$hb_val[scen_selector],
          col = cols[i])
  }
  text(x = 1991, y = 10, labels = panel_labels[1]) # Panel label


  # panel 2, et
  par(mar = c(3,4,1,2))
  y_range_et = range(obj_fn_wy$et_tot / 10^6 * -1)
  plot(x = NA, y = NA,
       xlim = range(wys), ylim = y_range_et,
       ylab = "Crop ET (million m 3)", xlab = "")
  grid()
  # add dry year highlights
  for(dry_yr in dry_years){
    xes = c(dry_yr + rec_wid, dry_yr - rec_wid,
            dry_yr - rec_wid, dry_yr + rec_wid, dry_yr + rec_wid)
    ys = c(y_range_et[1]-10, y_range_et[1]-10,
           y_range_et[2], y_range_et[2],
           y_range_et[1]-10) * 1.5
    polygon(x = xes, y = ys, col = dry_yr_col, border = NA)
  }
  # add scenario data
  for(i in 1:n_scen){
    scen = plot_scenarios[i]
    scen_selector = obj_fn_wy$scenario==scen
    points(x = obj_fn_wy$wy[scen_selector], y = obj_fn_wy$et_tot[scen_selector] / 10^6 *-1,
           col = cols[i], pch = pchs[i], cex = 1.8, lwd = 2)
    lines(x = obj_fn_wy$wy[scen_selector], y = obj_fn_wy$et_tot[scen_selector] / 10^6 *-1,
          col = cols[i], pch = pchs[i])

  }
  legend(x = "bottomright", pch = pchs, lwd = rep(1,n_scen), pt.lwd = 2,
         col = cols, cex = 1.2, ncol=2,
         legend = plot_scenarios, bg = "white")
  # legend(x = "bottomleft", pch = 15, pt.cex = 2,
  #        col = dry_yr_col, legend = "Dry-type water years", bg = "white")
  text(x = 1991, y = 30, labels = panel_labels[2])

  # alternate arrangement, maybe for an appendix
  # # # plot_scenarios = scenario_tab$scenario_id
  # plot_scenarios = c("basecase", "alf_irr_stop_aug_01", "alf_irr_stop_aug_01_dry_yrs_only")
  # cols = my_colors = colorblind_pal()(length(plot_scenarios))
  # wys = min(obj_fn_wy$wy) : max(obj_fn_wy$wy)
  #
  # bc_selector = obj_fn_wy$scenario=="basecase"
  #
  #   par(mfrow = c(2,1))
  #   # panel 1, hbv
  #   plot(x = NA, y = NA,
  #        ylim = range(obj_fn_wy$hb_val, na.rm=T),
  #        main = scen, ylab = "HB value (coho spf-equiv.)", xlab = "Water Year")
  #   grid()
  # for(i in 1:length(plot_scenarios)){
  #   scen = plot_scenarios[i]
  #   scen_selector = obj_fn_wy$scenario==scen
  #   points(x = obj_fn_wy$wy[scen_selector], y = obj_fn_wy$hb_val[scen_selector],
  #        type = "o",col = hbv_col, pch = 19)
  # }
  #   # panel 2, et
  #   plot(x = obj_fn_wy$wy[bc_selector], y = obj_fn_wy$et_tot[bc_selector] / 10^6 *-1,
  #        type = "o", col = basecase_col, pch = 18,
  #        ylim = range(obj_fn_wy$et_tot / 10^6 * -1),
  #        ylab = "Crop ET (million m 3)", xlab = "Water Year")
  #   grid()
  #   points(x = obj_fn_wy$wy[scen_selector], y = obj_fn_wy$et_tot[scen_selector] / 10^6 *-1,
  #        type = "o", col = et_crop_col, pch = 19)
  #   legend(x = "bottomright", pch = c(18, 19, 19), lwd = c(1,1,1),
  #          col = c(basecase_col, hbv_col, et_crop_col), ncol = 3,
  #          legend = c("Basecase", "Scen. HBV", "Scen. Crop ET"))
  # }
}



farm_fish_tradeoff_blank = function(scenario_tab, add_st_dev_bars = F){
  par(mar = c(5,4,5,2))
  plot(scenario_tab$HBF_mean, scenario_tab$et_mean / 1E6 * -1,
       pch = 19, cex = 2, #ylim = c(-0.20,0.02), #xlim = c(8.4,13),
       col =  NA,
       xlab = "Mean Hydrologic Benefit Value (coho spf-equiv.)",
       ylab = "Mean Annual Crop ET (million cubic m)",
       main = "Environmental vs Agricultural Benefit of Suite of Management Actions \n Mean and Standard Error for annual values, 1991-2018")
  grid()
}


farm_fish_tradeoff_fig = function(scenario_tab, n_years = 35,
                                  add_st_dev_bars = F,
                                  legend_position = "bottomleft",#"topright",
                                  plot_title){
  par(mar = c(5,4,5,2))
  plot(scenario_tab$HBF_mean, scenario_tab$et_mean / 1E6 * -1, # convert to + number in mill. m^3
       pch = scenario_tab$symbol,# pch = 21,
       cex = 2, #ylim = c(-0.20,0.02), #xlim = c(8.4,13),
       bg = scenario_tab$color,
       ylim = c(0,125),# xlim = c(3.5, 4.5), # ylim = c(0,113), xlim = c(55, 67),
       xlab = "Hydrologic Benefit Function (coho spf-equiv)",
       ylab = "Mean Annual Crop ET (million cubic m)",
       # main = "Environmental vs Agricultural Benefit of Suite of Management Actions \n Mean and Standard Error for annual values"
       main = plot_title)
  grid()
  # add st dev bars
  if(add_st_dev_bars == T){
    for(i in 1:nrow(scenario_tab)){
      # print(i)
      x_mean = scenario_tab$HBF_mean[i]
      x_se = scenario_tab$HBF_stdev[i] / n_years #x_mean
      y_mean = scenario_tab$et_mean[i] / 1E6 * -1
      y_se = scenario_tab$et_stdev[i] / 1E6 * -1 / n_years #y_mean
      if(x_se>0){
        # horiz. error bar
        arrows(x0 = x_mean - x_se, x1 = x_mean + x_se,
               y0 = y_mean, y1 = y_mean,
               # col = scenario_tab$color[i],
               angle = 90, code =3, length = 0.05)#,lwd = 2)
      }
      if(abs(y_se)>0){
        #vertical error bar
        arrows(x0 = x_mean, x1 = x_mean,
               y0 = y_mean - y_se, y1 = y_mean + y_se,
               # col = scenario_tab$color[i],
               angle = 90, code =3, length = 0.05)#,lwd = 2)
      }
    }
  }

  # Add points on top to make the error bars look less messy
  points(scenario_tab$HBF_mean, scenario_tab$et_mean/ 1E6 * -1,
         pch = scenario_tab$symbol, cex = 2, bg = scenario_tab$color)
  # overplot Basecase so it is visible
  bc_tab = scenario_tab[scenario_tab$scenario_id=="basecase",]
  points(bc_tab$HBF_mean, bc_tab$et_mean / 1E6 * -1, # convert to + number in mill. m^3
         pch = bc_tab$symbol, cex = 2)
  # add legend
  legend(x = legend_position, pch=scen_cat_tab$symbol, cex = .8, pt.cex = 1.5,
         title = "Mgmt. Action Category",# (# of scenarios)",
         pt.bg = scen_cat_tab$color,
         legend = scen_cat_tab$category_long)
}


tradeoff_efficiency_fig = function(scenario_tab){
  low_et_cost = scenario_tab[scenario_tab$scenario_category %in% c("IrrEff", "EnhRch", "EnhRchEx", "Res", "basecase"),]
  plot_2_scen = scenario_tab[scenario_tab$et_lost>0 ,]

  # for the legend
  scen_cat_tab_low = scen_cat_tab[scen_cat_tab$category %in% c("IrrEff", "EnhRch", "EnhRchEx", "Res", "basecase"),]
  scen_cat_tab_other = scen_cat_tab[!(scen_cat_tab$category %in% c("IrrEff", "EnhRch", "EnhRchEx", "Res","basecase")),]

  # par(mar=c(5,4,4,1))
  lim_vals = range(c(plot_2_scen$hb_gained_to_et_lost,
                     plot_2_scen$hb_gained_to_et_lost_lc), na.rm=T)
  plot(#x = (other_scen$hb_gained_to_et_lost),
    #y = (other_scen$hb_gained_to_et_lost_dry),
    x = (plot_2_scen$hb_gained_to_et_lost),
    y =(plot_2_scen$hb_gained_to_et_lost_lc),
    log="xy", xlim = lim_vals, ylim = lim_vals, xaxt = "n", yaxt = "n",
    main = "Trade-off efficiency: relative HB gained per \n relative ET lost, in all vs. low-coho years",
    xlab = "Avg. efficiency (1991-2018)", ylab = "Avg. efficiency (low-coho years only)",
    bg = plot_2_scen$color, pch = plot_2_scen$symbol, cex = 1.5)
  grid(col = "gray40")
  abline(0,1, lty = 2)
  text(x = .8, y = .5, label = "1:1 line")
  axis(side = 1, at = 10^(-2:2), labels = 10^(-2:2))
  axis(side = 1, at = rep(1:9,4)*sort(rep(10^(-2:2),9)), labels = NA, tck = -0.01)
  axis(side = 2, at = 10^(-2:2), labels = 10^(-2:2))
  axis(side = 2, at = rep(1:9,4)*sort(rep(10^(-2:2),9)), labels = NA, tck = -0.01)

  # legend(x = "topleft", pch=scen_cat_tab$symbol[1:6], cex = .8, pt.cex = 1.5, #ncol = 2,
  #        pt.bg = scen_cat_tab$color[1:6],
  #        legend = scen_cat_tab$category_long[1:6])
  # legend(x = "bottomright", pch=scen_cat_tab$symbol[7:11], cex = .8, pt.cex = 1.5, #ncol = 2,
  #        pt.bg = scen_cat_tab$color[7:11],
  #        legend = scen_cat_tab$category_long[7:11])

  sct2 = scen_cat_tab[!(scen_cat_tab$category %in% c("basecase", "Res")),]

  # legend(x = "bottomright", pch=sct2$symbol, cex = .8, pt.cex = 1.5, #ncol = 2,
  #        pt.bg = sct2$color,
  #        legend = sct2$category_long)
}

# Table functions ---------------------------------------------------------

landuse_table_2016 = function(){
  lu_dwr2016 = landuse_dwr_2016

  #Process land use data
  lu_processed = lu_dwr2016[watershed,]
  basin = basin[1,] # eliminate weird duplicate identical polygons

  # Read in categories for Scott 2016 land use
  lu_code_table = read.csv(file.path(ms_dir, "Data", "Landuse_Description_2016.csv"))
  lu_processed$crop_descrip = lu_code_table$Landuse.Description[match(lu_processed$CropType1, lu_code_table$Landuse.Code)]
  lu_processed$crop_category = lu_code_table$Category[match(lu_processed$CropType1, lu_code_table$Landuse.Code)]
  lu_processed$crop_category_2 = lu_processed$crop_category
  #Aggregate to higher levels and clean up water name
  lu_processed$crop_category_2[lu_processed$crop_category %in% c("Deciduous","Vineyard",
                                                                 "Truck","Rice","Field")] = "Other Crops"
  lu_processed$crop_category_2[lu_processed$crop_category %in% c("Semi-Ag") |
                                 lu_processed$crop_descrip=="Urban Res- Single and Multiple"] = "Residential"
  lu_processed$crop_category_2[lu_processed$crop_category == "Native Barren"] = "Native Vegetation"
  lu_processed$crop_category_2[lu_processed$crop_category == "Native Water"] = "Water"

  #Make legend and add colors to spatial layer
  lu_dwr2016_labels = c("Pasture",        "Alfalfa",     "Grain",     "Other Crops", "Idle",       "Urban", "Residential", "Water", "Native Vegetation")
  lu_dwr2016_colors = c("darkolivegreen2", "forestgreen", "darkorange", "darkorchid1",     "dodgerblue", "red",   "pink",  "blue",   "ghostwhite")
  lu_dwr2016_legend = data.frame(descrip = lu_dwr2016_labels, color = lu_dwr2016_colors)

  lu_processed$color = lu_dwr2016_legend$color[match(lu_processed$crop_category_2,lu_dwr2016_legend$descrip)]

  lu_crop=gBuffer(lu_processed,byid=T,width=0) # clean up invalid polygons
  lu_crop = raster::intersect(x = lu_crop, y = watershed)

  # make table
  m3_area = aggregate(area(lu_crop), by = list(lu_crop$crop_category_2), FUN = sum)
  # m3_area = aggregate(area(lu_processed), by = list(lu_processed$crop_category_2), FUN = sum)


  # Make new acreage table
  acreage = m3_area
  # convert to acres
  acreage$x = round(m3_area$x/4046.86)
  acreage = acreage[order(m3_area$x, decreasing = T),]
  acreage$percent = round(acreage$x/sum(acreage$x) * 100, digits = 1)
  colnames(acreage) = c("Land Cover", "Acres", "% of Basin")

  return(acreage)
}



get_scen_cat_tab = function(){
  scen_cat_tab = data.frame(category = c("Basecase","EnhRch", 
                                         "IrrEff", "Res", "CropCh",
                                         "AlfIrr", #"Curtail",
                                         "FlowLims",
                                         "NatVegH", "NatVegL"),
                            category_long = c("Basecase", "Enhanced Recharge",
                                              "Improved Irrigation Efficiency",
                                              "Small Reservoir",
                                              "Crop Change (Alfalfa Rot. to Perm. Grain)",
                                              "Cease Alfalfa Irrigation Early",
                                              # "Cease (Curtail) All Irrigation Early",
                                              "Low Flow Diversion Limits",
                                              "Some Nat. Veg. Land Use (high ET)",
                                              "Some Nat. Veg. Land Use (low ET)"),
                            num_scenario = c(1,1,
                                             2,3,3,
                                             3,1,
                                             4,4),
                            feas_cat = c(0, 3, 
                                         2, 4, 2,
                                         1, 1, #1,
                                         2, 2),
                            mgmt_cat = c("Basecase",
                                         rep("Infrastructure", 4),
                                         rep("Regulatory", 2),
                                         rep("Nat. Veg.", 2)))
  return(scen_cat_tab)
}

get_scenario_tab_init = function(){

  # Build scenario table
  scen_ids_for_tab = c("basecase",
                       "alf_curtail_07.15", "alf_curtail_07.31","alf_curtail_08.15",
                       "basecase_0_curtail","basecase_0_mar_0_curt", "basecase_0_mar", 
                       "eflows25_div_lims",
                       "grain_12k", "grain_14k", "grain_6k",
                       "irr_eff_0.1",  "irr_eff_0.2", #"irr_eff_minus_0.1",
                       "maxMAR_fields2024", 
                       "natveg_high_12k", "natveg_high_4k", "natveg_high_8k", "natveg_high_all",
                       "natveg_low_12k", "natveg_low_4k", "natveg_low_8k", "natveg_low_all",
                       "reservoir_etna", "reservoir_french", "reservoir_shackleford"
  )
  
  scen_id_cat = c("basecase",
                  "AlfIrr","AlfIrr","AlfIrr",
                  "Attr", "Attr", "Attr",
                  "FlowLims",
                  "CropCh", "CropCh", "CropCh",
                  "IrrEff","IrrEff", #"IrrEff",
                  "EnhRch",
                  "NatVegH","NatVegH","NatVegH","NatVegH",
                  "NatVegL","NatVegL","NatVegL","NatVegL",
                  "Res","Res","Res"
                  # "Curtail","Curtail","Curtail","Curtail","Curtail","Curtail",
  )

  scenario_tab_init = data.frame(scenario_id = scen_ids_for_tab,
                                 scenario_category = scen_id_cat,
                                 HBF_mean = 0.0,
                                 HBF_stdev = 0.0,
                                 et_mean = 0.0,
                                 et_stdev = 0.0)

  return(scenario_tab_init)
  
  
  # old names and order
  # Build scenario table
  scen_ids_for_tab = c("basecase",
                       "mar","ilr","mar_ilr",
                       "mar_ilr_max_0.003", "mar_ilr_max_0.019", "mar_ilr_max_0.035",
                       "irr_eff_improve_0.1","irr_eff_improve_0.2", #"irr_eff_worse_0.1",
                       "reservoir_shackleford", "reservoir_etna",
                       "reservoir_french", "reservoir_sfork", "reservoir_etna_29kAF",
                       "irrig_0.8","irrig_0.9",
                       "alf_irr_stop_jul10",
                       "alf_irr_stop_aug01", "alf_irr_stop_aug01_dry_yrs_only",
                       "alf_irr_stop_aug15", "alf_irr_stop_aug15_dry_yrs_only",
                       "curtail_start_jun01","curtail_start_jun15",
                       "curtail_start_jul01","curtail_start_jul15",
                       "curtail_start_aug01","curtail_start_aug15",
                       "flowlims", #"mar_ilr_flowlims",
                       "natveg_all", "natveg_gwmixed_all", # dzgwET Only are now in these folders
                       "natveg_inside_adj","natveg_gwmixed_inside_adj",
                       "natveg_outside_adj","natveg_gwmixed_outside_adj",
                       # "natveg_all_dzgwET_only", "natveg_gwmixed_all_dzgwET_only",
                       # "natveg_inside_adj_dzgwET_only","natveg_gwmixed_inside_adj_dzgwET_only",
                       # "natveg_outside_adj_dzgwET_only","natveg_gwmixed_outside_adj_dzgwET_only",
                       # "natveg_all_et_check_0.6nvkc_4.5m_ext",
                       # "natveg_all_et_check_0.6nvkc_10m_ext",
                       # "natveg_all_et_check_1.0nvkc_10m_ext",
                       # exclude these ^ natveg-all scens. b/c no crop ET, so meaningless in revised crop ET farmer benefit fn
                       "natveg_all_et_check_1.0nvkc_4.5m_ext",
                       "natveg_gwmixed_all_et_check_1.0nvkc_4.5m_ext",
                       "natveg_inside_adj_et_check_1.0nvkc_4.5m_ext",
                       "natveg_gwmixed_inside_adj_et_check_1.0nvkc_4.5m_ext",
                       "natveg_outside_adj_et_check_1.0nvkc_4.5m_ext",
                       "natveg_gwmixed_outside_adj_et_check_1.0nvkc_4.5m_ext"
  )
  
  scen_id_cat = c("basecase",
                  "EnhRch","EnhRch","EnhRch","EnhRchEx","EnhRchEx","EnhRchEx",
                  "IrrEff","IrrEff", #"IrrEff",
                  "Res","Res","Res","Res", "Res",
                  "CropCh", "CropCh",
                  "AlfIrr","AlfIrr","AlfIrr","AlfIrr","AlfIrr",
                  "Curtail","Curtail","Curtail","Curtail","Curtail","Curtail",
                  "FlowLims", #"FlowLims",
                  "NatVeg","NatVeg","NatVeg","NatVeg","NatVeg","NatVeg",
                  # "NatVegAllET","NatVegAllET","NatVegAllET",
                  "NatVegET","NatVegET","NatVegET","NatVegET","NatVegET","NatVegET"
  )
}

re_and_disconnect_date_tab=function(thresholds = c(10,20,30,40,60,100), 
                                    fj_flow, last_wy = 2025){
  
  
  calc_discon_days_since_aug_31 = function(dates, flow, discon_threshold){
    dates_since_aug_31 = as.numeric(dates - as.Date(paste0(year(min(dates))-1, "-08-31")))
    if(sum(flow < discon_threshold) > 0){ #if it does disconnect, calculate discon day
      discon_day = min(dates_since_aug_31[ flow < discon_threshold], na.rm=T)
    }
    
    if(sum(flow < discon_threshold) < 1){ # if it does not disconnect, return the end of the analysis period
      return(max(dates_since_aug_31))
    } else {
      return(discon_day)
    }
  }
  
  calc_recon_days_since_aug_31 = function(dates, flow, recon_threshold){
    dates_since_aug_31 = as.numeric(dates - as.Date(paste0(year(min(dates)), "-08-31")))
    recon_day = min(dates_since_aug_31[ flow > recon_threshold], na.rm=T)
    return(recon_day)
  }
  
  if(!("wy" %in% colnames(fj_flow))){fj_flow$wy = wtr_yr(fj_flow$Date)}
  wat_years = unique(fj_flow$wy)
  wat_years = wat_years[wat_years<=last_wy]
  # output_tab = data.frame(year = wat_years, min_flow = NA,
  #                         recon_date_10 = NA, recon_date_20 = NA,
  #                         recon_date_30 = NA, recon_date_40 = NA,
  #                         recon_date_60 = NA, recon_date_100 = NA,
  #                         discon_date_10 = NA, discon_date_20 = NA,
  #                         discon_date_30 = NA, discon_date_40 = NA,
  #                         discon_date_60 = NA, discon_date_100 = NA)
  output_tab = data.frame(matrix(data = NA, nrow = length(wat_years),
                                 ncol = length(thresholds) * 2 + 3))
  colnames(output_tab) = c('water_year', "min_flow", "tot_flow",paste0("recon_date_",thresholds),
                           paste0("discon_date_",thresholds))
  output_tab$water_year = wat_years
  
  
  for(i in 1:length(wat_years)){
    year = wat_years[i]
    
    date1_discon = as.Date(paste0(year,"-03-01"))
    date2_discon = as.Date(paste0(year,"-08-31"))
    dates_discon = seq.Date(from=date1_discon, to = date2_discon, by="day")
    
    date1_recon = as.Date(paste0(year-1,"-09-01"))
    date2_recon = as.Date(paste0(year,"-03-15")) # extend past Mar. 1 for wy 2001
    dates_recon = seq.Date(from=date1_recon, to = date2_recon, by="day")
    
    discon_flows = fj_flow$Flow[fj_flow$Date %in% dates_discon]
    recon_flows = fj_flow$Flow[fj_flow$Date %in% dates_recon]
    
    output_tab$min_flow[i] = min(fj_flow$Flow[fj_flow$wy==year])
    output_tab$tot_flow[i] = sum(fj_flow$Flow[fj_flow$wy==year] * cfs_to_m3day) / (10^6) #million cubic m
    
    
    for(thresh in thresholds){
      output_tab[i, paste0("discon_date_", thresh)] = calc_discon_days_since_aug_31(dates=dates_discon,
                                                                                    flow = discon_flows,
                                                                                    discon_threshold = thresh)
      output_tab[i, paste0("recon_date_", thresh)] = calc_recon_days_since_aug_31(dates=dates_recon,
                                                                                  flow = recon_flows,
                                                                                  recon_threshold = thresh)
    }
    #
    # output_tab$recon_date_10[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 10)
    # output_tab$discon_date_10[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 10)
    #
    # output_tab$recon_date_20[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 20)
    # output_tab$discon_date_20[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 20)
    #
    # output_tab$recon_date_30[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 30)
    # output_tab$discon_date_30[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 30)
    #
    # output_tab$recon_date_40[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 40)
    # output_tab$discon_date_40[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 40)
    #
    # output_tab$recon_date_60[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 60)
    # output_tab$discon_date_60[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 60)
    #
    # output_tab$recon_date_100[i] = calc_recon_days_since_aug_31(dates=yr_dates, flow = recon_flows, recon_threshold = 100)
    # output_tab$discon_date_100[i] = calc_discon_days_since_aug_31(dates=yr_dates, flow = discon_flows, discon_threshold = 100)
    
    
  }
  
  return(output_tab)
  
}


add_func_flows_to_hbf_tab=function(pre_hbf_tab, ff_ids, 
                                   ff_aligned_scen, scen_id){

  # csv_name = paste0(scen_id, " func flow metrics.csv")
  # ff_dir = file.path(data_dir, "SVIHM Model Results", "tables for func flows")
  # ff_scen = read.csv(file.path(ff_dir, csv_name))
  # colnames(ff_aligned)[colnames(ff_scen)=="Year"] = "func_flow"
  # ff_desc = ff_scen$func_flow
  # ff_scen$func_flow= NULL; row.names(ff_scen) = ff_desc
  
  
  #initialize hbf_tab
  # hbf_tab = data.frame("brood_year" = pre_hbf_tab[,"water_year"])
  hbf_tab = data.frame("water_year" = pre_hbf_tab[,"water_year"])
  # add brood year column to hbf_tab
  hbf_tab[,ff_ids]=NA
  for(i in 1:length(ff_ids)){
    ff_id = ff_ids[i]
    
    #for recon and discon dates
    if(grepl(pattern = "recon",x=ff_id) | grepl(pattern = "discon",x=ff_id)){
      # thresh = unlist(strsplit(ff_id, "_"))[3]
      # which_con = unlist(strsplit(ff_id, "_"))[2]
      # # find which column has the correct recon or discon dates
      # matching_column_index = which(grepl(pattern = which_con, x = colnames(pre_hbf_tab)) & 
      #                                 grepl(pattern = thresh, x = colnames(pre_hbf_tab)))
      # ff_vals = pre_hbf_tab[(1+BY_correction):(nrow(hbf_tab)), 
      #                       matching_column_index]
      # ff_vals = c(ff_vals, rep(NA, BY_correction))

      hbf_tab[,ff_id] = pre_hbf_tab[,ff_id]
    }
      
    
    
    if(!(grepl(pattern = "recon",x=ff_id) | 
         grepl(pattern = "discon",x=ff_id))){
      
      hbf_tab[,ff_id] = ff_aligned_scen[,ff_id]

    }
  }
  
  hbf_tab[!is.finite(as.matrix(hbf_tab))]=NA # clean up infinites
  
  return(hbf_tab)
}

calc_hbf_tab_nov2024 = function(thresholds_hbf, last_wy = 2018,
                                flow_tab_for_hbf, weights,
                                scen_id = "hist_obs"){
  
  pre_hbf_tab = re_and_disconnect_date_tab(thresholds = thresholds_hbf,
                                       fj_flow = flow_tab_for_hbf)
  # fill in missing values with averages
  ffs = colnames(weights)[colnames(weights) !="Intercept"] 
  
  hbf_tab = add_func_flows_to_hbf_tab(pre_hbf_tab = pre_hbf_tab, 
                                      ffs = ffs,
                                      scen_id = scen_id)
  hbf_tab = hbf_tab[hbf_tab$water_year <= last_wy,]

  write.csv(x = hbf_tab, quote = F, row.names = T,
            file = file.path(ms_dir, "Graphics and Supplements",
                             "Supplemental Table 2 - Flow Metrics by Water Year, 1942-2021.csv"))
  
  # fill gaps with averages
  col_avgs = apply(X=hbf_tab, MARGIN = 2, FUN = mean)
  col_avgs_no_na = apply(X=hbf_tab, MARGIN = 2, FUN = median, na.rm=T)
  for(j in 1:ncol(hbf_tab)){
    if(is.na(col_avgs[j])){
      hbf_tab[is.na(hbf_tab[,j]),j] = col_avgs_no_na[j]
    }
    
  }
  
  # Calculate HBF component parts and add together
  hbf_tab$Int = as.numeric(weights[1])# add intercept term
  
  for(i in 1:length(ffs)){
    hbf_colname_i = paste0("comp",i)
    ff_colname = colnames(weights)[i+1]
    # multiply metric values by coefficient
    hbf_tab[,hbf_colname_i] = hbf_tab[, ff_colname] * as.numeric(weights[i+1])
  }

  hbf_tab$hbf_total = hbf_tab$Int + 
    rowSums(hbf_tab[,grep(pattern = "comp", x = colnames(hbf_tab))], na.rm=T)
  
  return(hbf_tab)
}

calc_hbf_tab_oct2025 = function(thresholds_hbf, last_wy = 2025,
                                func_flows,
                                flow_tab_for_hbf, weights,
                                scen_id = "hist_obs"){
  
  pre_hbf_tab = re_and_disconnect_date_tab(thresholds = thresholds_hbf,
                                           fj_flow = flow_tab_for_hbf)
  # fill in missing values with averages
  ff_ids = weights$Predictor[weights$Predictor !="Intercept"] 
  ff_aligned_scen = func_flows
  
  hbf_tab = add_func_flows_to_hbf_tab(pre_hbf_tab = pre_hbf_tab, 
                                      ff_ids = ff_ids,
                                      ff_aligned_scen = ff_aligned_scen,
                                      scen_id = scen_id)
  
  apply_log10_for_hbf = function(hbf_ff){
    out = log10(hbf_ff)
    out[hbf_ff == 0] = 0
    return(out)
  }

  for(ff_id in ff_ids){
    hbf_tab[,paste0(ff_id,"_log10")] = apply_log10_for_hbf(hbf_tab[ff_id])
  }
  
  hbf_tab = hbf_tab[hbf_tab$water_year <= last_wy,]
  
  write.csv(x = hbf_tab, quote = F, row.names = T,
            file = file.path(ms_dir, "Graphics and Supplements",
                             "Supplemental Table 2 - Flow Metrics by Water Year, 1942-2021.csv"))
  
  # fill gaps with averages
  col_avgs = apply(X=hbf_tab, MARGIN = 2, FUN = mean)
  col_avgs_no_na = apply(X=hbf_tab, MARGIN = 2, FUN = median, na.rm=T)
  for(j in 1:ncol(hbf_tab)){
    if(is.na(col_avgs[j])){
      hbf_tab[is.na(hbf_tab[,j]),j] = col_avgs_no_na[j]
    }
    
  }
  
  # Calculate HBF component parts and add together
  hbf_tab$Int = as.numeric(weights$Value[weights$Predictor=="Intercept"])# add intercept term
  
  for(i in 1:length(ff_ids)){
    hbf_colname_i = paste0("comp",i)
    ff_colname = paste0(weights$Predictor[i+1],"_log10")
    # multiply metric values by coefficient
    hbf_tab[,hbf_colname_i] = hbf_tab[, ff_colname] * as.numeric(weights$Value[i+1])
  }
  
  if(length(ff_ids)>1){ 
    hbf_tab$hbf_total = hbf_tab$Int + 
    rowSums(hbf_tab[,grep(pattern = "comp", x = colnames(hbf_tab))], na.rm=T)
  }
  if(length(ff_ids)==1){
    hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1
  }
  
  return(hbf_tab)
}


calculate_HBF_and_crop_ET=function(scenario_tab = scenario_tab, weights, 
                                   include_years = "all",
                                   start_wy = 1991, 
                                   end_wy = 2025
                                   ){
  
  scenarios = scenario_tab$scenario_id
  # Initialize tables for results with full water years
  n_years = length(start_wy:end_wy); n_scen = length(scenarios)
  by_wy_tab = data.frame(scenario = sort(rep(scenarios, n_years)),
                         wy = rep(start_wy:end_wy, n_scen),
                         hb_val = NA, et_tot = NA)
  
  for(i in 1:length(scenarios)){
    scenario_id = scenarios[i]
    # print(scenario_id)
    
    # pull SWBM results data
    swbm = get_swbm_tab_with_separated_et(scen_id = scenario_id)
    swbm$Month=NULL
    
    # Pull FJ flow
    fj_flow_scen = scen_fj_out[,c("Date",scenario_id)]
    colnames(fj_flow_scen)[colnames(fj_flow_scen) != "Date"] = "Flow"
    fj_flow_scen$wy = wtr_yr(fj_flow_scen$Date)
    # Pull functional flow metrics
    ff = ffs_scenarios[[scenario_id]]
    
    # pull flow thresholds from weights column names
    re_and_discon_cols = colnames(weights)[c(grep(pattern = "recon", x = colnames(weights)), 
                                             grep(pattern = "discon", x = colnames(weights)))]
    if(length(re_and_discon_cols) > 1){
      re_and_discon_cols_matrix = matrix(unlist(strsplit(re_and_discon_cols, split="_")), ncol=3, byrow=T)
      re_and_discon_thresh = unique(as.numeric(re_and_discon_cols_matrix[,3]))
    } else {re_and_discon_thresh = c(20, 40, 120)}
    
    # 1. calculate benefit value distribution
    
    hbf_tab = calc_hbf_tab_oct2025(flow_tab_for_hbf = fj_flow_scen,
                                   func_flows = ff,
                                   weights = weights, 
                                   thresholds_hbf = re_and_discon_thresh, 
                                   scen_id = scenario_id)
    # 1a) Add HBF values to the water year table
    by_wy_selector = by_wy_tab$scenario == scenario_id
    by_wy_tab$hb_val[by_wy_selector] = hbf_tab$hbf_total[match(by_wy_tab$wy[by_wy_selector], hbf_tab$water_year)]
    
    # 1b) Clean up some specific scenarios manually; i guess the algorithm gets fooled on some of these scenarios (kinda randomly)
    # if(scenario_id == "curtail_start_aug15"){
      # problem_wy = 2011; wy_selector = hbf_tab$water_year==problem_wy
      # wet_tim_prob = 57 # replace with wet season onset from hist_obs
      # hbf_tab$Wet_Tim[wy_selector] = wet_tim_prob
      # sp_tim_prob = 199
      # hbf_tab$Wet_BFL_Dur[wy_selector] = sp_tim_prob - wet_tim_prob
      # # recalc HBF components
      # hbf_tab$comp3[wy_selector] = weights[rownames(weights) == "RY_Wet_Tim"] * hbf_tab$Wet_Tim[wy_selector]
      # hbf_tab$comp4[wy_selector] = weights[rownames(weights) == "RY_Wet_BFL_Dur"] * hbf_tab$Wet_BFL_Dur[wy_selector]
      # hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
      #   hbf_tab$comp3 + hbf_tab$comp4
    # }
    # if(scenario_id == "mar_ilr_flowlims"){
    #   problem_wy = 2001; wy_selector = hbf_tab$water_year==problem_wy
    #   wet_tim_prob = 129 # replace with wet season onset from hist_obs
    #   hbf_tab$Wet_Tim[wy_selector] = wet_tim_prob
    #   sp_tim_prob = 185
    #   hbf_tab$Wet_BFL_Dur[hbf_tab$water_year==2001] = sp_tim_prob - wet_tim_prob
    #   # recalc HBF components
    #   hbf_tab$comp3[wy_selector] = weights[4] * hbf_tab$Wet_Tim[wy_selector]
    #   hbf_tab$comp4[wy_selector] = weights[5] * hbf_tab$Wet_BFL_Dur[wy_selector]
    #   hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
    #     hbf_tab$comp3 + hbf_tab$comp4 + hbf_tab$comp5
    # }
    
    # 1c) Assign HBF mean value to scenario_tab
    # filter for dry years only if selected
    if(sum(include_years %in% c("all", "All", "ALL"))<1){hbf_tab = hbf_tab[hbf_tab$water_year %in% include_years,]}
    # 1d) Calculate distribution values and assign to scenario table
    scenario_tab$HBF_mean[i] = mean(hbf_tab$hbf_total)
    scenario_tab$HBF_stdev[i]=sd(hbf_tab$hbf_total)
    
    # 2. calculate ET deviation distribution
    et_annual = aggregate(swbm$`Crop ET`, by = list(swbm$Water_Year),
                          FUN = sum)
    et_nv_annual = aggregate(swbm$`Nat. Veg. ET`, by = list(swbm$Water_Year),
                             FUN = sum)
    # 2a) Add HBF values to the water year table
    by_wy_tab$et_tot[by_wy_selector] = et_annual$x[match(by_wy_tab$wy[by_wy_selector], et_annual$Group.1)]
    
    # filter for dry years only if selected
    if(sum(include_years %in% c("all", "All", "ALL"))<1){
      et_annual = et_annual[et_annual$Group.1 %in% include_years,]
      et_nv_annual = et_annual[et_nv_annual$Group.1 %in% include_years,]
    }
    scenario_tab$et_mean[i]=mean(et_annual$x)
    scenario_tab$et_stdev[i]=sd(et_annual$x)
    scenario_tab$et_mean_nv[i] = mean(et_nv_annual$x)
    
    
    
  }
  
  results = list(scenario_tab, by_wy_tab)
  names(results) = c("scenario_tab", "obj_fn_results_by_wy")
  return(results)
}




add_plot_cols_to_scen_cat_tab = function(scen_cat_tab_1){
  scen_cat_tab_2 = scen_cat_tab_1
  # add attributes to scenario category table for plotting
  scen_cat_tab_2$descrip_legend = c("Basecase (1)", "Enhanced Recharge (1)",
                                    # "Exp. Enhanced Recharge (3)",
                                    "Improved Irrigation Efficiency (2)",
                                    "Small Reservoir (3)",
                                    "Crop Change (Alf. to Gr.) (3)",
                                    "Cease Alfalfa Irrigation Early (5)",
                                    # "Cease (Curtail) All Irr. Early (6)",
                                    "Low Flow Diversion Limits (1)",
                                    "Some Nat. Veg. Land Cover (low ET) (4)",
                                    "Some Nat. Veg. Land Cover (high ET) (4)")

  scen_cat_tab_2$category[scen_cat_tab_2$category == "Basecase"] = "basecase" #to match later scen ids

  scen_cat_tab_2$color = c("black","dodgerblue3",#"cadetblue1",
                           "plum2", "springgreen4", "darkorchid",
                           "darkgoldenrod1",#"gray", 
                           "orangered",
                           "wheat1",  "wheat4")
  scen_cat_tab_2$symbol = c(20,         # small circle
                            rep(22,4),  #square
                            rep(23,2),  #diamond
                            rep(21,2))  #circle

  return(scen_cat_tab_2)
}

add_pareto_col_to_scen_tab = function(scenario_tab){
  scenario_tab$pareto_opt = "--"
  for(i in 1:nrow(scenario_tab)){

    hbv = scenario_tab$HBF_mean[i]
    et = scenario_tab$et_mean[i] * -1
    cost = scenario_tab$feas_cat[i]

    scens_with_better_hbv = scenario_tab$scenario_id[scenario_tab$HBF_mean > hbv]
    #a less negative ET deviation is better
    scens_with_better_et = scenario_tab$scenario_id[(scenario_tab$et_mean*-1) > et]
    scens_with_better_cost = scenario_tab$scenario_id[scenario_tab$feas_cat < cost]

    scens_better_all3 = intersect(
      intersect(scens_with_better_hbv, scens_with_better_et),
      scens_with_better_cost)
    if(length(scens_better_all3) ==0){scenario_tab$pareto_opt[i] = "Yes"}
  }

  scenario_tab$size_3d = 1
  scenario_tab$size_3d[scenario_tab$pareto_opt=="Yes"] = 1.5

  return(scenario_tab)
}


# Old HBF Functions (to revise June 2024) ---------------------------------


calculate_HBF_and_crop_ET_may2023=function(scenario_tab = scenario_tab, weights, include_years = "all",
                                           start_wy = 1991, end_wy = 2018){
  
  scenarios = scenario_tab$scenario_id
  # Initialize tables for results with full water years
  n_years = length(start_wy:end_wy); n_scen = length(scenarios)
  by_wy_tab = data.frame(scenario = sort(rep(scenarios, n_years)),
                         wy = rep(start_wy:end_wy, n_scen),
                         hb_val = NA, et_tot = NA)
  
  for(i in 1:length(scenarios)){
    scenario_id = scenarios[i]
    # print(scenario_id)
    
    # pull SWBM results data
    swbm = get_swbm_budget_table(scenario_id = scenario_id)
    swbm$Month=NULL
    
    #Break out ET by land use type
    aet_monthly = get_aet_by_landuse_table(scenario_id = scenario_id)
    et_crop = aet_monthly$Alfalfa + aet_monthly$Grain + aet_monthly$Pasture
    et_natveg = aet_monthly$ET_NoIrr
    swbm$`Crop ET` = et_crop * -1 # negative for swbm outflow
    swbm$`Nat. Veg. ET` = et_natveg * -1 # negative for swbm outflow
    
    # Pull FJ flow
    fj_flow_scen = get_simulated_fj_outflow(scenario_id = scenario_id)
    
    # 1. calculate benefit value distribution
    # hbf_tab = calc_hbf_tab_feb2022(flow_tab_for_hbf = fj_flow_scen, weights = weights,
    #                                scen_id = scenario_id)
    hbf_tab = calc_hbf_tab_mar2022(flow_tab_for_hbf = fj_flow_scen, weights = weights,
                                   scen_id = scenario_id)
    # 1a) Add HBF values to the water year table
    by_wy_selector = by_wy_tab$scenario == scenario_id
    by_wy_tab$hb_val[by_wy_selector] = hbf_tab$hbf_total[match(by_wy_tab$wy[by_wy_selector], hbf_tab$water_year)]
    
    # 1b) Clean up some specific scenarios manually; i guess the algorithm gets fooled on some of these scenarios (kinda randomly)
    if(scenario_id == "curtail_start_aug15"){
      problem_wy = 2011; wy_selector = hbf_tab$water_year==problem_wy
      wet_tim_prob = 57 # replace with wet season onset from hist_obs
      hbf_tab$Wet_Tim[wy_selector] = wet_tim_prob
      sp_tim_prob = 199
      hbf_tab$Wet_BFL_Dur[wy_selector] = sp_tim_prob - wet_tim_prob
      # recalc HBF components
      hbf_tab$comp3[wy_selector] = weights[rownames(weights) == "RY_Wet_Tim"] * hbf_tab$Wet_Tim[wy_selector]
      hbf_tab$comp4[wy_selector] = weights[rownames(weights) == "RY_Wet_BFL_Dur"] * hbf_tab$Wet_BFL_Dur[wy_selector]
      hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
        hbf_tab$comp3 + hbf_tab$comp4
    }
    # if(scenario_id == "mar_ilr_flowlims"){
    #   problem_wy = 2001; wy_selector = hbf_tab$water_year==problem_wy
    #   wet_tim_prob = 129 # replace with wet season onset from hist_obs
    #   hbf_tab$Wet_Tim[wy_selector] = wet_tim_prob
    #   sp_tim_prob = 185
    #   hbf_tab$Wet_BFL_Dur[hbf_tab$water_year==2001] = sp_tim_prob - wet_tim_prob
    #   # recalc HBF components
    #   hbf_tab$comp3[wy_selector] = weights[4] * hbf_tab$Wet_Tim[wy_selector]
    #   hbf_tab$comp4[wy_selector] = weights[5] * hbf_tab$Wet_BFL_Dur[wy_selector]
    #   hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
    #     hbf_tab$comp3 + hbf_tab$comp4 + hbf_tab$comp5
    # }
    
    # 1c) Assign HBF mean value to scenario_tab
    # filter for dry years only if selected
    if(sum(include_years %in% c("all", "All", "ALL"))<1){hbf_tab = hbf_tab[hbf_tab$water_year %in% include_years,]}
    # 1d) Calculate distribution values and assign to scenario table
    scenario_tab$HBF_mean[i] = mean(hbf_tab$hbf_total)
    scenario_tab$HBF_stdev[i]=sd(hbf_tab$hbf_total)
    
    # 2. calculate ET deviation distribution
    et_annual = aggregate(swbm$`Crop ET`, by = list(swbm$Water_Year),
                          FUN = sum)
    et_nv_annual = aggregate(swbm$`Nat. Veg. ET`, by = list(swbm$Water_Year),
                             FUN = sum)
    # 2a) Add HBF values to the water year table
    by_wy_tab$et_tot[by_wy_selector] = et_annual$x[match(by_wy_tab$wy[by_wy_selector], et_annual$Group.1)]
    
    # filter for dry years only if selected
    if(sum(include_years %in% c("all", "All", "ALL"))<1){
      et_annual = et_annual[et_annual$Group.1 %in% include_years,]
      et_nv_annual = et_annual[et_nv_annual$Group.1 %in% include_years,]
    }
    scenario_tab$et_mean[i]=mean(et_annual$x)
    scenario_tab$et_stdev[i]=sd(et_annual$x)
    scenario_tab$et_mean_nv[i] = mean(et_nv_annual$x)
    
    
    
  }
  
  results = list(scenario_tab, by_wy_tab)
  names(results) = c("scenario_tab", "obj_fn_results_by_wy")
  return(results)
}

calc_hbf_tab_mar2022 = function(thresholds_hbf = c(10,100), last_wy = 2021,
                                flow_tab_for_hbf, ch1_hbftab = F, weights,
                                scen_id = "hist_obs"){

  hbf_tab = re_and_disconnect_date_tab(thresholds = thresholds_hbf,
                                       fj_flow = flow_tab_for_hbf)
  hbf_tab = hbf_tab[hbf_tab$water_year <= last_wy,]
  hbf_tab = hbf_tab[,c("water_year","recon_date_10","recon_date_100")]

  fflows = read_fflows_csv(scen_id = scen_id)

  hbf_tab$Wet_Tim = fflows$Wet_Tim[match(hbf_tab$water_year,fflows$Water_Year)]
  hbf_tab$Wet_BFL_Dur = fflows$Wet_BFL_Dur[match(hbf_tab$water_year,fflows$Water_Year)]


  if(ch1_hbftab ==T){
    write.csv(x = hbf_tab, quote = F, row.names = T,
              file = file.path(ms_dir, "Graphics and Supplements",
                               "Supplemental Table 2 - Flow Metrics by Water Year, 1942-2021.csv"))
  }

  # Calculate HBF component parts and add together
  hbf_comp = weights

  # hbf_tab$Int = hbf_comp[rownames(hbf_comp) == "Intercept" | names(hbf_comp) == "Intercept" ]
  # hbf_tab$comp1 = hbf_comp[rownames(hbf_comp) == "BY_recon_10"] * hbf_tab$recon_date_10
  # hbf_tab$comp2 = hbf_comp[rownames(hbf_comp) == "BY_recon_100"] * hbf_tab$recon_date_100
  # hbf_tab$comp3 = hbf_comp[rownames(hbf_comp) == "RY_Wet_Tim"] * hbf_tab$Wet_Tim
  # hbf_tab$comp4 = hbf_comp[rownames(hbf_comp) == "RY_Wet_BFL_Dur"] * hbf_tab$Wet_BFL_Dur

  hbf_tab$Int = hbf_comp[1]
  hbf_tab$comp1 = hbf_comp[2] * hbf_tab$recon_date_10
  hbf_tab$comp2 = hbf_comp[3] * hbf_tab$recon_date_100
  hbf_tab$comp3 = hbf_comp[4] * hbf_tab$Wet_Tim
  hbf_tab$comp4 = hbf_comp[5] * hbf_tab$Wet_BFL_Dur

  hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
    hbf_tab$comp3 + hbf_tab$comp4

  return(hbf_tab)
}


read_fflows_csv = function(scen_id){
  fflows_scen = read.csv(file.path(data_dir, "SVIHM Model Results", "tables for func flows",
                                   paste(scen_id, "func flow metrics.csv")))
  fflows = data.frame(t(as.matrix(fflows_scen)))
  colnames(fflows)=fflows_scen[,1]
  fflows = fflows[row.names(fflows)!="Year",]
  for(i in 1:ncol(fflows)){
    fflows[,i]=as.numeric(as.character(fflows[,i]))
  }

  years = as.numeric(substr(x=rownames(fflows), start = 2, stop = 5)) # convert rownames to years
  max_yr = max(years); min_yr = min(years)
  fflows$Water_Year = min_yr:max_yr

  new_col_order = c("Water_Year", colnames(fflows)[colnames(fflows)!="Water_Year"])
  fflows = fflows[,new_col_order]
  row.names(fflows) = NULL

  return(fflows)
}


# Supplement figures ------------------------------------------------------

mar_ilr_map_fig = function(){
  # assign ILR flag
  svihm_fields$ilr_flag = fields_tab$ILR_Flag[match(svihm_fields$Field_ID, fields_tab$Field_ID)]
  # assign mar and ilr color
  svihm_fields$mar_ilr_color = "gray90"
  # mar + ilr fields
  svihm_fields$mar_ilr_color[svihm_fields$mar_field == "Yes" &
                               svihm_fields$ilr_flag == 1] = marilr_col
  # mar-only fields
  svihm_fields$mar_ilr_color[svihm_fields$mar_field == "Yes" &
                               svihm_fields$ilr_flag == 0] = mar_col
  # ilr-only fields
  svihm_fields$mar_ilr_color[svihm_fields$mar_field == "No" &
                               svihm_fields$ilr_flag == 1] = ilr_col

  # Assign colors for expanded Mar + ILR
  svihm_fields$mar_ilr_exp_color = svihm_fields$mar_ilr_color
  # add all fields with a surface water source
  svihm_fields$mar_ilr_exp_color[svihm_fields$wat_source_desc=="SW"] = marilr_col


  par(mfrow = c(1,2), mar = c(1,1,1,1))
  # Panel with MAR + ILR arrangement
  plot(svihm_fields, col = svihm_fields$mar_ilr_color, border = NA)
  legend(x = "topleft", legend = "A", box.lwd = NA) # panel label
  # Panel with max MAR + ILR arrangement
  plot(svihm_fields, col = svihm_fields$mar_ilr_exp_color, border = NA)
  legend(x = "topleft", legend = "B", box.lwd = NA) # panel label
  legend(x = "bottomleft", col=c(mar_col, ilr_col, marilr_col),
         pch = 15, pt.cex = 2, legend = c("MAR", "ILR", "MAR and ILR"))

}


reservoir_map_fig = function(){
  # generate spatial object for reservoir locations
  xmin = min(bbox(basin)[1,])
  ymin = min(bbox(basin)[2,])
  x_range = diff(bbox(basin)[1,])
  y_range = diff(bbox(basin)[2,])
  monlocs = data.frame(stms = c("Shackleford", "Etna",
                                "French","South Fork"),
                       xrel = c(.00, .31, .37, .65),
                       yrel = c(.83, .38, .23, -.01),
                       xmod = c(.30, .15, .2, .30) * -1 * x_range,
                       ymod = c(0, 0, 0, 0))
  monlocs$xabs = xmin+monlocs$xrel * x_range
  monlocs$yabs = ymin+monlocs$yrel * y_range
  coordinates(monlocs)=~xabs+yabs
  proj4string(monlocs) = CRS("+init=epsg:3310")

  # plot(svihm_domain)
  plot(basin, main = "Reservoir Locations in Tributary \n Flow-altering Scenarios", col = "gray")
  plot(mapped_streams, add=T, col = "blue")
  plot(monlocs[monlocs$stms=="Etna",],add=T, col= "black", pch=24, cex = 3, bg="dodgerblue")
  plot(monlocs,add=T, col= "black", pch=24, cex = 1.5, bg="dodgerblue")
  pointLabel(x = monlocs@coords + monlocs@data[,c("xmod","ymod")],
             labels = monlocs$stms, offset = 30)
  legend(x = "bottomleft", pt.bg= c("dodgerblue","dodgerblue","gray"),
         pt.cex= c(1, 2.5, 2), pch = c(24,24,22), legend = c("9 TAF", "29 TAF", "Alluvial Aquifer"))
}

irr_regulation_fig = function(scenario_ids, scenario_colors,
                       water_year = 2015, plot_title){

  # pull basecase
  fj_bc = get_simulated_fj_outflow(scenario_id = "basecase")

  n_scens = length(scenario_ids)
  # Make list of FJ daily flow tables for curtailment scenarios
  fj_scens = vector(mode = "list", length = n_scens)
  for(i in 1:n_scens){
    scen_id = scenario_ids[i]
    fj_scens[[i]] = get_simulated_fj_outflow(scenario_id = scen_id)
    fj_scens[[i]]$scenario = scen_id

  }
  names(fj_scens) = scenario_ids


  # plot params
  ylims = c(1,5000)
  plot_dates = as.Date(c(paste0(water_year - 1,"-10-01"), paste0(water_year,"-09-30")))
  cdfw_drought = make_daily_flow_regime(regime_tab = cdfw_2021, record_date = plot_dates)

  # plot
  plot(fj_bc$Date, fj_bc$Flow, type = "l", log = "y",
       xlim = plot_dates, ylim = ylims,
       main = paste0(plot_title,", ", water_year),
       ylab = "FJ Daily Flow (cfs)",
       xlab = paste("Date in water year", water_year))
  abline(lty = 2, col = "gray", lwd = .75,
         h = 1*10^(0:5),
         v = seq.Date(from = plot_dates[1], to = plot_dates[2], by = "month"))

  # Basecase, observed flow, and CDFW Emergency Drought lines
  lines(fj_bc$Date, fj_bc$Flow, col = "black", lwd = 2)
  lines(fj_flow$Date, fj_flow$Flow, col = "gray", lwd = 2)
  lines(cdfw_drought$Date, cdfw_drought$Flow, col = "salmon", lwd = 2)

  for(j in 1:length(scenario_ids)){
    fj_scen = fj_scens[[j]]
    lines(fj_scen$Date, fj_scen$Flow, lwd = 2, lty = 2,
          col = scenario_colors[j])
  }

  legend(x = "bottomleft", lwd = 2, ncol = 2, bg="white", cex =.9,
         lty = c(1,1,1,rep(2, n_scens)),
         col = c("black", "gray", "salmon", scenario_colors),
         legend = c("Basecase Simulated", "Observed", "CDFW Emg. Drt. Flows", scenario_ids))


}

fj_streamflow_by_wy_figure= function(show_instream_flow_violations = F,
                                     show_simulated_flow = F,
                                     water_year = 2018){
  fjd = fj_flow
  max_flow = max(fjd$Flow)

  if(show_simulated_flow==TRUE){
    # Read in simulated FJ flow in case those annotations are desired
    FJ_Flow = get_simulated_fj_outflow(scenario_id = "basecase")
  }

  # plot dates
  wy_start_date = as.Date(paste0(water_year-1,"-10-01"))
  nextwy_start_date = as.Date(paste0(water_year,"-10-01"))
  fjd_wy = fjd[fjd$Date >= wy_start_date & fjd$Date < nextwy_start_date,]
  #plot
  plot(fjd_wy$Date, fjd_wy$Flow, type="l", lwd = 2, col = "blue",
       log = "y", yaxt = "n", ylim = c(1, max_flow),
       main = paste("Water Year", water_year), xlab = "Month in Water year",
       ylab = "Average Daily Flow (cfs)")
  axis(side = 2, at = 10^(0:4), las = 2, labels = c("1", "10", "100", "1000", "10,000"))
  axis(side = 2, tck = -.01, at = rep(1:9, 5) * rep(10^(0:4), each = 10), labels = NA)
  abline(h = 10^(0:4), v = seq(wy_start_date, nextwy_start_date, by="month"),
         lty = 3, col = "gray")


  if(show_instream_flow_violations==TRUE){

    start_days = as.Date(paste(water_year,cdfw_tab$start_date_month,
                               cdfw_tab$start_date_day, sep = "-"))
    end_days = as.Date(paste(water_year, cdfw_tab$end_date_month,
                             cdfw_tab$end_date_day, sep = "-"))
    start_days_lastyear =  as.Date(paste(water_year-1, cdfw_tab$start_date_month,
                                         cdfw_tab$start_date_day, sep = "-"))
    end_days_lastyear = as.Date(paste(water_year-1, cdfw_tab$end_date_month,
                                      cdfw_tab$end_date_day, sep = "-"))

    instream = data.frame(date = c(start_days_lastyear,
                                   end_days_lastyear, start_days, end_days),
                          rec_flow_cfs = rep(cdfw_tab$rec_flow_cfs, 4))
    instream = instream[order(instream$date),]

    # Plot instream flows
    lines(instream$date, instream$rec_flow_cfs, col = "dodgerblue",lwd=2)

    #color-code if flow is above or below instream flow recs
    rec_flows = data.frame(start_day = c(start_days_lastyear, start_days),
                           end_day = c(end_days_lastyear, end_days),
                           cfs = rep(cdfw_tab$rec_flow_cfs,2))
    fjd_wy$rec_flow_cfs = NA
    for(j in 1:length(rec_flows$start_day)){
      selector = fjd_wy$Date>=rec_flows$start_day[j] &fjd_wy$Date<=rec_flows$end_day[j]
      # print(sum(selector))
      fjd_wy$rec_flow_cfs[selector] = rec_flows$cfs[j]
    }
    if(i %in% seq(from=1976,to = 2040, by = 4)){# take care of leapdays
      selector = fjd_wy$Date== as.Date(paste(i, 2, 29, sep = "-"))
      fjd_wy$rec_flow_cfs[selector] = rec_flows$cfs[rec_flows$end_day==
                                                      as.Date(paste(i, 2, 28, sep = "-"))]
    }
    #calculate which days violate the cdfw recommended instream flow
    not_enough_flow = fjd_wy$Flow < fjd_wy$rec_flow_cfs

    points(fjd_wy$Date[not_enough_flow], rep(x = 1, times = sum(not_enough_flow)),
           pch = 4, col = "red")
    points(fjd_wy$Date[!not_enough_flow], rep(1,sum(!not_enough_flow)),
           pch = 4, col = "darkgreen")
    legend(x="topright",pch = c(NA,NA,4,4),
           col = c("blue","dodgerblue","darkgreen","red"),
           lwd = c(2,2,NA,NA), cex = 0.8, bg = "white",
           legend = c("Measured Flow","CDFW Flow", "Excess flow day","No excess flow")
    )

    #Calculate total number of days MAR or ILR diversions would be possible
    # (i.e. days with extra water)

    # text(x = fjd_wy$Date[1], y =2E4, pos = 4,
    #      label = paste("Total days with recommended flow or greater:",sum(!not_enough_flow)))
  }

  if(show_simulated_flow == TRUE){
    lines(FJ_flow$Date, FJ_flow$Flow_cfs, lwd = 2, lty = 2, col = "dodgerblue")
  }

}


# One-time calculations ---------------------------------------------------

save_scenario_flow_data_for_ff_calcs = function(){
  # Gotta go through this scenario by scenario so you can run Spyder in the middle of each.

  # scenario_ids = "basecase"; i=1 # for dev.
  scenario_ids = scen_ids_for_tab
  # Directory where tables of scenario daily flow data gets stored
  for_ff_tab_dir = file.path(data_dir, "SVIHM Model Results","tables for func flows")
  #weed out ones that already have fflows in the folder
  filename_list = list.files(for_ff_tab_dir)
  ff_scenario_filenames = filename_list[grep(filename_list,pattern = "func flow metrics.csv")]
  existing_scen_ids = trim(unlist(strsplit(x = ff_scenario_filenames, split = "func flow metrics.csv")))

  scenario_ids = scen_ids_for_tab[!(scen_ids_for_tab %in% existing_scen_ids)]

  # Directory in Functional Flows project where raw flow data is stored for processing
  ff_calc_rawfile_dir = "C:/Users/Claire/Documents/GitHub/func-flow/rawFiles"
  # Directory in Functional Flows project where processed functional flow metrics matrix is stored
  ff_calc_postproc_dir = "C:/Users/Claire/Documents/GitHub/func-flow/post_processedFiles/Class-3"

  # File names for template RawFile, RawFile with new scenario data inserted
  ff_template_file = file.path(for_ff_tab_dir, "orig new_postProcess_4.csv")
  ff_new_tab_file = file.path(for_ff_tab_dir, "new_postProcess_4.csv")
  # File name for the postprocessed metric data in the Functional Flows project
  postproc_file = file.path(ff_calc_postproc_dir,"11413100_annual_result_matrix.csv")

  # Prepare to overwrite template file
  ff_line_1 = ",3,,3,,2,,9,,2"
  ff_line_2 = "result_dt,11413100,result_dt,11341400,result_dt,11299000,result_dt,11355500,result_dt,11446220"
  ff_ncols = 10
  ff_tab_template = read.csv(file = ff_template_file, skip = 1)
  i=0 # initialize i index


  # Run this block for every scenario
  # spyder setup block start-----------------------------------------------
  i= i+1
    scen_id = scenario_ids[i]
    ff_scen_tab_filename = file.path(for_ff_tab_dir, paste(scen_id, "func flow metrics.csv")) # output filename
    scen_fj = get_simulated_fj_outflow(scenario_id = scen_id)

    if(scen_id == "hist_obs"){
      # Pull, process flow data
      scen_id = "hist_obs_3"
      # File name for when FF metric matrixs gets copied over to the dissertation directory
      ff_scen_tab_filename = file.path(for_ff_tab_dir, paste(scen_id, "func flow metrics.csv"))

      # define flow data
      # scen_fj = fj_flow[fj_flow$Date>= as.Date("1941-10-01") & fj_flow$Date<= as.Date("1968-09-30"),]
      # scen_fj = fj_flow[fj_flow$Date>= as.Date("1968-10-01") & fj_flow$Date<= as.Date("1987-09-30"),]
      # scen_fj = fj_flow[fj_flow$Date>= as.Date("1987-10-01") & fj_flow$Date<= as.Date("1988-09-30"),]
      scen_fj = fj_flow[fj_flow$Date>= as.Date("1987-10-01") & fj_flow$Date<= as.Date("2021-09-30"),]

    }

    scen_fj = scen_fj[,c("Date", "Flow")] # scen_fj$Date = format(scen_fj$Date, format = "%m/%d%/%Y")
    scen_fj$Flow=round(scen_fj$Flow)

    row_diff = nrow(scen_fj) - nrow(ff_tab_template)
    if(row_diff <= 0){ # make filler rows on bottom of scen_fj or ff_tab_template if necessary
      scen_fj = rbind(scen_fj, data.frame(Date = rep(NA, abs(row_diff)), Flow = rep(NA, abs(row_diff))) )
    } else {
      temp_tab = ff_tab_template[1:row_diff,] ; ff_tab_template = rbind(ff_tab_template, temp_tab)
    }

    #transpose so it writes properly in the file
    ff_tab_new = t(as.matrix(cbind(scen_fj, ff_tab_template[,c(-1,-2)])))
    # Write the scenario flow data into the template file
    writeLines(ff_line_1, con = ff_new_tab_file)
    write(ff_line_2, file = ff_new_tab_file, append=T)
    write(ff_tab_new, file = ff_new_tab_file, ncolumns = ff_ncols, append = T, sep = ",")

    # copy this back to the python metric-calculator file (overwrite existing version)
    file.copy(from = ff_new_tab_file, to = file.path(ff_calc_rawfile_dir,"new_postProcess_4.csv"), overwrite = T)
    print(paste(scen_id, "copied to FF dir;", i, "of", length(scenario_ids)))
    # spyder setup block end-----------------------------

    #### OPERATOR
    # Open Spyder. navigate to main.py.
    ## Hit Run.
    ## Enter 7 (Create Annual Flow Matrix csv)
    ## Hit enter to accept 10/1 start of water year
    ## Enter 2 (calculate for gauges)
    ## Enter "11413100" (the name of the gauge whose data you've replaced with the scenario flow)
    ## It will run. Probably warnings.
    ## It will say "Done!!!"

    # Then run this line to copy the file back into this folder, with the scenario_id in the file name
    file.copy(from = postproc_file, to = ff_scen_tab_filename) ;print(paste(scen_id, "FF matrix copied back to diss. dir"))

}


# scratchwork -------------------------------------------------------------



obs_v_sim_flow_fig= function(water_year = 2016, obs_flow, sim_flow){
  # obs_flow = obs
  # sim_flow = sim

  obs_wy = obs_flow[obs_flow$wy==water_year,]
  sim_wy = sim_flow[sim_flow$wy==water_year,]

  # Initialize Plot
  par(mar = c(5,5,4,5) + 0.1) # add more space for 2nd axis
  plot(obs_wy$Date, obs_wy$Flow, type = "l", log = "y",
       lwd = 2, col = obs_col, yaxt = "n", xaxt="n",
       ylim = range(obs_wy$Flow),
       main = "Observed FJ Flow vs. FJ Flow simulated with SVIHM",
       # ylab = "Average Daily Flow at FJ Gauge (cfs)",
       ylab = expression(Average~Daily~Flow~at~FJ~Gauge~(ft^3~"/"~sec)),
       xlab = paste("Date in water year", water_year))
  lines(sim_wy$Date, sim_wy$Flow, lwd = 2, col = sim_col)

  # Plot fine-tuning
  flow_labels = c("0.1","1","10","100","1,000","10,000","100,000")
  # date_lines = as.Date(paste0(water_year,"-01-01")) + (month_1s - 1)
  date_lines = seq.Date(from=min(obs_wy$Date), to = max(obs_wy$Date)+32, by = "month")
  label_these_months = date_lines[c(2,4,6,8,10,12,14)]
  axis(side=1, at=label_these_months,
       labels = month.abb[c(month(label_these_months))], las = 1)
  axis(side=2, at=10^(-1:5), labels = flow_labels, las = 1)
  axis(side=2, at=rep(1:9,6) * 10 ^ (rep(0:5, each = 9)), labels = NA, tck = -0.01)
  abline(h=10^(0:5), v = date_lines, col = "gray", lty = 3)

  # Add 2nd cms axis
  par(new=TRUE)
  plot(x=c(1,1), y = range(obs_wy$Flow) * cfs_to_m3sec,
       ylim = range(obs_wy$Flow) * cfs_to_m3sec, col = NA,
       xaxt = "n", yaxt = "n", log = "y", ylab = "", xlab = "")
  axis(side=4, at=10^(-1:5), labels = flow_labels, las = 1)
  axis(side=4, at=rep(1:9,7) * 10 ^ (rep(-1:5, each = 9)), labels = NA, tck = -0.01)
  mtext(expression(Average~Daily~Flow~at~FJ~Gauge~(m^3~"/"~sec)), side = 4, line = 3)

  legend(x="topright", lwd=2, col = c(obs_col, sim_col), legend = c("Obs.", "Sim."))
}

obs_v_sim_hbf_fig = function(obs_hbf_tab, sim_hbf_tab){
  fj_flow_bc = get_simulated_fj_outflow(scenario_id = "basecase")
  sim_hbf_tab = calc_hbf_tab_mar2022(flow_tab_for_hbf = fj_flow_bc,
                                     weights = metric_weights,
                                     ch1_hbftab = F,
                                     thresholds_hbf = c(10,100),
                                     last_wy = 2018,
                                     scen_id = "basecase")
  obs_hbf_tab = calc_hbf_tab_mar2022(flow_tab_for_hbf = fj_flow,
                                     weights = metric_weights,
                                     ch1_hbftab = F,
                                     thresholds_hbf = c(10,100),
                                     last_wy = 2018,
                                     scen_id = "hist_obs")
  obs_hbf_tab = obs_hbf_tab[obs_hbf_tab$water_year >= 1991,]
  # par(mfrow = c(2,1))
  # Panel 1, time series
  # par(mar = c(5,4,5,1)) # extra room for plot title
  plot(x = obs_hbf_tab$water_year, y = obs_hbf_tab$hbf_total,
       main = "HB values calculated from observed and \n simulated flow, water years 1991-2018",
       xlab = "Water Year", ylab = "HB value (coho spf-equiv.)",
       col = obs_col, type = "o", pch = 19)
  points(x = sim_hbf_tab$water_year, y = sim_hbf_tab$hbf_total, col = sim_col, pch = 19)
  lines(x = sim_hbf_tab$water_year, y = sim_hbf_tab$hbf_total, col = sim_col)
  grid()
  abline(h=0, lwd = 1.5, col= "gray")

  summary(obs_hbf_tab$hbf_total - sim_hbf_tab$hbf_total)
  # sim overpredicts by a mean of 4.5 coho spf (range of 37 underpredict to 49 over)

  # # Panel 2, cdf fuction
  # par(mar = c(5,4,1,1)) # remove space for plot title
  # plot(x = (1:nrow(obs_hbf_tab))/nrow(obs_hbf_tab),
  #      y = sort(obs_hbf_tab$hbf_total, decreasing = T),
  #      xlab = "Exceedance Probability", ylab = "HB value (coho spf-equiv.)",
  #      type = "o", pch = 19, col = obs_col)
  # points(x = (1:nrow(sim_hbf_tab))/nrow(obs_hbf_tab),
  #        y = sort(sim_hbf_tab$hbf_total, decreasing = T),
  #        type = "o", pch = 19, col = sim_col)
  # grid()
  # abline(h=0, lwd = 1.5, col= "gray")
  # legend(x="bottomleft", pch = 19, lty = 1, col = c(obs_col, sim_col),
  #        legend = c("Observed Flow", "Simulated Flow"))
}




# calculate_HBF_and_ET=function(scenario_tab = scenario_tab, weights){
#
#   scenarios = scenario_tab$scenario_id
#
#   for(i in 1:length(scenarios)){
#     scenario_id = scenarios[i]
#     # print(scenario_id)
#     # pull results data
#     swbm = get_swbm_budget_table(scenario_id = scenario_id)
#     fj_flow_scen = get_simulated_fj_outflow(scenario_id = scenario_id)
#
#     # 1. calculate benefit value distribution
#     hbf_tab = calc_hbf_tab_feb2022(flow_tab_for_hbf = fj_flow_scen, weights = weights,
#                                    scen_id = scenario_id)
#
#     # clean up some specific scenarios manually; i guess the algorithm gets fooled on some of these scenarios (kinda randomly)
#     if(scenario_id == "curtail_start_aug15"){
#       problem_wy = 2011; wy_selector = hbf_tab$water_year==problem_wy
#       wet_tim_prob = 57 # replace with wet season onset from hist_obs
#       hbf_tab$Wet_Tim[wy_selector] = wet_tim_prob
#       sp_tim_prob = 199
#       hbf_tab$Wet_BFL_Dur[wy_selector] = sp_tim_prob - wet_tim_prob
#       # recalc HBF components
#       hbf_tab$comp3[wy_selector] = weights[4] * hbf_tab$Wet_Tim[wy_selector]
#       hbf_tab$comp4[wy_selector] = weights[5] * hbf_tab$Wet_BFL_Dur[wy_selector]
#       hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
#         hbf_tab$comp3 + hbf_tab$comp4 + hbf_tab$comp5
#     }
#     if(scenario_id == "mar_ilr_flowlims"){
#       problem_wy = 2001; wy_selector = hbf_tab$water_year==problem_wy
#       wet_tim_prob = 129 # replace with wet season onset from hist_obs
#       hbf_tab$Wet_Tim[wy_selector] = wet_tim_prob
#       sp_tim_prob = 185
#       hbf_tab$Wet_BFL_Dur[hbf_tab$water_year==2001] = sp_tim_prob - wet_tim_prob
#       # recalc HBF components
#       hbf_tab$comp3[wy_selector] = weights[4] * hbf_tab$Wet_Tim[wy_selector]
#       hbf_tab$comp4[wy_selector] = weights[5] * hbf_tab$Wet_BFL_Dur[wy_selector]
#       hbf_tab$hbf_total = hbf_tab$Int + hbf_tab$comp1 + hbf_tab$comp2 +
#         hbf_tab$comp3 + hbf_tab$comp4 + hbf_tab$comp5
#     }
#
#     # Calculate distribution values and assign to scenario table
#     scenario_tab$HBF_mean[i] = mean(hbf_tab$hbf_total)
#     scenario_tab$HBF_stdev[i]=sd(hbf_tab$hbf_total)
#
#     # 2. calculate ET deviation distribution
#     et_annual = aggregate(swbm$ET,
#                           by = list(swbm$Water_Year),
#                           FUN = sum)
#     scenario_tab$et_mean[i]=mean(et_annual$x)
#     scenario_tab$et_stdev[i]=sd(et_annual$x)
#   }
#   return(scenario_tab)
# }


# calculate_HBF_and_ET_dev=function(scenario_tab = scenario_tab, et_hist_mean, weights){
#
#   scenarios = scenario_tab$scenario_id
#
#   for(i in 1:length(scenarios)){
#     scenario_id = scenarios[i]
#     # pull results data
#     swbm = get_swbm_budget_table(scenario_id = scenario_id)
#     fj_flow_scen = get_simulated_fj_outflow(scenario_id = scenario_id)
#
#     # 1. calculate benefit value distribution
#     hbf_tab = calc_hbf_tab_feb2022(flow_tab_for_hbf = fj_flow_scen, weights = weights,
#                                    scen_id = scenario_id)
#
#     # Calculate distribution values and assign to scenario table
#     scenario_tab$HBF_mean[i] = mean(hbf_tab$hbf_total)
#     scenario_tab$HBF_stdev[i]=sd(hbf_tab$hbf_total)
#
#     # 2. calculate ET deviation distribution
#     et_annual = aggregate(swbm$ET,
#                           by = list(swbm$Water_Year),
#                           FUN = sum)
#     et_dev_dist = (et_annual$x / et_hist_mean) - 1
#     scenario_tab$et_dev_mean[i]=mean(et_dev_dist)
#     scenario_tab$et_dev_stdev[i]=sd(et_dev_dist)
#   }
#   return(scenario_tab)
# }


parallel_coord_figure = function(scenario_tab, alpha_val = .6){
  # # Save historical ET mean
  scen_id = "basecase"
  # swbm_monthly = get_swbm_budget_table(scenario_id = scen_id)
  # et_annual = aggregate(swbm_monthly$ET,
  #                       by = list(swbm_monthly$Water_Year),
  #                       FUN = sum)
  # mean_et_basecase = mean(et_annual$x)
  mean_et_basecase = scenario_tab$et_mean[scenario_tab$scenario_id=="basecase"]

  # Save historical ET hbf
  fj_flow_bc = get_simulated_fj_outflow(scenario_id = "basecase")
  hbf_tab = calc_hbf_tab_feb2022(flow_tab_for_hbf = fj_flow_bc, weights = metric_weights,
                                 scen_id = "basecase")
  mean_hbf_basecase = mean(hbf_tab$hbf_total)

  scenario_tab$hbf_rel = (scenario_tab$HBF_mean-mean_hbf_basecase) / mean_hbf_basecase
  scenario_tab$et_rel = (scenario_tab$et_mean-mean_et_basecase) / mean_et_basecase
  scenario_tab$feas_rel = scenario_tab$feas_cat / - 8
  y_range = range(scenario_tab[,c("hbf_rel","et_rel", "feas_rel")])

  for(i in 1:nrow(scenario_tab)){
    scen_id = scenario_tab$scenario_id[i]

    if(i==1){ #initialize plot
      par(mar = c(5,5,5,2))
      plot(x = c(1,3), y =c(0.5, -1), pch = 0, col = NA,
           xaxt = "n", ylab = "Objective performance relative to basecase \n (Lower numbers = less favorable)", xlab = "")
      # axis labels
      axis(side = 1, at = 1:3, labels = c("HBF", "ET", "Feasibility"))
      grid()
    }
    points(x = 1:3, scenario_tab[i,c("hbf_rel","et_rel", "feas_rel")],
           type = "o", pch = 19, cex = 1.2, lwd = 2, col = alpha(scenario_tab$color[i], alpha_val))
    # lines(x = 1:3, scenario_tab[i,c("hbf_rel","et_rel", "feas_rel")],
    # col = alpha(scenario_tab$color[i], 0.5), lwd = 2)
  }
  legend(x = "topright", pch=21, pt.bg = scen_cat_tab$color,
         pt.cex = 1.5, cex = 0.8,
         title = "Mgmt. Action Category",# (# of scenarios)",
         # col = scen_cat_tab$color, pch = 19,
         legend = scen_cat_tab$category_long)
}


