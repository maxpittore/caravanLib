compute_loss_geocells <-
function(shakemap,nighttime=FALSE,dbin,dbout)
{
	nighttime_coeff <- 0.5
	daytime_coeff <- 0.3
	coeff<-ifelse(nighttime,nighttime_coeff,daytime_coeff)
	# load geocells from db
	tmpdata<-readOGR(dsn=dbin$dbconn,layer=dbin$layer)  #"urbanstructures")
	
	# project into WGS84
	tmpdata<-spTransform(tmpdata,CRS('+proj=longlat +datum=WGS84 +no_defs'))
	# filter out non-built-up and industrial/commercial areas
	geocells<-subset(tmpdata,tmpdata@data$structure >0)
	
	#compute centroids of geocells
	geocells_centroids<-coordinates(geocells)
	
	# evaluate intensity for each settlement using the shakemap
	intens<-round(extract(shakemap,geocells_centroids,method='bilinear'))
	
	#keep only intensities >5
	ind<-which(intens>5)
	
	# get vulnerability composition for all strata
	vuln_strata<-vuln_strata()

	# select uncertainty level. type=3 refers to most likely fragility values
	type<-3
	# init DPMs
	dpm_a<-ems98_DPM('a',type)
	dpm_b<-ems98_DPM('b',type)
	dpm_c<-ems98_DPM('c',type)
	dpm_d<-ems98_DPM('d',type)
	dpm_e<-ems98_DPM('e',type)
	dpm_f<-ems98_DPM('f',type)
	
	#service function to process each geocell
	process_geocells<-function(cell)
	{
		stratum<-geocells[cell,]@data$structure
		nb<-round(geocells[cell,]@data$nbuildings_mean)
		ncel<-0
		ncol<-dpm_a[intens[cell]-4,'d5']*vuln_strata[stratum,1]
		ncol<-ncol+dpm_b[intens[cell]-4,'d5']*vuln_strata[stratum,2]
		ncol<-ncol+dpm_c[intens[cell]-4,'d5']*vuln_strata[stratum,3]
		ncol<-ncol+dpm_d[intens[cell]-4,'d5']*vuln_strata[stratum,4]
		ncol<-ncol+dpm_e[intens[cell]-4,'d5']*vuln_strata[stratum,5]
		ncol<-ncol+dpm_f[intens[cell]-4,'d5']*vuln_strata[stratum,6]
		
		ncol<-round(ncol*nb)
		#average occupancy
		avg_occ <- 10
		#estimated fatalities, using modifier coefficient
		nfat<-ncol*avg_occ*coeff
	}
	
	#for each geocell estimate fatalities
	est_fat<-(sapply(ind,FUN=process_geocells))

	df2<-data.frame(geocells[ind,]@data,est_fat)
	df3<-SpatialPolygonsDataFrame(geocells[ind,],df2)
		
	# write loss estimates to the db
	#db_conn<-"PG:dbname=test_db host='localhost' user='postgres' password='postgres'"

	#dbconn<-"PG:dbname='Bishkek2012' host='localhost' user='postgres' password='postgres'"
	#dbout<-list(dbconn=dbconn,layer='urbanstructures_loss',overwrite=TRUE)

	writeOGR(df3,dbout$dbconn,dbout$layer, "PostgreSQL",overwrite_layer=dbout$overwrite)
	
	#uncomment to save them as a shapefile
	#path<-'/home/max/Documents/GFZ_sync/workspace/yurta/Loss'
	#writeOGR(df3,path,"geocells_loss", driver="ESRI Shapefile")

	return(tot_fatalities=sum(est_fat))
}
