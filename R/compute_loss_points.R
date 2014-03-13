compute_loss_points <-
function(shakemap,nighttime=FALSE,dbin,dbout)
{
	#define vulnerability - draft
	vuln_emca <- c('b','b','b','d','d','e','e','c','d','a','c','d','e')
	mmi<-c('I','II','III','IV','V','VI','VII','VIII','IX','X')
	occupancy <- c(28, 8, 140, 60,200,200,200,200,140,5,28,5,5)
	nighttime_coeff <- 0.5
	daytime_coeff <- 0.3
	nclass<-13
	# off: offset, depends on the number of columns of input data frame
	off<-4
	
	# daytime or nighttime?
	coeff<-ifelse(nighttime,nighttime_coeff,daytime_coeff)
	
	#load exposure
	# load coords of settlements as spatial points	
	data<-readOGR(dsn=dbin$dbconn,layer=dbin$layer)
	if (is.na(proj4string(data)))
	{
		print('no projection on input file, assigning WGS84')
		proj4string(data)<-"+proj=longlat +datum=WGS84 +no_defs"
	}
	# filter out places with no population information	
	places<-subset(data,data$population>0 & data$population < 1e6)
		
	# evaluate intensity for each settlement using the shakemap
	intens<-round(extract(shakemap,places@coords,method='bilinear'))
	
	#keep only intensities >5
	ind<-which(intens>5)
	
	#plot(shakemap)
	#points(places[ind,],col='red')
		
	loss<-matrix(ncol=6,nrow=length(ind))
	tot_loss_min<-0
	tot_loss_med<-0
	tot_loss_max<-0
	i<-1
	for (place in ind) # loop on settlements
	{
		n_fat_min<-0
		n_fat_med<-0
		n_fat_max<-0
		for (type in 1:nclass) # loop on building types
		{	
			#get dpm for most likely vulnerability
			dpm<-ems98_DPM(vuln_emca[type],3)
			
			#compute prob of collapse
			pd4<-dpm[mmi[intens[place]],'d4']
			pd5<-dpm[mmi[intens[place]],'d5']
		
			# expected min (5%) num  
			b_min<-(places@data)[place,off+type]	
			# expected median 
			b_med<-(places@data)[place,off+nclass+type]	
			# expected max (95%) num  
			b_max<-(places@data)[place,off+2*nclass+type]	
			
			# number of expected collapses
			# uses damage grade 5 plus 25% of damage grade 4 (see Spence 2002)
			pd<-(pd5+0.25*pd4)
			n_coll_min<-pd*b_min
			n_coll_med<-pd*b_med
			n_coll_max<-pd*b_max
			
			# addd on the expected fatalities
			n_fat_min<-n_fat_min+(occupancy[type]*coeff*n_coll_min)
			n_fat_med<-n_fat_med+(occupancy[type]*coeff*n_coll_med)
			n_fat_max<-n_fat_max+(occupancy[type]*coeff*n_coll_max)
		}
		
		# absolute loss
		loss[i,1]<-round(n_fat_min)
		loss[i,2]<-round(n_fat_med)
		loss[i,3]<-round(n_fat_max)
		
		# total loss
		tot_loss_min<-tot_loss_min+loss[i,1]
		tot_loss_med<-tot_loss_med+loss[i,2]
		tot_loss_max<-tot_loss_max+loss[i,3]
		
		# relative loss: fatalities as percentage of population
		pop<-(places@data)$population[place]
		loss[i,4]<-n_fat_min/pop
		loss[i,5]<-n_fat_med/pop
		loss[i,6]<-n_fat_max/pop
		
		i<-i+1
	}
	
	# write loss estimates to the db
	df2<-data.frame(osm_id=places@data$osm_id[ind],name=places@data$name[ind],population=places@data$population[ind],loss=loss)
	df3<-SpatialPointsDataFrame(places[ind,],df2)

	#db_conn<-"PG:dbname=test_db host='localhost' user='postgres' password='postgres'"
	writeOGR(df3,dbout$dbconn,dbout$layer, "PostgreSQL",overwrite_layer=dbout$overwrite)
	
	return (list(totloss_min=tot_loss_min,totloss_med=tot_loss_med,totloss_max=tot_loss_max))
}
