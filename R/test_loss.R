test_loss <-
function(type)
{
	#load shakemap
	#file<-sprintf("%s%s",path,'rasters/buildings_density_2_utm32n.tiff')
	path<-'/home/max/Documents/GFZ_sync/workspace/yurta/'
	file<-sprintf("%s%s",path,'Intensities/intensities.tif')
	GDALinfo(file)
	#read data from file
	shakemap_gdal= readGDAL(file)
	#convert to raster
	shakemap<-raster(shakemap_gdal)
	
	dbconn<-"PG:dbname=test_db host='localhost' user='postgres' password='postgres'"
	dbconn2<-"PG:dbname=centralasia_db host='lhotse.gfz-potsdam.de' user='postgres' password='postgres'"
	
	if (type==1)
	{
		dbin<-list(dbconn=dbconn2,layer='places',overwrite=FALSE)
		dbout<-list(dbconn=dbconn2,layer='places_loss',overwrite=TRUE)	
		compute_loss_points(shakemap,nighttime=FALSE,dbin,dbout)
	} 
	else if (type==2)
	{
		# test geocell losses
		dbin<-list(dbconn=dbconn2,layer='urbanstructures',overwrite=FALSE)
		dbout<-list(dbconn=dbconn2,layer='urbanstructures_loss',overwrite=TRUE)
		compute_loss_geocells(shakemap,nighttime=FALSE,dbin,dbout)
	}
	
}
