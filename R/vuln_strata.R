vuln_strata <-
function()
{
	# load average vulnerability distributions
	# from postgresql db
	sql <- "select structure as s, unnest(post_vuln) as vuln from avg_posteriors;" 
	
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, host='lhotse.gfz-potsdam.de', user= "postgres", password="postgres", dbname="centralasia_db")
	post <- dbGetQuery(con, statement = sql)
	
	# Closes the connection
	dbDisconnect(con)
	# Frees all the resources on the driver
	dbUnloadDriver(drv)	
	
	# return avg vulnerability distribution of all strata 
	#post$vuln[post$s==stratum]
	t(sapply(1:21,FUN=function(x){post$vuln[post$s==x]}))
}
