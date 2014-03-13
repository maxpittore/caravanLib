ems98_DPM <-
function(ems,type)
{
	V<-ems98_C2V(ems,type)
	#init dpm
	mus<-sapply(seq(5,10),function(x) rep(ems98_DG(x,V),6))
	ind<-rep(seq(0,5),6)
	d<-sapply((1:36),function(x) P_DS(ind[x],mus[x]))
	dpm<-matrix(d,nrow=6,ncol=6,byrow=TRUE,
			dimnames=list(c('V','VI','VII','VIII','IX','X'),c('d0','d1','d2','d3','d4','d5')))
	dpm
}
