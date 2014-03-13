pbeta <-
function(x,mu_d,t=8)
{
	a<-0
	b<-6
	r<-t*(0.007*mu_d^3-0.0525*mu_d^2+0.2875*mu_d)
	p1<-gamma(t)/(gamma(r)*gamma(t-r))
	p2<-p1*((x-a)^(r-1)*(b-x)^(t-r-1))/((b-a)^(t-1))
	p2
}
