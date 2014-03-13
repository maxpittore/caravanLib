P_DS <-
function(dg,mu_d,t=8)
{
	integrate(function(x)pbeta(x,mu_d,t),dg,dg+1)$value
}
