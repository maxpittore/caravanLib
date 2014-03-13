ems98_DG <-
function(I,V,Q=2.3)
{
	res <- 2.5*(1+tanh((I+6.25*V-13.1)/Q))
	res
}
