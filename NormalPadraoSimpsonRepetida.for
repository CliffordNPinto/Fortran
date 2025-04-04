	  program integracaoSimpsonRepetida
	  real x0,x1,x2,Isr,IsrT,a,b,h,pi
	  integer m
	  pi = 3.141592653589793238
	  print*,"entre com b e m"
	  read*,b,m
	  a=0.
	  h=abs((b-a)/m)
	  x0=a
	  Isr=0.
	  do i=1,m/2-1
	  	x1=x0+h
	  	x2=x1+h
	  	Isr=Isr+4.*exp(-x1**2./2.)+2.*exp(-x2**2./2.)
	   	x0=x2
	  enddo
	  IsrT=exp(-b**2./2.)+exp(-a**2./2.)+4*exp(-((m-1)*h)**2./2.)+Isr
	  IsrT=IsrT*h/(3*sqrt(2.*pi))+0.50
	  print*,IsrT
	  end

