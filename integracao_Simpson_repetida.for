	  program integracaoSimpsonRepetida
	  real x0,x1,x2,Isr,IsrT,a,b,h
	  integer m
	  print*,"entre com a,b e m"
	  read*,a,b,m
	  h=abs((b-a)/m)
	  x0=a
	  Isr=0.
	  do i=1,m/2-1
	  	x1=x0+h
	  	x2=x1+h
	  	Isr=Isr+4.*exp(x1)+2*exp(x2)
	   	x0=x2
	  enddo
	  IsrT=(exp(b)+exp(a)+4*exp((m-1)*h)+Isr)*h/3
	  print*,IsrT
	  end

