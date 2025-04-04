	  program integracaoSimpsonRepetida
	  real x0,x1,x2,Isr,IsrT,a,b,h,pi,IsrTpedido
	  integer m
	  pi = 3.141592653589793238
	  print*,""
	  print*,"entre com a probabilidade"
	  read*,IsrTpedido
	  if (Isrpedido.le.0.50) then
	  IsrTpedido=0.5-IsrTpedido
	  a=0.
	  do i=1,1000000
	  	x0=a
	    Isr=0.
	  	b=i*0.0001
	    m=1000
	    h=abs((b-a)/m)
		do j=1,m/2-1
	  		x1=x0+h
	  		x2=x1+h
	  		Isr=Isr+4.*exp(-x1**2./2.)+2.*exp(-x2**2./2.)
	   		x0=x2
	  	enddo
	  	IsrT=exp(-b**2./2.)+exp(-a**2./2.)+4*exp(-((m-1)*h)**2./2.)+Isr
	  	IsrT=IsrT*h/(3*sqrt(2.*pi))
	    if (abs(IsrTpedido-IsrT).le.0.0001) then
	    b=-b
	    	print*,"o valor da variável aleatória reduzida é ", b
	    	goto 10
	    endif
	  enddo
	  else
	  IsrTpedido=IsrTpedido-0.5
	  a=0.
	  do i=1,1000000
	  	x0=a
	    Isr=0.
	  	b=i*0.0001
	    m=1000
	    h=abs((b-a)/m)
		do j=1,m/2-1
	  		x1=x0+h
	  		x2=x1+h
	  		Isr=Isr+4.*exp(-x1**2./2.)+2.*exp(-x2**2./2.)
	   		x0=x2
	  	enddo
	  	IsrT=exp(-b**2./2.)+exp(-a**2./2.)+4*exp(-((m-1)*h)**2./2.)+Isr
	  	IsrT=IsrT*h/(3*sqrt(2.*pi))
	    if (abs(IsrTpedido-IsrT).le.0.0001) then
	    	print*,"o valor da variável aleatória reduzida é ", b
	    	goto 10
	    endif
	  enddo
	  endif
10	  return
      end

