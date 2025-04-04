	  program NewtonRaphson
	  real x0,x1,xs,e1,e2, Delta,f,df
	  print*, "Entre com x0."
	  read*,x0
	  x1=0.
	  xs=0.
	  e1=0.01
	  e2=0.01
	  Delta=x1-x0
	  f=0.95*x0**3-5.9*x0**2+10.9*x0-6
	  if (abs(f).lt.e1) then 
	  	xs=x0
	  else
	  	dowhile((abs(f).gt. e1).or.(abs(Delta).gt.e2))
	  		f= 0.95*x0**3-5.9*x0**2+10.9*x0-6
	  		df= 380.95*x0**2-2*5.9*x0+10.9
	  		x1=x0 - f/df
	  	    f= 0.95*x1**3-5.9*x1**2+10.9*x1-6
	  	    Delta=x1-x0
            x0=x1
	  	enddo
	  	xs=x0
	  endif
	  print*,"a solução é ", xs
	  end