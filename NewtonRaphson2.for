	  program NewtonRaphson
	  real x0,x1,xs,e1,e2, Delta,f,df
	  print*, "Entre com x0."
	  read*,x0
	  x1=0.
	  xs=0.
	  e1=0.0001
	  e2=0.0001
	  Delta=x1-x0
	  f=log((1+0.05*x0)/(0.05*x0)) - (x0+1)/(x0*(1+0.05*x0))
	  if (abs(f).lt.e1) then 
	  	xs=x0
	  else
	  	dowhile((abs(f).gt. e1).or.(abs(Delta).gt.e2))
	  		f=log((1.+0.05*x0)/(0.05*x0)) - (x0+1)/(x0*(1.+0.05*x0))
	  		df= 4/(x0**2*(x0+2)**2)
	  		x1=x0 - f/df
	  	    f=log((1.+0.05*x1)/(0.05*x1)) - (x1+1.)/(x1*(1.+0.05*x1))
	  	    Delta=x1-x0
            x0=x1
	  	enddo
	  	xs=x0
	  endif
	  print*,"a solução é ", xs
	  end