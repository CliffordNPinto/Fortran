      program NewtonRaphson
	  real f,df,Delta,x0,x1,xs,e1,e2
	  print*, "Entre com x0"
	  read*,x0
	  x1=0.
	  xs=0.
	  e1=0.0001
	  e2=0.0001
	  Delta=x1-x0
	  f= 2*sin(sqrt(x0)) -x0
	  if (abs(f).lt.e1) then 
	  	xs=x0
	  else
	  	dowhile((abs(f).gt. e1).or.(abs(Delta).gt.e2))
	  		f=2*sin(sqrt(x0)) - x0
	  		df= cos(sqrt(x0))*x0**(-0.5) - x0
	  		x1=x0 - f/df
	  	    f=2*sin(sqrt(x1)) -x1
	  	    Delta=x1-x0
            x0=x1
	  	enddo
	  	xs=x0
	  endif
	  print*,"a solução é ", xs
	  end