	  program Secante
	  real f0,f1,Delta,x0,x1,x2,xs,e1,e2
	  print*, "Entre com x0, x1."
	  read*,x0,x1
	  x2=0.
	  xs=0.
	  e1=0.01
	  e2=0.01
	  Delta=x1-x0
	  f0= 0.95*x0**3-5.9*x0**2+10.9*x0-6
	  f1= 0.95*x1**3-5.9*x1**2+10.9*x1-6
	  if (abs(f0).lt.e1) then 
	  	xs=x0
	  else
	  	if((abs(f1).lt. e1).or.(abs(Delta).lt.e2)) then
	  		xs=x1
	  	else
	  		dowhile((abs(f1).gt. e1).or.(abs(Delta).gt.e2))
	  			f0= 0.95*x0**3-5.9*x0**2+10.9*x0-6
	  			f1= 0.95*x1**3-5.9*x1**2+10.9*x1-6
	  			x2=x1 - f1*Delta/(f1-f0)
	  	    	f1= 0.95*x2**3-5.9*x2**2+10.9*x2-6
	  	    	Delta=x2-x1
            	x0=x1
            	x1=x2
	  		enddo
	  	xs=x2
	  	endif
	  endif
	  print*,"a solução é ", xs
	  end