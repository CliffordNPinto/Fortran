	  program Secante
	  real f0,f1,Delta,x0,x1,x2,xs,e1,e2
	  print*, "Entre com x0 e x1."
	  read*,x0,x1
	  x2=0.
	  xs=0.
	  e1=0.0001
	  e2=0.0001
	  Delta=x1-x0
	  f0= log((1+0.05*x0)/(0.05*x0)) - (x0+1)/(x0*(1+0.05*x0))
	  f1= log((1.+0.05*x1)/(0.05*x1)) - (x1+1.)/(x1*(1.+0.05*x1))
	  if (abs(f0).lt.e1) then 
	  	xs=x0
	  else
	  	if((abs(f1).lt. e1).or.(abs(Delta).lt.e2)) then
	  		xs=x1
	  	else
	  		dowhile((abs(f1).gt. e1).or.(abs(Delta).gt.e2))
				f0= log((1+0.05*x0)/(0.05*x0)) - (x0+1)/(x0*(1+0.05*x0))
	  			f1= log((1.+0.05*x1)/(0.05*x1)) - (x1+1.)/(x1*(1.+0.05*x1))
	  			x2=x1 - f1*Delta/(f1-f0)
	  	    	f1=log((1.+0.05*x2)/(0.05*x2)) - (x2+1.)/(x2*(1.+0.05*x2))
	  	    	Delta=x2-x1
            	x0=x1
            	x1=x2
	  		enddo
	  	xs=x2
	  	endif
	  endif
	  print*,"a solução é ", xs
	  end