        program NewtonRaphson
	real f,df,Delta,x0,x1,xs,e1,e2
	print*, "Entre com x0"
	read*,x0
	i=0
	x1=0.
	xs=0.
	e1=0.0001
	e2=0.0001
	Delta=x1-x0
	f= x0**3-9*x0+3
	if (abs(f).lt.e1) then 
	  xs=x0
	else
	  do while((abs(f).gt. e1).or.(abs(Delta).gt.e2))
	     f= x0**3-9*x0+3
	     df= 3*x0**2-9
	     x1=x0 - f/df
	     f= x1**3-9*x1+3
	     Delta=x1-x0
             x0=x1
	     i=i+1
	 enddo
	 xs=x0
	endif
	print*,"a solução é ", xs,"Número de iterações: ", i
	end
