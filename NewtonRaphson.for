	  program NewtonRaphson
	  real pi,f,df,Delta,v0,theta,v0x,v0y,g,x0,x1,xs,e1,e2
	  pi=3.14159
	  print*, "Entre com v0, theta, x0 e g."
	  read*,v0,theta,x0,g
	  theta=theta*pi/180.
	  v0x=v0*cos(theta)
	  v0y=v0*sin(theta)
	  print*,v0x,v0y
	  x1=0.
	  xs=0.
	  e1=0.0001
	  e2=0.0001
	  Delta=x1-x0
	  f= v0y*x0/v0x-g*x0**2/(2*v0x**2)
	  if (abs(f).lt.e1) then 
	  	xs=x0
	  else
	  	dowhile((abs(f).gt. e1).or.(abs(Delta).gt.e2))
	  		f=v0y*x0/v0x-g*x0**2/(2*v0x**2)
	  		df= v0y/v0x-g*x0/(v0x**2)
	  		x1=x0 - f/df
	  	    f=v0y*x1/v0x-g*x1**2/(2*v0x**2)
	  	    Delta=x1-x0
            x0=x1
	  	enddo
	  	xs=x0
	  endif
	  print*,"a solução é ", xs
	  end