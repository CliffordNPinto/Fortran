	  program IntegracaoMonteCarlo
	  real area,AT,Y,m,a,b,rx,ry,r1,r2
	  integer N,NS,i
	  a=0
	  b=1
	  N=100000000
	  NS=0
	  m=1
	  AT=(b-a)*m
	  i=0
	  dowhile(i.le.N)
	  	r1=rand()
	  	r2=rand()
	  	rx=(b-a)*r1+a
	  	ry=m*r2
	  	Y=sqrt(1-(rx)**2) !Função a ser integrada
	  	if(ry.lt.Y) then
	  		NS=NS+1
	  	endif
	  	i=i+1
	  enddo
	  area=AT*NS/N
	  print*, "A área é: ", area
	  end
