      program EliminacaoGauss
      real a(10,10),b(10,1),x(10,1),m,s
      integer i,j,k,n
      print*,"Entre com a ordem da matriz dos coeficientes."
      read*,n
      do i=1,n
	  	do j=1,n
	  		print*,"Entre com os elementos de matriz da matriz
     d dos coeficientes."
	  		read*,a(i,j)
	  	enddo
	  enddo
	  do i=1,n
	  		print*,"Entre com os elementos de matriz da matriz
     aresultado."
	  		read*,b(i,1)
	  enddo
	  !Eliminação
	  k=1
	  dowhile(k.lt.n)
	  	i=k+1
	  	dowhile(i.le.n)
	  		m= a(i,k)/a(k,k)
	  		a(i,k)=0.
	  		j=k+1
	  		dowhile(j.le.n)
	  			a(i,j)=a(i,j)-m*a(k,j)
	  			j=j+1
	  		enddo
	  		b(i,1)=b(i,1) -m*b(k,1)
	  		i=i+1
	  	enddo
	  	k=k+1
	  enddo	
	  do i=1,n
	  	do j=1,n
	  		print*, a(i,j)
	  	enddo
	  enddo
	  !Resolução 	
	  	x(n,1)=b(n,1)/a(n,n)
	  	k=n-1
	  	dowhile(k.ge.1)
	  		s=0.
	  		j=k+1
	  		dowhile(j.le.n)
	  			s=s+a(k,j)*x(j,1)
	  			j=j+1
	  		enddo
	  		x(k,1)=(b(k,1)-s)/a(k,k)
	  		k=k-1
	  	enddo

	do i=1,n
		print*,x(i,1)
	enddo	  	
	end
