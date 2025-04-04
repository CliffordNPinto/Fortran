      program FatoracaoLU
      real a(10,10),b(10,1),y(10,1),p(10,1),c(10,1),x(10,1)
      real pv,aux,m,soma,s
      integer i,j,k,r,n
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

      do i=1,n
      	p(i,1)=i
      enddo

      do k=1,n-1
      	pv=abs(a(k,k))
      	r=k
      	do i=k+1,n
      		if(abs(a(i,k)).gt. pv) then
      			pv=a(i,k)
      			r=i
      		endif
      	enddo

      	if(pv.eq.0.) then
      		print*,"A matriz é singular."
      		goto 10
      	else
      		if(r.ne.k)then
      			aux=p(k,1)
      			p(k,1)=p(r,1)
      			p(r,1)=aux
      			do j=1,n
      				aux = a(k,j)
      				a(k,j)=a(r,j)
      				a(r,j)=aux
      			enddo
      		endif
      		do i=k+1,n
      			m=a(i,k)/a(k,k)
      			a(i,k)=m
      			do j=k+1,n
      				a(i,j)=a(i,j)-m*a(k,j)
      			enddo
      		enddo
      	endif
      enddo
      do i=1,n
      	r=p(i,1)
      	c(i,1)=b(r,1)
      enddo

      do i=1,n
      	soma=0.
      	do j=1,i-1
      		soma=soma+a(i,j)*y(j,1)
      	enddo
      	y(i,1)=c(i,1)-soma
      enddo
      i=n
      dowhile(i.ge.1)
            soma=0.
            do j=i+1,n
                  soma=soma+a(i,j)*x(j,1)
            enddo
            x(i,1)=(y(i,1)-soma)/a(i,i)
            i=i-1
      enddo
      print*,"A solução é:"
      do i=1,n
            write(*,*)"x",i," = ",x(i,1)
      enddo      
10    end

      
