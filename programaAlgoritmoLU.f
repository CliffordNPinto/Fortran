      program fatoracaolu
      integer i,j,k,r,n
      real a(10,10),p(10,1),x(10,1),y(10,1),c(10,1),b(10,1)
      real pv,aux,m,soma
      !Inseri ordem da matriz
      print*,"Entre com a ordem da matriz dos coeficientes."
      read*,n
      !Inseri matriz dos coeficientes
      do i=1,n
	       do j=1,n
	           print*,"Entre com os elementos de matriz da matriz dos
     scoeficientes."
	           read*,a(i,j)
	       enddo
      enddo
      !Inseri matriz dos resultados
      do i=1,n
	      print*,"Entre com os elementos de matriz da matriz
     a   resultado."
	      read*,b(i,1)
      enddo
      !Calculo dos fatores
      i = 1
      dowhile (i.le.n)
        p(i,1) = i
        i = 1+i
      enddo
      k = 1
      dowhile (k.lt.n)
        pv = abs(a(k,k))
        r = k
        i = k + 1
        dowhile (i.le.n)
          if (abs(a(i,k)).gt.pv) then
            pv = abs(a(i,k))
            r = i
          endif
        i = i +1
       enddo
       if (pv.eq.0) then
         print*, "A matriz ‚ singular"
         Goto 76
       else
        if (r.ne.k) then
          aux = p(k,1)
          p(k,1) = p(r,1)
          p(r,1) = aux
          j = 1
          dowhile (j.le.n)
            aux = a(k,j)
            a(k,j) = a(r,j)
            a(r,j) = aux
            j = j + 1
          enddo
        endif
        i = k + 1
        dowhile (i.le.n)
          m = a(i,k)/a(k,k)
          a(i,k) = m
          j = k + 1
          dowhile (j.le.n)
            a(i,j) = a(i,j) - m*a(k,j)
            j = j + 1
          enddo
          i = i + 1
        enddo
       endif
       k=K+1
      enddo
      i = 1
      !Resolução dos sistem,as triangulares
      dowhile (i.le.n)
        r = p(i,1)
        c(i,1) = b(r,1)
        i = i + 1
      enddo
      i = 1
      dowhile (i.le.n)
        soma = 0
        j = 1
        dowhile (j.lt.i)
          soma = soma + a(i,j)*y(j,1)
          j = j + 1
        enddo
        y(i,1)=c(i,1)-soma
        i = i + 1
      enddo
      i = n
      dowhile (i.ge.1)
          soma = 0.
          j = i + 1
          dowhile (j.le.n)
            soma = soma + a(i,j)*x(j,1)
            j = 1 + j
          enddo
          x(i,1) = (y(i,1)-soma)/a(i,i)
          i = i - 1
      enddo
      print*,"A solução é:"
      do i=1,n
          write(*,*)"x",i," = ",x(i,1)
      enddo
      !pause
76    end
         
