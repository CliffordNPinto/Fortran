	program simplex
	integer, parameter :: m1=5000,n1 = 7000
   	real, dimension(0:m1,0:n1) :: a,aa,aaa
	real, dimension(m1,1) :: b,bb,bbb,bi	
	real, dimension(1,n1) :: c,az,aL
	integer, dimension(m1) :: des
	real x,e,menor
	integer y,d,f,n2,l,m2,n,m,p,q,r,r1,r2,l1,s(5)
	logical :: encontrou,var
	print*,"Programa que Maximiza a função-objetiva de um problema 
     ade PL"
	print*,"Versão 0.0"
	print*,"Versão beta ainda na fase de validação!"
	print*,"Desenvolvido pelo Prof. Clifford Neves Pinto"
	print*,"Data: 04/11/2025"
	print*,"Desenvolvido usando gfortran - GNU Fortran compiler."
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
 	y=0
	!Definir as desigualdades nas restrições
	Print*,"Para cada restrição selecione a operação relacional:"
	Print*,"1 para >"
	Print*,"2 para >="
	Print*,"3 para <"
	Print*,"4 para <="
	Print*,"5 para ="
	print*,"Entre com a quantidade de restrições."
	read*,m
	do i=1,m
		Print*,"Qual é a operação relacional para a restrição",i
		read*,des(i)
	enddo
	print*,"Entre com a quantidade de variáveis independentes
     a presentes na função-objetivo"
	read*,n

	do j=1,5
		s(j)=0
	enddo
	do i=1,m
		do j=1,5
			if (des(i)==j) then
				s(j)=s(j)+1
			endif
		enddo
	enddo
	r=s(1)+s(2)!Total de variáveis de excesso
	r2=s(3)+s(4)!Total de variáveis de folga
	n2=n+r+r2+s(5)!Total de variáveis de folga + excesso
	r1=n+2*(s(1)+s(2))+s(3)+s(4)+s(5)!Total de variáveis
	do j=1,5
	print*,j, " tem ", s(j)
	enddo
!Preenchendo as colunas da linha 0, linhas das variáveis, com as variáveis originais
	do j=1,n
	   aa(0,j)=j
	enddo

	do i=1,m
	   do j=1,n
	       print*,"entre com o coef. a(",i,j,")"
	       read*,a(i,j)
	       aa(i,j)=a(i,j)
	   enddo
	enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
!Preenchendo as colunas da linha 0, linhas das variáveis, com as variáveis de folga
	do j=n+1,n+r2
	    aa(0,j)=j
	    print*,"variáveis de folga",aa(0,j)
	enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
!Preenchendo as colunas da linha 0, linhas das variáveis, com as variáveis de excesso
	do j=n+r2+1,n+r2+r
	    aa(0,j)=j
	    print*,"variáveis de excesso",aa(0,j)
	enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
!Preenchendo as colunas da linha 0, linhas das variáveis, com as variáveis artificiais
	if ((s(1).ne.0).or.(s(2).ne.0).or.(s(5).ne.0)) then 
	   do j=n+r2+r+1,r1
	      aa(0,j)=j
	      print*,"variáveis artificiais",aa(0,j)
	   enddo
	endif	
	aa(0,r1+1)=r1+1
!Preencher a matriz aa considerando as variáveis extras

!variáveis de folga
	l=n+1
	do i=1,m
	   if ((des(i) == 3) .or. (des(i) == 4)) then 
			do j=n+1,n+r2
				if (j==l) then 
		       	  		aa(i,j)=1.0
				else
			  		aa(i,j)=0.0
				endif
			enddo	
			l=l+1
	   endif	
	enddo
!variáveis de excesso
 	l=n+r2+1
	do i=1,m	
	   if ((des(i)==1) .or. (des(i)==2)) then 
			do q=n+r2+1,n+r2+r
				if (q==l) then 
			  	  	aa(i,q)=-1.0
				else
				  	aa(i,q)=0.0
				endif
			enddo
			l=l+1
	   endif
	enddo	
!variáveis artificiais
  	l1=n+r2+r+1
	do i=1,m
	   if ((des(i)==1) .or. (des(i)==2)) then 
		do q=n+r2+r+1,r1-s(5)
			if (q==l1) then 
			       	aa(i,q)=1.0
				go to 10
			else
				aa(i,q)=0.0
			endif
		enddo	
10		l1=l1+1	
	   endif
	enddo

  	l1=r1-s(5)+1
	do i=1,m
	   if (des(i)==5) then 
		do q=r1-s(5)+1,r1
			if (q==l1) then 
			       	aa(i,q)=1.0
				go to 20
			else
				aa(i,q)=0.0
			endif
		enddo	
20		l1=l1+1	
	   endif
	enddo

!Preenchendo as linhas da coluna 0, linhas da base, com as variáveis de folga
	do i=1,m
	   if ((n+1)==(n+r2)) then
      	        if (aa(i,n+1)==1) then 
		  	aa(i,0)=aa(0,n+1)
			go to 30
	      	endif
	   else
	   	do k=n+1,n+r2
              	   if (aa(i,k)==1) then 
		        aa(i,0)=aa(0,k)
			go to 30
	      	   endif
	   	enddo	   
30	endif	
	enddo

!Preenchendo as linhas da coluna 0, linhas da base, com as variáveis artificiais
	do i=1,m
   	   if (n+r2+r+1==r1) then
             	if (aa(i,r1)==1) then 
		   aa(i,0)=aa(0,r1)
		   go to 40
	      	endif
	   else
	   	do k=n+r2+r+1,r1
              	   if (aa(i,k)==1) then 
		  	aa(i,0)=aa(0,k)
		   	go to 40
	      	   endif
	   	enddo	   
40	   endif
	enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	print*,"Matriz dos coeficientes das restrições."
	do i=0,m
		write(*,50)(aa(i,j),j=0,r1)
	enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	!Matriz coluna do lado direito das restrições b(nx1)
	print*,"Entre com a matriz dos resultados"
	print*,"Lado direito(LD) das restrições."
        do i=1,m
               print*,"entre com o número do LD de cada restrição."
               read*,b(i,1)
        enddo
	!Preenchendo linha de -W
	if (r1>n+r2+r+1) then
	    y=1
	    do j=n+r2+r+1,r1
		aa(m+1,j)=1
	    enddo
	else
	    if (r1==n+r2+r+1) then
	    	y=1
		aa(m+1,r1)=1
	    endif
	endif
	
	!Matriz linha dos coeficientes da função-objetivo c(1xn)
        print*,"Entre com a matriz dos coef. da função-objetivo."
        do j=1,n
           print*,"entre com o valor do coef. da função-objetivo.."
            read*,c(1,j)
        enddo

	!Juntando matriz dos coeficientes aa(mxn) com a c(1xn)
	do j=1,n
	   aa(m+y+1,j)=-c(1,j)
        enddo

	!Juntanto matriz aa(m+1xn) com a b(nx1)
        do i=1,m
           aa(i,r1+1)=b(i,1)
        enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	!Imprimindo a matriz ampliada
	print*,"Imprimindo a matriz ampliada"
	do i=0,m+y+1
		write(*,50)(aa(i,j),j=0,r1+1)
	enddo

	!Salvando a matriz aa(m+1xn+1) na matriz aaa(m+1,n+1)
	do i=0,m+y+1
	   do j=0,r1+1
	      aaa(i,j)=aa(i,j)
	   enddo
	enddo

	encontrou = .false.	
	if (y==1) then

	    !Eliminando o conflito	

	    do i=1,m
   	       if (n+r2+r+1==r1) then
             	  if (aa(i,r1)==1) then 
			aa(i,0)=aa(0,r1)
	      		do j=1,r1+1
		  		aa(m+1,j)=aa(m+1,j)-aa(i,j)
	      		enddo	
	      	  endif
	       else
	   	  do k=n+r2+r+1,r1
              	     if (aa(i,k)==1) then 
		  	aa(i,0)=aa(0,k)
	      		do j=1,r1+1
		  		aa(m+1,j)=aa(m+1,j)-aa(i,j)
	      		enddo
	      	     endif
	   	  enddo	   
   	       endif
	    enddo
	    print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	    print*,"Imprimindo a matriz ampliada com -W e sem conflito"
	    do i=0,m+y+1
		write(*,50)(aa(i,j),j=0,r1+1)
	    enddo

	    do j=1,r1
		aaa(m+1,j)=aa(m+1,j)
	    enddo

	    encontrou=.false.
	    do j=1,r1
		if (aaa(m+1,j)<0.0) then
			encontrou=.true.
			go to 60
		endif
60	    enddo

	    !Início do escalonamento de -W
	    do while (encontrou.eqv..true.)

	       !Determinando o menor valor da (m+1)-ésima linha da matriz aa(m+1xr1)
	       do j=1,r1-1
	          if (aa(m+1,j)<=aa(m+1,j+1)) then
		      e=aa(m+1,j+1)
		      aa(m+1,j+1)= aa(m+1,j)
		      aa(m+1,j)=e
	          endif
	       enddo

	       !Determinando a coluna da variável da base
	       do j=1,r1
	          if (aaa(m+1,j)==aa(m+1,r1)) then 
		      d=j
	          endif
	       enddo
	       print*, "Variável básica na coluna",d

	       !Reobtendo a matriz aa(m+1xj)
               do j=1,r1
                  aa(m+1,j)=aaa(m+1,j)
               enddo

	       !Determinando a variável não-básica
   	       do i = 1, m
		   if (aa(i,d)<=0.0) then 
			bb(i,1)=-1
		   else
			bb(i,1)=b(i,1)/aa(i,d)
		   endif
	       enddo
   	       !Inicialização
   	       menor = huge(menor)  ! Inicializa com o maior valor possível

   	       !Busca única pelo menor valor não negativo
   	       do i = 1, m
       		   if (bb(i,1) >= 0.0 .and. bb(i,1) < menor) then
            		menor = bb(i,1)
           		f = i
         	   end if
   	       end do

	       print*,"Imprimindo matriz b/x"
               do i=1,m
                   write(*,*)bb(i,1)
               enddo 
	       print*, "Variável não-básica na linha",f

	       !Pivô e escalonamento
	       if (aa(f,d)==1.) then
	           do i=1,m+1
	              if (i.ne.f) then
		         x=aa(i,d)
	                 do j=1,r1+1
		            aa(i,j)=aa(i,j)-x*aa(f,j)
		         enddo
		      endif
	           enddo
	       else
	           j=1
                   do while ((1<=j).and.(j<=r1+1))
		       x=aa(f,j)
	               x=x/aa(f,d)
		       aL(f,j)=x
		       j=j+1
	           end do
	           j=1
                   do while ((1<=j).and.(j<=r1+1))
                      aa(f,j)=aL(f,j)
	              j=j+1
                   enddo
	           do i=1,m+1
	              if (i.ne.f) then
		         x=aa(i,d)
		         do j=1,r1+1
	                    aa(i,j)=aa(i,j)-x*aa(f,j)
	                 enddo
	              endif
	           enddo
	       endif
	       aa(f,0)=aa(0,d)
	       print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	       print*,"Matriz ampliada otimizada de -w"
               do i=0,m+1
                  write(*,50)(aa(i,j),j=0,r1+1)
               enddo
               do j=1,r1+1
                   aaa(m+1,j)=aa(m+1,j)
               enddo
	       do i=1,m
		   b(i,1)=aa(i,r1+1)
	       enddo
	       encontrou = .false.
	       do j=1,r1
		  if (aaa(m+1,j)<0.0) then
			encontrou=.true.
			go to 70
		  endif
70	       enddo
	    enddo

	    !Verificando se existe solução a partir da Matriz ampliada otimizada de -w
	    if (n2+1==r1) then
	       var=.true.
	       do i=1,m
	          if (aa(i,0)==aa(0,r1)) then
		     if (aa(i,r1+1).ne.0.0) then
		        print*,"Solução indeterminada!"
		        var=.false.
		     endif
	          endif
	       enddo

	       if (var.eqv..true.) then
		  print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
     	          print*,"Valores das variáveis originais:"
		      do i=1,m
		           do j= 1,n
			      if (aa(i,0)==aa(0,j)) then
				print*,"   Variáveis--------------valor"
				print*,aa(i,0)," = ", aa(i,r1+1)
			      endif
		           enddo
		  enddo
		  print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		  print*,"Valores de folga/excesso:"
		  do i=1,m
		           do j= n+1,r1
			      if (aa(i,0)==aa(0,j)) then
				k=j
				print*,"   Variáveis--------------valor"
			        print*, aa(i,0)," = ", aa(i,r1+1)
			      endif
		     	   enddo
		  enddo
		  print*, "Max -W = ", aa(m+1,r1+1)

		  do i=1,m+1
			    aa(i,r1)=aa(i,r1+1)
		  enddo
		  do j=1,r1
			   aa(m+1,j)=aa(m+2,j)
		  enddo

		  print*,"Matriz ampliada nova de Z"
        	  do i=0,m+1
           			write(*,50)(aa(i,j),j=0,r1)
        	  enddo		
		  do j=1,r1
              		    aaa(m+1,j)=aa(m+1,j)
        	  enddo

		  encontrou=.false.
		  do j=1,r1-1
			   if (aaa(m+1,j)<0.0) then
				encontrou=.true.
				go to 200
			   endif
200		  enddo

		  !Início do escalonamento de Z
		  do while (encontrou.eqv..true.)
	
		  	   !Determinando o menor valor da (m+1)-ésima linha da matriz aa(m+1xr1)
		  	   do j=1,r1-1
	   		      if (aa(m+1,j)<=aa(m+1,j+1)) then
				e=aa(m+1,j+1)
				aa(m+1,j+1)= aa(m+1,j)
				aa(m+1,j)=e
	   		      endif
		  	   enddo

		  	   !Determinando a coluna da variável da base
		  	   do j=1,r1
	   		      if (aaa(m+1,j)==aa(m+1,r1)) then 
				d=j
	   		      endif
		  	   enddo
		  	   print*, "Variável básica na coluna",d

		  	   !Reobtendo a matriz aa(m+1xj)
        	  	   do j=1,r1
			      aa(m+1,j)=aaa(m+1,j)
        	  	   enddo

		  	   !Determinando a variável não-básica
   		  	   do i = 1, m
			      if (aa(i,d)<=0.0) then 
				bb(i,1)=-1
			      else
				bb(i,1)=b(i,1)/aa(i,d)
			      endif
		  	   enddo
   	 	  	   !Inicialização
   		  	   menor = huge(menor)  ! Inicializa com o maior valor possível

   		  	   !Busca única pelo menor valor não negativo
   		  	   do i = 1, m
       			      if (bb(i,1)>=0.0.and.bb(i,1) < menor) then
            			menor = bb(i,1)
           			f = i
         		      end if
   		  	   end do

		  	   print*,"Imprimindo matriz b/x"
        		   do i=1,m
            			write(*,*)bb(i,1)
        		   enddo 
		  	   print*, "Variável não-básica na linha",f

		  	   !Pivô e escalonamento
		  	   if (aa(f,d)==1.) then
	    		      do i=1,m+1
	        		if (i.ne.f) then
		     			x=aa(i,d)
	             			do j=1,r1+1
		       			   aa(i,j)=aa(i,j)-x*aa(f,j)
		     			enddo
				endif
	    		      enddo
		  	   else
	    		      j=1
            		      do while ((1<=j).and.(j<=r1))
				x=aa(f,j)
	        		x=x/aa(f,d)
				aL(f,j)=x
				j=j+1
	    		      end do
	    		      j=1
            		      do while ((1<=j).and.(j<=r1))
              			aa(f,j)=aL(f,j)
	      			j=j+1
            		      enddo
	    		      do i=1,m+1
	       			if (i.ne.f) then
		  			x=aa(i,d)
		  			do j=1,r1
	            			aa(i,j)=aa(i,j)-x*aa(f,j)
	          			enddo
	       			endif
	    		      enddo
		  	   endif
		  	   aa(f,0)=aa(0,d)
		  	   print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		  	   print*,"Matriz ampliada otimizada de Z"
        	  	   do i=0,m+1
           		      write(*,50)(aa(i,j),j=0,r1)
        	  	   enddo
        	  	   do j=1,r1
              		      aaa(m+1,j)=aa(m+1,j)
        	  	   enddo
		  	   do i=1,m
			      b(i,1)=aa(i,r1)
		  	   enddo
		  	   encontrou = .false.
		  	   do j=1,r1-1
			      if (aa(m+1,j)<0.0) then
				encontrou=.true.
				go to 210
			      endif
210		  	   enddo
		      enddo
		      print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		      print*,"Valores das variáveis originais:"
		      do i=1,m
			do j= 1,n
				if (aa(i,0)==aa(0,j)) then
				    print*,"   Variáveis--------------valor"
				    print*,aa(i,0)," = ", aa(i,r1)
				endif
			enddo
		      enddo
		      print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		      print*,"Valores de folga/excesso:"
		      do i=1,m
		         do j= n+1,r1
			    if (aa(i,0)==aa(0,j)) then
				print*,"   Variáveis--------------valor"			
			        print*, aa(i,0)," = ", aa(i,r1)
			    endif
		         enddo
		      enddo
		      print*, "Max Z = ", aa(m+1,r1)
	       endif
	    endif

	    if (n2+1<r1) then
	       var=.true.
	       do i=1,m
	         do j=n2+1,r1
	          if (aa(i,0)==aa(0,j)) then
		     if (aa(i,r1+1).ne.0.0) then
		  	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		        print*,"Solução indeterminada!"
		        var=.false.
		     endif
	          endif
	         enddo
	       enddo

	       if (var.eqv..true.) then
		  print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	  	  print*,"Valores das variáveis originais:"
		  do i=1,m
		     do j= 1,n
			if (aa(i,0)==aa(0,j)) then
				print*,"   Variáveis--------------valor"
				print*,aa(i,0)," = ", aa(i,r1+1)
			endif
		     enddo
		  enddo
		  print*,"Valores de folga/excesso:"
		  do i=1,m
		     do j= n+1,r1
			if (aa(i,0)==aa(0,j)) then
				print*,"   Variáveis--------------valor"
			        print*, aa(i,0)," = ", aa(i,r1+1)
			endif
		     enddo
		  enddo
		  print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		  print*, "Max -W = ", aa(m+1,r1+1)

		  do i=1,m+1
			aa(i,n2+1)=aa(i,r1+1)
		  enddo
		  do j=1,n2+1
			aa(m+1,j)=aa(m+2,j)
		  enddo
		  print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		  print*,"Matriz ampliada nova de Z"
        		do i=0,m+1
           			write(*,50)(aa(i,j),j=0,n2+1)
        		enddo		
		  do j=1,n2
              		aaa(m+1,j)=aa(m+1,j)
        	  enddo

		  encontrou=.false.
		  do j=1,n2
			if (aaa(m+1,j)<0.0) then
				encontrou=.true.
				go to 100
			endif
100		  enddo

		  !Início do escalonamento de Z
		  do while (encontrou.eqv..true.)
	
		  	!Determinando o menor valor da (m+1)-ésima linha da matriz aa(m+1xr1)
		  	do j=1,n2-1
	   			if (aa(m+1,j)<=aa(m+1,j+1)) then
				    e=aa(m+1,j+1)
				    aa(m+1,j+1)= aa(m+1,j)
				    aa(m+1,j)=e
	   			endif
		  	enddo

		  	!Determinando a coluna da variável da base
		  	do j=1,n2
	   			if (aaa(m+1,j)==aa(m+1,n2)) then 
				    d=j
	   			endif
		  	enddo
		  	print*, "Variável básica na coluna",d

		  	!Reobtendo a matriz aa(m+1xj)
        	  	do j=1,n2
			    aa(m+1,j)=aaa(m+1,j)
        	  	enddo

		  	!Determinando a variável não-básica
   		  	do i = 1, m
				if (aa(i,d)<=0.0) then 
				    bb(i,1)=-1
				else
				    bb(i,1)=b(i,1)/aa(i,d)
				endif
		  	enddo
   	 	  	!Inicialização
   		  	menor = huge(menor)  ! Inicializa com o maior valor possível

   		  	!Busca única pelo menor valor não negativo
   		  	do i = 1, m
       			     if (bb(i,1)>=0.0.and.bb(i,1)<menor) then
            			   menor = bb(i,1)
           			   f = i
         		     end if
   		  	end do

		  	print*,"Imprimindo matriz b/x"
        		do i=1,m
            			write(*,*)bb(i,1)
        		enddo 
		  	print*, "Variável não-básica na linha",f

		  	!Pivô e escalonamento
		 	if (aa(f,d)==1.) then
	    			do i=1,m+1
	        			if (i.ne.f) then
		     			   x=aa(i,d)
	             			   do j=1,n2+1
		       			      aa(i,j)=aa(i,j)-x*aa(f,j)
		     			   enddo
					endif
	    			enddo
		  	else
	    			j=1
            			do while ((1<=j).and.(j<=n2+1))
					x=aa(f,j)
	        			x=x/aa(f,d)
					aL(f,j)=x
					j=j+1
	    			end do
	    			j=1
            			do while ((1<=j).and.(j<=n2+1))
              				aa(f,j)=aL(f,j)
	      				j=j+1
            			enddo
	    			do i=1,m+1
	       				if (i.ne.f) then
		  			   x=aa(i,d)
		  			   do j=1,n2+1
	            			      aa(i,j)=aa(i,j)-x*aa(f,j)
	          			   enddo
	       				endif
	    			enddo
		  	endif
		  	aa(f,0)=aa(0,d)
		  	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		  	print*,"Matriz ampliada otimizada de Z"
        	  	do i=0,m+1
           			write(*,50)(aa(i,j),j=0,n2+1)
        	  	enddo
        	  	do j=1,r1+1
              			aaa(m+1,j)=aa(m+1,j)
        	  	enddo
		  	do i=1,m
				b(i,1)=aa(i,n2+1)
		  	enddo
		  	encontrou = .false.
		  	do j=1,n2
				if (aa(m+1,j)<0.0) then
					encontrou=.true.
					go to 110
				endif
110		  	enddo
		  enddo
		  print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		  print*,"Valores das variáveis originais:"
		  do i=1,m
			do j= 1,n
				if (aa(i,0)==aa(0,j)) then
				    print*,"   Variáveis--------------valor"
				    print*,aa(i,0)," = ", aa(i,n2+1)
				endif
			enddo
		  enddo
		  print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		  print*,"Valores de folga/excesso:"
		  do i=1,m
		     do j= n+1,r1
			if (aa(i,0)==aa(0,j)) then
				print*,"   Variáveis--------------valor"
			        print*, aa(i,0)," = ", aa(i,n2+1)
			endif
		     enddo
		  enddo
		  print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		  print*, "Max Z = ", aa(m+1,n2+1)
	       endif
	    endif	    
	else

	    do j=1,r1
		if (aaa(m+1,j)<0.0) then
			encontrou=.true.
			go to 90
		endif
90	    enddo

	!Início do escalonamento de Z
	do while (encontrou.eqv..true.)
	
	!Determinando o menor valor da (m+1)-ésima linha da matriz aa(m+1xr1)
	do j=1,r1-1
	   if (aa(m+1,j)<=aa(m+1,j+1)) then
		e=aa(m+1,j+1)
		aa(m+1,j+1)= aa(m+1,j)
		aa(m+1,j)=e
	   endif
	enddo

	!Determinando a coluna da variável da base
	do j=1,r1
	   if (aaa(m+1,j)==aa(m+1,r1)) then 
		d=j
	   endif
	enddo
	print*, "Variável básica na coluna",d

	!Reobtendo a matriz aa(m+1xj)
        do j=1,r1
              aa(m+1,j)=aaa(m+1,j)
        enddo

	!Determinando a variável não-básica
   	do i = 1, m
		if (aa(i,d)<=0.0) then 
			bb(i,1)=-1
		else
			bb(i,1)=b(i,1)/aa(i,d)
		endif
	enddo
   	 ! Inicialização
   	menor = huge(menor)  ! Inicializa com o maior valor possível

   	! Busca única pelo menor valor não negativo
   	do i = 1, m
       		if (bb(i,1) >= 0.0 .and. bb(i,1) < menor) then
            		menor = bb(i,1)
           		f = i
         	end if
   	end do

	print*,"Imprimindo matriz b/x"
        do i=1,m
            write(*,*)bb(i,1)
        enddo 
	print*, "Variável não-básica na linha",f

	!Pivô e escalonamento
	if (aa(f,d)==1.) then
	    do i=1,m+1
	        if (i.ne.f) then
		     x=aa(i,d)
	             do j=1,r1+1
		       aa(i,j)=aa(i,j)-x*aa(f,j)
		     enddo
		endif
	    enddo
	else
	    j=1
            do while ((1<=j).and.(j<=r1+1))
		x=aa(f,j)
	        x=x/aa(f,d)
		aL(f,j)=x
		j=j+1
	    end do
	    j=1
            do while ((1<=j).and.(j<=r1+1))
              aa(f,j)=aL(f,j)
	      j=j+1
            enddo
	    do i=1,m+1
	       if (i.ne.f) then
		  x=aa(i,d)
		  do j=1,r1+1
	            aa(i,j)=aa(i,j)-x*aa(f,j)
	          enddo
	       endif
	    enddo
	endif
	aa(f,0)=aa(0,d)
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	print*,"Matriz nova de Z"
        do i=0,m+1
           write(*,50)(aa(i,j),j=0,r1+1)
        enddo
        do j=1,r1+1
              aaa(m+1,j)=aa(m+1,j)
        enddo
	do i=1,m
		b(i,1)=aa(i,r1+1)
	enddo
	encontrou = .false.
	do j=1,r1
		if (aa(m+1,j)<0.0) then
			encontrou=.true.
			go to 80
		endif
80	enddo
	enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	print*,"Valores das variáveis originais:"
	do i=1,m
		do j= 1,n
			if (aa(i,0)==aa(0,j)) then
				print*,"   Variáveis--------------valor"
				print*,aa(i,0)," = ", aa(i,r1+1)
			endif
		enddo
	enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	print*,"Valores de folga/excesso:"
	do i=1,m
		do j= n+1,r1
			if (aa(i,0)==aa(0,j)) then
				print*,"   Variáveis--------------valor"
			        print*, aa(i,0)," = ", aa(i,r1+1)
			endif
		enddo
	enddo
	print*,"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
	print*, "Max Z = ", aa(m+1,r1+1)

	endif
50	format(f5.1,1x,f5.1,1x,f5.1,1x,f5.1,1x,f5.1,1x,f5.1,1x,f5.1,1x,
     af5.1,1x,f5.1,1x,f5.1,1x,f5.1)
	end

