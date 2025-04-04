	program metodoBissecao
	real x,x1,x2,xm,ea1,ea2,es,fx1,fx2,fxm,teste,n
	i=0
	n=0
	es=0.0001
	print*,"Entre com os limites,inferior e superior, do intervalo 
     ada solução."
	read*,x1,x2
	fx1=x1**3-9*x1+3
	fx2=x2**3-9*x2+3
	teste=fx1*fx2
	if (teste<0) then
		xm=(x1+x2)/2.
		ea1=abs((x1-xm)/x1)
		ea2=abs((x2-xm)/x2)
		if ((ea1<es) .or. (ea2<es)) then
			x=xm
			print*,"A raíz é ",x,n
		else
			Do while ((ea1>es) .and. (ea2>es))
				n=n+1
				fxm=xm**3.-9.*xm+3.
				fx1=x1**3-9*x1+3				
				teste = fx1*fxm
				if (teste < 0) then
					x2=xm
					ea1=abs((x1-x2)/x1)
					xm=(x1+x2)/2.
					print*,x1,fx1,ea1,"--",x2,fx2,ea2
				else
					x1=xm
					ea2=abs((x1-x2)/x2)
					xm=(x1+x2)/2.
					print*,x1,fx1,ea1,"--",x2,fx2,ea2
				endif
			enddo
			x=xm
			print*,"A raíz é ",x, "Número de iterações: ", n
		endif
	else
		print*,"Necessário redefinir o intervalo no qual a raíz
     ase encontra."
	endif
	end
	
