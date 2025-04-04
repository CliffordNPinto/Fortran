	program metodoFalsaposicao
	real x,x1,x2,xr,ea1,es,fx1,fx2,fxr,teste,n,j1,j2,pi,R,V
	pi=3.14159265
	j1=0
	j2=0
	i=0
	n=0
	es=0.0001
	print*,"Entre com o volume do tanque e o raio."
	read*,V,R
	print*,"Entre com os limites,inferior e superior, do intervalo 
     ada solução."
	read*,x1,x2
	fx1=x1**3-3*R*x1**2+3*V/pi
	fx2=x2**3-3*R*x2**2+3*V/pi
	teste=fx1*fx2
	if (teste<0) then
		xr=x2+fx2*(x2-x1)/(fx1-fx2)
		ea1=abs((x1-xr)/x1)
		ea2=abs((x2-xr)/x2)
		if ((ea1<es) .or. (ea2<es)) then
			x=xr
			print*,"A raíz é ",x, n
		else
			Do while ((ea1>es) .and. (ea2>es))
				n=n+1
				fxr=xr**3-3*R*xr**2+3*v/pi
				fx1=x1**3-3*R*x1**2+3*v/pi
				teste = fx1*fxr
				if (teste < 0) then
					j1=j1+1
					if (j1>=2) Then !Modificação no programa Método de Posição Falsa para evitar quant. de iterações muito grande
						fx1=fx1/4
						j1=0
					endif
					x2=xr
					fx2=x2**3-3*R*x2**2+3*v/pi
					ea1=abs((x1-x2)/x1)
					xr=x2+fx2*(x2-x1)/(fx1-fx2)
					print*,x1,fx1,ea1,"--",x2,fx2,ea2
				else
					j2=j2+1
					if (j2>=2) Then!Modificação no programa Método de Posição Falsa para evitar quant. de iterações muito grande
						fx2=fx2/4
						j2=0
					endif
					x1=xr
					fx1=x1**3-3*R*x1**2+3*v/pi
					ea2=abs((x1-x2)/x2)
					xr=x2+fx2*(x2-x1)/(fx1-fx2)
					print*,x1,fx1,ea1,"--",x2,fx2,ea2
				endif
			enddo
			x=xr
			print*,"A raíz é ",x, "Número de iterações: ", n
		endif
	else
		print*,"Necessário redefinir o intervalo no qual a raíz
     ase encontra."
	endif
	end
