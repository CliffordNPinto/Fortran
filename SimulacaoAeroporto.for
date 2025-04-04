	  program SimulacaoMonteCarloAeroporto
	  Integer QAR,QSOLO,NA, ND,IND, MD,MA
      real T,rA,rD,DeltaT,TS,TA,TD,PA,PD,MQAR,MQSOLO
      real T0,MTIND0,MTP,TP,TIND0
	  !Atribuir valores iniciais para as variáveis
	  TP= 0.
	  TIND0= 0.
	  QAR = 0
	  NA = 0
	  QSOLO = 0
	  ND=0
	  DeltaT=1.
	  TA= 3.
	  TD=2.
	  MD= 10.
	  MA= 10.
	  PA= MA/60.
	  PD= MD/60.
	  print*,'Qual é o instante de tempo inicial e o intervalo de tempo
     a de duração da simulação?'
	  read*,T0,TS
	  print*,'Quantas aeronaves se encontram no ar esperando para
     aaterrissar e quantas se encontram no solo esperando para
     adecolar no instante de tempo inicial?'
	  read*,QAR,QSOLO
	  print*,'Dado que:IND = 0: pista livre; IND=1: pista ocupada por
     aaeronave em aterrissagem; IND=2: pista ocupada por aeronave em
     adecolagem, entre com o status da pista noinstante inicial.'
	  read*,IND
	  T = T0
	  dowhile(T.le. TS)
	  	if (IND .eq. 0) then
	  		!Gerar número aleatório para aterrissagem
	  		rA =rand()
	  		if(rA .lt. PA) then
	  			QAR = QAR - 1
	  			NA = NA + 1
	  			QSOLO= QSOLO + 1
	  			T = T + TA
	  			TP = TP + TA
	  		else
	  			!Gerar número aleatório para decolagem
	  			rD = rand()
	
		  			if (rD .lt. PD) then
	  				QSOLO = QSOLO - 1
	  				ND = ND+1
	  				QAR = QAR + 1
	  				T = T + TD
	  				TP = TP + TD
	  				print*,'solo',qsolo
	  			endif
	  			TIND0=TIND0+DeltaT
	  			print*,'Em T=', T, ' ',NA,' aeronaves aterrissaram ',
     aND,' decolaram', QSOLO, ' estão no solo e ', QAR, 
     a' estão aguardando autorização para pouso.'
	        endif
	 	else
	 		if (IND .eq. 1) then
	 			!Gerar número aleatório para aterrissagem
	 			rA =rand()
	 			if (rA .lt.PA) then
	 				QAR = QAR - 1
	 				NA = NA + 1
	 				QSOLO= QSOLO + 1
	 				T = T + TA
	 				TP = TP + TA
	 				IND = 0
	 			endif
	 			print*,'Em T=', T, ' ',NA,' aeronaves aterrissaram ',
     aND,' decolaram', QSOLO,' estão no solo e ', QAR, ' 
     aestão aguardando autorização para pouso.'
			else
				!Gerar número aleatório para decolagem
				rD =rand()
				if(rD.lt.PD) then
					QSOLO = QSOLO - 1
					ND = ND+1
					T=T+TD
					TP = TP + TD
					IND = 0
				endif
				print*,'Em T=', T, ' ',NA,' aeronaves aterrissaram ',
     aND,'decolaram', QSOLO,' estão no solo e ', QAR, ' estão 
     aaguardando autorização para pouso.'
	 		endif
	 	endif
	 	T = T+DeltaT
	 	enddo
	 	!Cálculo dos parâmetros do sistema
	 	print*,'Parâmetros do sistema'
	 	MQAR=QAR/TS
	 	MQSOLO=QSOLO/TS
	 	MTIND0= TIND0/TS
	 	MTP= TP/TS
	 	print*,'Média de aeronaves esperando para pousar:',MQAR
	 	print*,'Média de aeronaves esperando para decolar:',MQSOLO
	 	print*,'Média de tempo livre da pista:',MTIND0
	 	print*,'Média de tempo de uso da pista:',MTP
	 	end