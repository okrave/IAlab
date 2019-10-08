/*applicabile(Az,s) true se l'azione az è applicabile in un certo stato scalar*/

applicabile(est,pos(Riga,Colonna)) :-
	num_colonne(Nc), %Andra a matchare con 10, quindi Nc = 10
	Colonna<Nc,
	ColonnaAccanto is Colonna+1,
	\+occupata(pos(Riga,ColonnaAccanto)).
	
applicabile(ovest,pos(Riga,Colonna)) :-		
	Colonna>1,
	ColonnaAccanto is Colonna-1,
	\+occupata(pos(Riga,ColonnaAccanto)).
	
applicabile(nord,pos(Riga,Colonna)):-
	Riga>1,
	RigaAccanto is Riga-1,
	\+occupata(pos(RigaAccanto,Colonna)).

applicabile(sud,pos(Riga,Colonna)) :-
	num_righe(Nr),
	Riga<Nr,
	RigaAccanto is Riga+1,
	\+occupata(pos(RigaAccanto,Colonna)).
	
trasforma(est,pos(Riga,Colonna),pos(Riga,ColonnaAccanto)):-
	ColonnaAccanto is Colonna+1.

trasforma(ovest,pos(Riga,Colonna),pos(Riga,ColonnaAccanto)):-
	ColonnaAccanto is Colonna-1.
	
trasforma(nord,pos(Riga,Colonna),pos(RigaAccanto,Colonna)):-
	RigaAccanto is Riga-1.
	
trasforma(sud,pos(Riga,Colonna),pos(RigaAccanto,Colonna)):-
	RigaAccanto is Riga+1.
	