bfs(Soluzione):-   
    iniziale(S),
	writeln(["Statp iniziale:",S]),
	open('ListaApplicabili.txt',write, Stream1),
    writeln(Stream1,""),
	close(Stream1),
    bfs_aux([nodo(S,[])],[],Soluzione),
	writeln(["Soluzione finale: ",Soluzione]),
	open('file.txt',write, Stream),	
    write(Stream,Soluzione),  
	close(Stream).

% bfs_aux(Coda,Visitati,Soluzione)
% Coda = [nodo(S,Azioni)|...]
bfs_aux([nodo(S,Azioni)|_],_,Azioni):-
	finale(S) -> writeln("Stato finale trovato"),!.
	
bfs_aux([nodo(S,Azioni)|Tail],Visitati,Soluzione):-
    findall(Azione,applicabile(Azione,S),ListaApplicabili),
	open('ListaApplicabili.txt',append,Stream),
    writeln(Stream,["ListaApplicabili stato: ",S," Sono:",ListaApplicabili]),  
	close(Stream),
    generaFigli(nodo(S,Azioni),ListaApplicabili,[S|Visitati],ListaFigli),
    append(Tail,ListaFigli,NuovaCoda),
    bfs_aux(NuovaCoda,[S|Visitati],Soluzione).

generaFigli(_,[],_,[]).

generaFigli(nodo(S,AzioniPerS),[Azione|AltreAzioni],Visitati,[nodo(SNuovo,[Azione|AzioniPerS])|FigliTail]):-
    trasforma(Azione,S,SNuovo),
    \+member(SNuovo,Visitati),!,
    generaFigli(nodo(S,AzioniPerS),AltreAzioni,Visitati,FigliTail).

generaFigli(nodo(S,AzioniPerS),[_|AltreAzioni],Visitati,FigliTail):-
    generaFigli(nodo(S,AzioniPerS),AltreAzioni,Visitati,FigliTail).

program :-
    open('file.txt',write, Stream),
    forall(man(Man), write(Stream,Man)),
    close(Stream).