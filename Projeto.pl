:- use_module(library(clpfd)).
:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- ['puzzlesAcampar.pl'].

vizinhanca((L,C),Vizinhanca):-
    %Funcao que devolve as coordenadas das posicoes exatamente ao lado da (L,C)
    findall((X,Y),(member((L1,C1),[(L-1,C),(L,C-1),(L,C+1),(L+1,C)]),X is L1, Y is C1),
    Vizinhanca).

vizinhancaAlargada((L,C),VizinhancaAlargada):-
    %Funcao que devolve as coordenadas das posicoes exatamente ao lado e na diagonal da (L,C)
    findall((X,Y),(member((L1,C1),[(L-1,C-1),(L-1,C),(L-1,C+1),(L,C-1),(L,C+1),(L+1,C-1),
    (L+1,C),(L+1,C+1)]),X is L1,Y is C1),VizinhancaAlargada).

todasCelulas_aux(0, []).
todasCelulas_aux(N, [N | L]) :- 
    N > 0,
    Aux is N - 1,
    todasCelulas_aux(Aux, L).

todasCelulas_aux2(N, TodasCelulas) :-
    todasCelulas_aux(N, Linhas),
    reverse(Linhas, L2),
    findall((Linha,Coluna), (member(Linha, L2),between(1, N, Coluna)), TodasCelulas).

todasCelulas(Tabuleiro, TodasCelulas) :-
    %Funcao que devolve todas as coordenadas do tabuleiro
    length(Tabuleiro, X),
    todasCelulas_aux2(X, TodasCelulas).

todasCelulas(Tabuleiro, TodasCelulas,Objeto) :-
    %Funcao que devolve todas as coordenadas do Objeto no tabuleiro
    (   var(Objeto)->  Valor is 3;Valor=Objeto),
    maplist(subs, Tabuleiro, TabuleiroSubstituido),
    findall((L, C), (nth1(L, TabuleiroSubstituido, Linha), nth1(C, Linha, Valor)), TodasCelulas).

subs(Linha, LinhaSubs) :-
    maplist(subselem, Linha, LinhaSubs).

subselem(Elemento, X) :-
    (nonvar(Elemento) -> X = Elemento ; X = 3).

contaLinhas([Linha|Tabuleiro], [ContagemLinhas|Resto], Objecto) :-
    findall(Objecto,member(Objecto,Linha),Contador),
    length(Contador,ContagemLinhas),
    contaLinhas(Tabuleiro, Resto, Objecto).
contaLinhas([], [], _).

contaColuna(Tabuleiro,[X|Y],Objecto,Contador):-
    maplist(nth1(Contador),Tabuleiro,Lista),
    findall(Objecto,member(Objecto,Lista),Cont),
    length(Cont,X),
    Contador2 is Contador + 1,
    contaColuna(Tabuleiro,Y,Objecto,Contador2).
contaColuna(Tabuleiro,[X],Objecto,Contador):-
    member(L,Tabuleiro),length(L,W),Contador==W,
    maplist(nth1(Contador),Tabuleiro,Lista),
    findall(Objecto,member(Objecto,Lista),Cont),
    length(Cont,X).
contaColuna(_,[],_,0).

calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, Objecto):-
    %Funcao que conta o numero de objetos nas linhas e colunas e devolve duas listas
    %com os respetivos
    (   var(Objecto) ->  Objecto is 3;true),
    maplist(subs, Tabuleiro, TabuleiroSubstituido),
    verificaT(TabuleiroSubstituido),
    contaLinhas(TabuleiroSubstituido,ContagemLinhas,Objecto),
    contaColuna(TabuleiroSubstituido,ContagemColunas,Objecto,1). 

celulaVazia(Tabuleiro,(L,C)):-
    %Funcao que averigua se uma coordenada e uma celula vazia
    todasCelulas(Tabuleiro,Celulas1,t),
    todasCelulas(Tabuleiro,Celulas2,r),
    todasCelulas(Tabuleiro,Celulas3,a),
    append(Celulas1,Celulas2,Aux),append(Aux,Celulas3,Celulasf),
    \+ member((L,C),(Celulasf)).

verificaT(Tabuleiro) :-
    %verifica se o Tabuleiro e valido
    is_list(Tabuleiro),
    length(Tabuleiro, X),
    maplist(is_list, Tabuleiro),
    maplist(aux_comp(X), Tabuleiro).

aux_comp(Comprimento, Lista) :-
    length(Lista, Comprimento).

insereObjectoCelula(Tabuleiro, TendaOuRelva, (L, C)) :-
    %Funcao que introduz uma tenda ou relva no tabuleiro
    verificaT(Tabuleiro),
    nth1(L, Tabuleiro, Linha),
    nth1(C, Linha, Elemento),
    (var(Elemento) ->   nth1(C, Linha, TendaOuRelva);true).

insereObjectoEntrePosicoes(Tabuleiro, TendaOuRelva, (L, C1), (L, C2)) :-
    %Funcao que introduz tendas ou relvas numa linha do tabuleiro
    verificaT(Tabuleiro),
	todasCelulas(Tabuleiro,Celulas),
    member((L,C1),Celulas),member((L,C2),Celulas),
    findall(Y,between(C1,C2,Y),Lista),
    insereObjectoEntrePosicoes_aux(Lista,TendaOuRelva,Tabuleiro,L).
    
insereObjectoEntrePosicoes_aux([],_,_,_).
insereObjectoEntrePosicoes_aux([X|Y],TendaOuRelva,Tabuleiro,L):-
    insereObjectoCelula(Tabuleiro, TendaOuRelva, (L,X)),
    insereObjectoEntrePosicoes_aux(Y,TendaOuRelva,Tabuleiro,L).

verificaP_aux((T,_,_),Comp):-
	verificaT(T),
    nth1(1,T,Linha),
    length(Linha,Comp).

verificaP_aux2((_,X,Y),Comp):-
    length(X,Comp),
    length(Y,Comp).

verificaP(Puzzle):-
    %Funcao que verifica se o Puzzle e valido
    verificaP_aux(Puzzle,Comp),
    verificaP_aux2(Puzzle,Comp).

relva_auxL(_,[],_).
relva_auxL(Tabuleiro,[L1|L2],Comprimento):-
    insereObjectoEntrePosicoes(Tabuleiro,r,(L1,1),(L1,Comprimento)),
    relva_auxL(Tabuleiro,L2,Comprimento).

relva((Tabuleiro,L,C)):-
    %Funcao que preenche com relva todas a linhas e/ou colunas em que o numero de tendas
    %ja e o pretendido
    verificaP((Tabuleiro,L,C)),
    calculaObjectosTabuleiro(Tabuleiro, ContagemLinhas, ContagemColunas, t),
    findall(X,(nth1(X,L,Y),nth1(X,ContagemLinhas,Y)),Linha),
    findall(X1,(nth1(X1,C,Y),nth1(X1,ContagemColunas,Y)),Coluna),
    length(Tabuleiro,Comprimento),
    relva_auxL(Tabuleiro,Linha,Comprimento),
    transpose(Tabuleiro,Tabuleiro_aux),
    relva_auxL(Tabuleiro_aux,Coluna,Comprimento),
    transpose(Tabuleiro_aux,Tabuleiro).

inacessiveis(Tabuleiro):-
    %Funcao que coloca relva nas posicoes inacessiveis
    verificaT(Tabuleiro),
    todasCelulas(Tabuleiro,CelulasVazias,_),
    todasCelulas(Tabuleiro,Celulasa,a),
    maplist(vizinhanca,Celulasa,Ocupadas),
	flatten(Ocupadas,Ocupadas2),
    subtract(CelulasVazias,Ocupadas2,Colocar),
    maplist(insereObjectoCelula(Tabuleiro,r),Colocar).

puzzle_tabuleiro((Tabuleiro,L,C),Tabuleiro,L,C).
%Transforma um puzzle num Tabuleiro

aproveita_aux([L1|R],Tabuleiro,Comp):-
    insereObjectoEntrePosicoes(Tabuleiro,t,(L1,1),(L1,Comp)),
    aproveita_aux(R,Tabuleiro,Comp).
aproveita_aux([],_,_).

aproveita(Puzzle):-
    %Funcao que coloca tendas nas linhas e colunas que faltavam colocar tendas e tem 
    %esse exato numero de espacos livres
    verificaP(Puzzle),
    puzzle_tabuleiro(Puzzle,Tabuleiro,L,C),
    length(Tabuleiro,Comp),
    calculaObjectosTabuleiro(Tabuleiro,L1,C1,_),
    findall(X,(nth1(X,L,Y),nth1(X,L1,Y)),Lista),
    aproveita_aux(Lista,Tabuleiro,Comp),
	transpose(Tabuleiro,TabuleiroAux),
    findall(X1,(nth1(X1,C,Y1),nth1(X,C1,Y1)),Lista1),
    aproveita_aux(Lista1,TabuleiroAux,Comp),
    transpose(TabuleiroAux,Tabuleiro),
    puzzle_tabuleiro(Puzzle,Tabuleiro,L,C).

limpaVizinhancas(Puzzle):-
    %Funcao que coloca relva em todas as posicoes a volta de uma tenda
    verificaP(Puzzle),
    puzzle_tabuleiro(Puzzle,Tabuleiro,L,C),
    todasCelulas(Tabuleiro,TodasCelulas,t),
    maplist(vizinhancaAlargada,TodasCelulas,Total),
    flatten(Total,Total2),
    subtract(Total2,TodasCelulas,CelulasVizinhas),
    todasCelulas(Tabuleiro,TodasCord),
    intersection(CelulasVizinhas,TodasCord,CelulasTotais),
    maplist(insereObjectoCelula(Tabuleiro, r),CelulasTotais),
    puzzle_tabuleiro(Puzzle,Tabuleiro,L,C).

unicaHipotese((Tabuleiro,Lista,Colunas)):-
    %Funcao que coloca uma tenda em arvores que ainda nao tenham tenda ligada e tem 
    %exatamente um espaco livre
    verificaP((Tabuleiro,Lista,Colunas)),
    todasCelulas(Tabuleiro,TodasCelulas_a,a),
    todasCelulas(Tabuleiro, TodasCelulas_t, t),
    todasCelulas(Tabuleiro,TodasCelulas_v,_),
    maplist(vizinhanca,TodasCelulas_a,Vizinhanca_a),
   	findall(X,(member(X,Vizinhanca_a),intersection(X, TodasCelulas_v, Celula_vazia),
           length(Celula_vazia,Comp),Comp>0,Comp<2,\+ (member(Elemento, X),
           member(Elemento, TodasCelulas_t))),Lista_Vazia),
    flatten(Lista_Vazia,L1),
    intersection(TodasCelulas_v,L1,Colocar),
    maplist(insereObjectoCelula(Tabuleiro,t),Colocar).

valida(LArv, LTen):-
    %Funcao que averigua se existe uma e apenas uma tenda ligada a cada arvore
    length(LArv,Comp),
    length(LTen,Comp),
    maplist(vizinhanca,LArv,Lista_a),
    findall(X,(member(X,Lista_a),intersection(X,LTen,Lista_aux),
                  length(Lista_aux,Cmp),Cmp>0),Lista),
    length(Lista,Comp).