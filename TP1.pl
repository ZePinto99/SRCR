%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3 - TP1

%Grupo 12

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- op( 900,xfy,'::' ).
:- dynamic adjudicante/4.
:- dynamic adjudicataria/4.
:- dynamic contrato/9.


%-------------------------------------------------
%Invariantes

%Não pode existir mais que um adjudicante para com o mesmo id
+adjudicante(ID,_,NI,_) :: (solucoes(X,(adjudicante(ID,_,X,_)),L),
                           comprimento(L,N),
                           N == 1).

%Não pode existir mais que um adjudicaria para com o mesmo id
+adjudicataria(ID,_,NI,_) :: (solucoes(X,(adjudicataria(ID,_,X,_)),L),
                             comprimento(L,N),
                             N == 1).

%Um contrato tem de ter dois ids válidos
+contrato(ADE,ADA,_,_,_,_,_,_,_) :: (solucoes(X,(adjudicataria(ADE,_,X,_)),L),
                                    comprimento(L,N),
                                    N == 1,
                                    solucoes(X,(adjudicataria(ADA,_,X,_)),S),
                                    comprimento(S,N),
                                    N == 1).

%Um contrato tem de ter um tipo de procedimento válido
+contrato(_,_,_,PROC,_,_,_,_,_) :: (procValido(PROC)).

procValido('Ajuste direto').
procValido('Consulta previa').
procValido('Concurso publico').

%Condições ajuste direto

execao( contrato(_,_,CONT,'Ajuste direto',_,V,P,_,_) ) :-
        P <= 366, V <= 5000, 
        (CONT='Contrato de aquisição'; CONT='Locação de bens móveis'; CONT='Aquisição de serviços' ).


%Regra dos três anos

%--------------------------------- - - - - - - - - - -  -
%Invariantes

%Não é possível retirar um adjudicante que celebrou um contrato


%Não é possível retirar um adjudicataria que celebrou um contrato


%-------------------------------------------------
adjudicante(1,'Município de Alto de Basto',705330336,'Portugal,Braga, Alto de Basto').
adjudicataria(1,'Associados - Sociedade de Advogados, SP, RL', 702675112, 'Portugal').
contrato(705330336,702675112,'Aquisição de serviços', 'Consulta Prévia', 'Assessoria jurídica', 13599, 547, 'Alto de Basto', '11-02-2020').


%Representar casos de conhecimento imperfeito, pela utilização de valores nulos de todos os tipos estudados;


%--------------------------------- - - - -      - - - - - -  -  -  -  -   -

evolucao( Termo ) :-
    solucoes( Invariante,+Termo::Invariante,Lista ),
    insercao( Termo ),
    teste( Lista ).

insercao( Termo ) :-
    assert( Termo ).
insercao( Termo ) :-
    retract( Termo ),!,fail.

teste( [] ).
teste( [R|LR] ) :-
    R,
    teste( LR ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensão do predicado que permite a involucao do conhecimento

involucao( Termo ) :-
    solucoes( Invariante,-Termo::Invariante,Lista ),
    remocao( Termo ),
    teste( Lista ).

remocao( Termo ) :-
    retract( Termo ).
remocao( Termo ) :-
    assert( Termo ),!,fail.

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}
%                            Resposta = { verdadeiro,falso,desconhecido }

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -

solucoes( X,Y,Z ) :-
    findall( X,Y,Z ).

comprimento( S,N ) :-
    length( S,N ).



pertence( X,[X|L] ).
pertence( X,[Y|L] ) :-
    X \= Y,
    pertence( X,L ).
