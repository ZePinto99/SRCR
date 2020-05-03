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
:- dynamic contrato/12.

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

%Não pode existir mais que um adjudicante para com o mesmo nif
+adjudicante(ID,_,NI,_) :: (solucoes(X,(adjudicante(X,_,NI,_)),L),
                           comprimento(L,N),
                           N == 1).

%Não pode existir mais que um adjudicaria para com o mesmo nif
+adjudicataria(ID,_,NI,_) :: (solucoes(X,(adjudicataria(X,_,NI,_)),L),
                             comprimento(L,N),
                             N == 1).

%Nif e id válidos
+adjudicante(ID,_,NIF,_) :: (integer(ID), integer(NIF)).

+adjudicataria(ID,_,NIF,_) :: (integer(ID), integer(NIF)).

+contrato(ID,_,_,_,_,_,_,_,_,D,M,A) :- (integer(ID), D<31, M<12).


%Um contrato tem de ter dois ids válidos
+contrato(_,ADE,ADA,_,_,_,_,_,_,_,_,_) :: (solucoes(X,(adjudicataria(ADE,_,X,_)),L),
                                    comprimento(L,N),
                                    N == 1,
                                    solucoes(X,(adjudicataria(ADA,_,X,_)),S),
                                    comprimento(S,N),
                                    N == 1).

%Só existe um contrato por id
+contrato(IdC,_,_,_,_,_,_,_,_,_,_,_):: (solucoes(IdC, contrato(IdC,_,_,_,_,_,_,_,_,_,_,_), R),
                                    comprimento(R,C),
                                    1 >= C).


%Um contrato tem de ter um tipo de procedimento válido
+contrato(_,_,_,_,PROC,_,_,_,_,_,_,_) :: (procValido(PROC)).

procValido('Ajuste Direto').
procValido('Consulta Previa').
procValido('Concurso Publico').



% Condições ajuste direto 

ajusteDireto('Contrato de aquisicao').
ajusteDireto('Locacao de bens moveis').
ajusteDireto('Aquisicao de servicos').

valInf(X) :- 5000 >= X.

+contrato(_,_,_,AjDir,'Ajuste Direto',_,Valor,Prazo,_,_,_,_) :: ( solucoes(ajusteDireto(AjDir) , contrato(_,_,_,AjDir,_,_,Valor,Prazo,_,_) , Valor),
                                        valInf(Valor)).

+contrato(_,_,_,AjDir,'Ajuste Direto',_,Valor,Prazo,_,_,_,_) :: ( solucoes(ajusteDireto(AjDir) , contrato(_,_,_,AjDir,_,_,Valor,Prazo,_,_) , Prazo),
                                       365>=Prazo).




%Calcula a diferença entre 2 datas em dias
timediff(DateTime1, DateTime2, Day) :-
        date_time_stamp(DateTime1, TimeStamp1),
        date_time_stamp(DateTime2, TimeStamp2),
        Day is TimeStamp2 - TimeStamp1.

%Soma do valor cobrado em todos os contratos
sum_contrato([],0).
sum_contrato([contrato(_,_,_,_,_,_,V,_,_,_,_,_)|T],Sum) :-
    sum_contrato(T,R),
    Sum is V + R.

%--------------------------------- - - - - - - - - - -  -
%Invariantes

-adjudicante(N,A,V,L) :-
    nao(adjudicante(N,A,V,L)),
    nao(excecao(adjudicante(N,A,V,L))).

-adjudicataria(N,A,V,L) :-
    nao(adjudicataria(N,A,V,L)),
    nao(excecao(adjudicataria(N,A,V,L))).

-contrato(_,_,_,_,_,_,_,_,_,_,_,_) :-
    nao(contrato(_,_,_,_,_,_,_,_,_,_,_,_)),
    nao(excecao(contrato(_,_,_,_,_,_,_,_,_,_,_,_))).

%Não é possível retirar um adjudicante que celebrou um contrato
-adjudicante(_,_,NIF,_) :: (solucoes(NIF, contrato(_,_,_,fisc,_,_,_,_,_,_,_,_,_,_) ,S ),
                            comprimento(S,N),
                            N>=1).

%Não é possível retirar um adjudicataria que celebrou um contrato
-adjudicataria(_,_,NIF,_) :: (solucoes(NIF, contrato(_,_,_,_,fisc,_,_,_,_,_,_,_,_,_) ,S ),
                           	  comprimento(S,N),
                              N>=1).

%------------------------------------------------

%Representar casos de conhecimento imperfeito, pela utilização de valores nulos de todos os tipos estudados

%nome d0 adjudicante desconhecido
adjudicante(999,x007,14141414,'Portugal,Vila Real, Tras-os-Montes e Alto Douro').
excecao(adjudicante(A,N,B,C)):- adjudicante(A,x007,B,C).


%Não sabemos a terra d0 adjudicante mas sabemos que não é de vila real
-adjudicante(500,'Padaria Fernandes',907265511,'Vila Real').
adjudicante(500,'Padaria Fernandes',907265511,xpto123).
excecao(adjudicante(Id,Nome,Nif,Loc)):-
    adjudicante(Id,Nome,Nif,xpto123).

%localização da adjudicataria ímpossivel de obter
adjudicataria(1818189,'RNE - Rede Nacional de Expressos, Lda', 702675112, x007).
excecao(adjudicataria(A,B,C,E)):- adjudicataria(A,B,C,x007).
nulo(x007).
+adjudicataria(1818189,_,702675112,S) :: (solucoes(S,(adjudicataria(1818189,_,702675112,S), nao(nulo(S))),L), 
                                 comprimento(L,N),
                                  N == 0).

contrato(11,134772977,555555555,'Aquisicao de servicos', 'Consulta Previa', x017, 75000, 365, 'Nossa Senhora da Conceicao', 12, 07, 2020).
excecao(contrato(A,B,C,D,E,Z,F,G,H,I,J,K)) :- contrato(A,B,C,D,E,x017,F,G,H,I,J,K).


contrato(12,577550677,987654321,'Aquisicao de servicos', 'Concurso Publico', x121, x251, 365, 'Abambres', 11, 02, 2020).
excecao(contrato(A,B,C,D,E,Z,W,F,G,H,I,J)) :- contrato(A,B,C,D,E,x121,x251,F,G,H,I,J).


excecao(contrato(13,705330336,969696969,'Aquisicao de servicos', 'Concurso Publico', 'Requisicao de Seguranca e Cerco', 10000, 450, 'Vila Seca de Poiares', 04, 09, 17)).
excecao(contrato(13,705330336,969696969,'Aquisicao de servicos', 'Concurso Publico', 'Requisicao de Seguranca e Cerco', 10000, 450, 'Vila Nova de Gaia', 04, 09, 17)).

contrato(14,680013539,801696969,'Aquisicao de servicos', 'Consulta Previa', 'Prestacao de servicos gerais', 5000, 230, 'Gualtar', xpto).
excecao(contrato(A,B,C,D,E,F,G,H,I,Z,J,K)) :- contrato(A,B,C,D,E,F,G,H,I,xpto,xpto2,xpto3).


excecao(contrato(15,033199321,888888888,'Aquisicao de servicos', 'Consulta Previa', 'Transporte de morangos', 500, 10, 'Amarante', 19, 10, 2010)).
excecao(contrato(15,210494994,888888888,'Aquisicao de servicos', 'Consulta Previa', 'Transporte de morangos', 500, 10, 'Amarante', 19, 10, 2010)).
excecao(contrato(15,033199321,801696969,'Aquisicao de servicos', 'Consulta Previa', 'Transporte de morangos', 500, 10, 'Amarante', 19, 10, 2010)).
excecao(contrato(15,210494994,801696969,'Aquisicao de servicos', 'Consulta Previa', 'Transporte de morangos', 500, 10, 'Amarante', 19, 10, 2010)).

excecao(contrato(16,405210436,444444444,'Aquisicao de servicos', 'Consulta Previa', 'Fornecimento de ventiladores', 100000, 2, L, 30, 04, 2020)) :-
                                                                                                        pertence(L, ['Lisboa', 'Braga', 'Porto', 'Madeira']).



%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Desenvolver um sistema de inferência capaz de implementar os mecanismos de raciocínio inerentes a estes sistemas

%Contratos realizados por o nif d0 adjudicante
historicoAdjudicante(NIF,L) :- findall(ID,contrato(ID,NIF,_,_,_,_,_,_,_,_,_,_),L).

%Contratos realizados por o nif de uma adjudicataria
historicoAdjudicataria(NIF,L) :- findall(ID,contrato(ID,_,NIF,_,_,_,_,_,_,_,_,_),L).

%Contratos realizados por o id d0 adjudicante
historicoAdjudicanteId(ID,L) :- adjudicante(ID,_,NIF,_), historicoAdjudicante(NIF,L).

%Contratos realizados por o id de uma adjudicataria
historicoAdjudicatariaId(ID,L) :- adjudicataria(ID,_,NIF,_), historicoAdjudicataria(NIF,L).



%%#########   Não está a dar #########################

%O adjudicante com mais contratos
topAdjudicante(ID) :- findall(I,adjudicante(I,_,_,_),L), topAjAux(ID,L).

topAjAux(ID,[]).
topAjAux(ID,[X|T]) :- historicoAdjudicanteId(ID,L1), length(L1,R1), historicoAdjudicanteId(X,L2), length(L2,R2), R1 >= R2.

%A adjudicataria com mais contratos
topAdjudicataria(ID) :- findall(I,adjudicante(I,_,_,_),L), topAcAux(ID,L).

topAcAux(ID,[]).
topAcjAux(ID,[X|T]) :- historicoAdjudicatariaId(ID,L1), length(L1,R1), historicoAdjudicatariaId(X,L2), length(L2,R2), R1 >= R2.


%####################################################


%O valor dos contratos de um adjudicante
valorAdjudicante(ID,R) :-  historicoAdjudicanteId(ID,L), valores(L,R).


%O valor dos contratos de uma adjudicataria
valorAdjudicataria(ID,R) :-  historicoAdjudicatariaId(ID,L), valores(L,R).

valores([],0).
valores([X|T],R) :- valores(T,R1), contrato(X,_,_,_,_,_,V,_,_,_,_,_), R is R1+V.

%Tipo de contrato mais realizado
nrContTipo(T,C) :- findall(T,contrato(_,_,_,_,T,_,_,_,_,_,_,_),L), length(L,C).

topTipo('Ajuste Direto') :- nrContTipo('Ajuste Direto',R), nrContTipo('Consulta Previa',R1), nrContTipo('Concurso Publico',R2), R > R1, R > R2.
topTipo('Consulta Previa') :- nrContTipo('Ajuste Direto',R), nrContTipo('Consulta Previa',R1), nrContTipo('Concurso Publico',R2), R1 > R, R1 > R2.
topTipo('Concurso Publico') :- nrContTipo('Ajuste Direto',R), nrContTipo('Consulta Previa',R1), nrContTipo('Concurso Publico',R2), R2 > R, R2 > R1. 

%Valor por Tipo de contrato
valorTipoContrato(T,V) :- findall(ID,contrato(ID,_,_,_,T,_,_,_,_,_,_,_),L), valores(L,V).


%Dinheiro movimentado num ano
dinheiroAno(A,R) :- findall(ID, contrato(ID,_,_,_,_,_,_,_,_,_,_,A),L), valores(L,R).


%--------------------------------- - - - - - - - - - -  -  -  -  -   -

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

demo(Questao,verdadeiro):- Questao.
demo(Questao,falso):- -Questao.
demo(Questao,desconhecido):- nao(Questao), nao(-Questao).

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

%-------------------------------------------------
%Falta conhecimento negativo

adjudicante(1,'Município de Alto de Basto',705330336,'Portugal,Braga, Alto de Basto').
adjudicante(2,'Junta de Freguesia Este S.Pedro',680013539,'Portugal,Braga, Este S.Pedro').
adjudicante(3,'Hospital De Braga',123456789,'Portugal,Braga, Atras da UM').
adjudicante(4,'Loja do cidadão',412823999,'Portugal,Braga, Avenida da Liberdade').
adjudicante(5,'Universidade do minho Braga',134772977,'Portugal,Braga, Gualtar').
adjudicante(6,'Universidade do minho Espanha',405210436,'Espanha,Guimaraes, Azurem').
adjudicante(7,'PSP Braga',033199321,'Portugal,Braga, Se').
adjudicante(8,'Gold Center',577550677,'Portugal,Braga,S. Vicente ').
adjudicante(9,'GNR Sameiro',210494994,'Portugal,Braga, Sameiro').
adjudicante(10,'Finanças',760200300,'Portugal,Braga, Real').

%-------------------------------------------------
%Falta conhecimento negativo

adjudicataria(1, 'Associados - Sociedade de Advogados', 702675112, 'Portugal').
adjudicataria(2, 'AgroLândia', 222222222, 'Pedralva, Braga, Portugal').
adjudicataria(3, 'Azeite Galo', 969696969, 'Azurém, Guimarães, Espanha').
adjudicataria(4, 'Padaria de Aljubarrota', 760400500, 'Aljubarrota, Alcobaça, Portugal').
adjudicataria(5, 'ZooMarine', 888888888, 'Guia, Albufeira, Portugal').
adjudicataria(6, 'Lavandarias Coentrão', 987654321, 'Caxinas, Vila do Conde, Portugal').
adjudicataria(7, 'TUB', 555555555, 'Maximinos, Braga, Portugal').
adjudicataria(8, 'Sex Shop Avé Maria', 444444444, 'São Vitor, Braga, Portugal').
adjudicataria(9, 'Residencial Cairense', 801696969 , 'Maximinos, Braga, Portugal').
adjudicataria(10, 'Tasquinha Bracarense', 111111111, 'Gualtar, Braga, Portugal').

%-------------------------------------------------
%Falta conhecimento negativo

contrato(1,705330336,702675112,'Aquisicao de servicos', 'Consulta Previa', 'Assessoria juridica', 13599, 547, 'Alto de Basto', 11, 02, 2020).
contrato(2,680013539,222222222,'Aquisicao de servicos', 'Concurso Publico', 'Agricultores', 11111, 1, 'Este S.Pedro', 03, 09, 2001).
contrato(3,123456789,760400500,'Aquisicao de servicos', 'Concurso Publico', 'Requisicao de Seguranca e Cerco', 7753, 54, 'Atras da UM', 17, 08, 2011).
contrato(4,412823999,888888888,'Aquisicao de servicos', 'Consulta Previa', 'Emprestimo de Golfinho', 13, 547, 'Avenida da Liberdade', 28, 12, 2010).
contrato(5,134772977,111111111,'Aquisicao de servicos', 'Concurso Publico', 'Catering', 1000000000, 70000, 'Gualtar', 18, 08, 2019).
contrato(6,405210436,969696969,'Aquisicao de servicos', 'Consulta Previa', 'Venda de Azeite', 0.67, 123, 'Azurem', 29, 02, 2021).
contrato(7,033199321,987654321,'Aquisicao de servicos', 'Concurso Publico', 'Lavagem de dinheiro', 4109, 54, 'Se', 21, 08, 2017).
contrato(8,577550677,555555555,'Aquisicao de servicos', 'Ajuste Direto', 'Transporte de Droga', 99, 100, 'S. Vicente', 01, 01, 2000).
contrato(9,210494994,444444444,'Aquisicao de servicos', 'Ajuste Direto', 'Fornecimento de Cassetetes', 1399, 5489, 'Sameiro', 12, 06, 2011).
contrato(10,705330336,801696969,'Aquisicao de servicos', 'Concurso Publico', 'Prestaçao de servicos gerais', 999, 1547, 'Real', 02, 11, 2015).
