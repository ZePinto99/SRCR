e%--------------------------------- - - - - - - - - - -  -  -  -  -   -
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

% Condições ajuste direto 

ajusteDireto('Contrato de aquisicao').
ajusteDireto('Locacao de bens moveis').
ajusteDireto('Aquisicao de servicos').

valInf(X) :- X =<5000.
 
+contrato(_,_,_,AjDir,_,_,Valor,Prazo,_,_) :: ( solucoes(ajusteDireto(AjDir) , contrato(_,_,_,AjDir,_,_,Valor,Prazo,_,_) , Valor),
                                        valInf(Valor)).

+contrato(_,_,_,AjDir,_,_,Valor,Prazo,_,_) :: ( solucoes(ajusteDireto(AjDir) , contrato(_,_,_,AjDir,_,_,Valor,Prazo,_,_) , Prazo),
                                       Prazo=<365).


%Regra dos três anos




%--------------------------------- - - - - - - - - - -  -
%Invariantes

- adjudicante(N,A,V,L) :-
    nao(adjudicante(N,A,V,L)),
    nao(excecao(adjudicante(N,A,V,L))).

- adjudicataria(N,A,V,L) :-
    nao(adjudicataria(N,A,V,L)),
    nao(excecao(adjudicataria(N,A,V,L))).

- contrato(_,_,_,_,_,_,_,_,_) :-
    nao(contrato(_,_,_,_,_,_,_,_,_)),
    nao(excecao(contrato(_,_,_,_,_,_,_,_,_))).

%Não é possível retirar um adjudicante que celebrou um contrato

%Não é possível retirar um adjudicataria que celebrou um contrato


%-------------------------------------------------

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

contrato(705330336,702675112,'Aquisição de serviços', 'Consulta Prévia', 'Assessoria jurídica', 13599, 547, 'Alto de Basto', '11-02-2020').
contrato(680013539,222222222,'Aquisição de serviços', 'Concurso Publico', 'Agricultores', 11111, 1, 'Este S.Pedro', '03-09-2001').
contrato(123456789,760400500,'Aquisição de serviços', 'Concurso Publico', 'Requisicao de Seguranca e Cerco', 7753, 54, 'Atras da UM', '17-08-2011').
contrato(412823999,888888888,'Aquisição de serviços', 'Consulta Prévia', 'Emprestimo de Golfinho', 13, 547, 'Avenida da Liberdade', '28-12-2010').
contrato(134772977,111111111,'Aquisição de serviços', 'Concurso Publico', 'Catering', 1000000000, 70000, 'Gualtar', '18-08-2019').
contrato(405210436,969696969,'Aquisição de serviços', 'Consulta Prévia', 'Venda de Azeite', 0.67, 123, 'Azurem', '29-02-2021').
contrato(033199321,987654321,'Aquisição de serviços', 'Concurso Publico', 'Lavagem de dinheiro', 4109, 54, 'Se', '21-08-2017').
contrato(577550677,555555555,'Aquisição de serviços', 'Ajuste Direto', 'Transporte de Droga', 99, 100, 'S. Vicente', '01-01-2000').
contrato(210494994,444444444,'Aquisição de serviços', 'Ajuste Direto', 'Fornecimento de Cassetetes', 1399, 5489, 'Sameiro', '12-06-2011').
contrato(705330336,801696969,'Aquisição de serviços', 'Concurso Publico', 'Prestaçao de servicos gerais', 999, 1547, 'Real', '02-11-2015').

%------------------------------------------------

%Representar casos de conhecimento imperfeito, pela utilização de valores nulos de todos os tipos estudados

%nome d0 adjudicante desconhecido
adjudicante(999,x007,14141414,'Portugal,Vila Real, Tras-os-Montes e Alto Douro').
excecao(adjudicante(_,_,N,_)):- adjudicante(999,x007,14141414,'Portugal,Vila Real, Tras-os-Montes e Alto Douro').

%localização da adjudicataria ímpossivel de obter
adjudicataria(1818189,'RNE - Rede Nacional de Expressos, Lda', 702675112, x007).
excecao(adjudicataria(_,_,_,E)):- adjudicataria(_,_,_,x007).
nulo(x007).
+adjudicataria(I,_,_,S) :: (solucoes(S,(adjudicataria(1818189,_,_,S), nao(nulo(S))),L), 
                            comprimento(L,N),
                            N == 0).
 
contrato(134772977,555555555,'Aquisição de serviços', 'Consulta Prévia', x017, 75000, 365, 'Nossa Senhora da Conceicao', '12-07-2020').
excecao(contrato(_,_,_,_,E,_,_,_,_)) :- contrato(_,_,_,_,x017,_,_,_,_).

contrato(577550677,987654321,'Aquisição de serviços', 'Concurso Público', x121, x251, 365, 'Abambres', '11-02-2020').
excecao(contrato(_,_,_,_,E,I,_,_,_)) :- contrato(_,_,_,_,x121,x22,_,_,_).

excecao(contrato(705330336,969696969,'Aquisição de serviços', 'Concurso Publico', 'Requisicao de Seguranca e Cerco', 10000, 450, 'Vila Seca de Poiares', '4-09-2017')).
excecao(contrato(705330336,969696969,'Aquisição de serviços', 'Concurso Publico', 'Requisicao de Seguranca e Cerco', 10000, 450, 'Vila Nova de Gaia', '4-09-2017')).

contrato(680013539,801696969,'Aquisição de serviços', 'Consulta Prévia', 'Prestaçao de servicos gerais', 5000, 230, 'Gualtar', xpto).
excecao(contrato(A,B,C,D,E,F,G,H,I)) :- contrato(A,B,C,D,E,F,G,H,xpto).


excecao(contrato(033199321,888888888,'Aquisição de serviços', 'Consulta Prévia', 'Transporte de morangos', 500, 10, 'Amarante', '19-10-2010')).
excecao(contrato(210494994,888888888,'Aquisição de serviços', 'Consulta Prévia', 'Transporte de morangos', 500, 10, 'Amarante', '19-10-2010')).
excecao(contrato(033199321,801696969,'Aquisição de serviços', 'Consulta Prévia', 'Transporte de morangos', 500, 10, 'Amarante', '19-10-2010')).
excecao(contrato(210494994,801696969,'Aquisição de serviços', 'Consulta Prévia', 'Transporte de morangos', 500, 10, 'Amarante', '19-10-2010')).

excecao(contrato(405210436,444444444,'Aquisição de serviços', 'Consulta Prévia', 'Fornecimento de ventiladores', 100000, 2, L, '30-04-2020')). :- 
    pertence(L, ['Lisboa', 'Braga', 'Porto', 'Madeira']).


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
