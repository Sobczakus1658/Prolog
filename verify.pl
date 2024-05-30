:- use_module(library(lists)).

% Michal Sobczak 
% ms440009
% initSingleArray(+Name, +N, -ArrayWithName]) 
initSingleArray(Name, N , [Name, A]) :-
    length(A, N),
    mapList(0, A).

% mapList(+Value, -Array)
mapList(_, []).
mapList(V, [V|T]) :-
    mapList(V, T).

% initArray(+N, +ArraysNames, -ArraysWithValues) 
% AI - ArrayInit
initArray(_, [], []).
initArray(N, [Name|RN], [AI|RA]) :-
    initSingleArray(Name, N, AI),
    initArray(N, RN, RA).

% Funkcja która wczytuje dane z pliku i zwraca trojke:
% Variables    (V) - nazwy zmiennych 
% Arrays       (A) - nazwy tablic
% Instructions (I) - instrukcje
% uploadProgram (+File, -Content)
uploadProgram(F, content(V, A, I)) :-
    open(F, read, Stream),
    read(Stream, variables(V)),
    read(Stream, arrays(A)),
    read(Stream, program(I)),
    close(Stream).

% initArrayProcess(+N, -ArrayProcess)
initArrayProcess(N, A) :-
    length(A, N),
    mapList(1, A).

% initVariables(+VariablesNames, -VariablesWithValue)
initVariables([], []).
initVariables([V|R], [[V, 0]|Rest]) :-
    initVariables(R, Rest).

% Zwraca wartosc zmiennej o nazwie Ident.
% getValue(+Ident, +Variables, -Value)
getValue(I, [[I, V]|_], V):- !.
getValue(I, [ _ | Rest], V):- 
    getValue(I, Rest, V).
    
% Pobiera i - ty element z listy. 
% getFromList(+Position, +Array, -Result)
getFromList(0, [V|_], V):- !.

getFromList(P, [_|Rest], R) :-
    P > 0,
    NewP is P - 1,
    getFromList(NewP, Rest, R).

% Pobiera wartosc z pozycji Position z listy o nazwie Ident.
% getNumber(+Ident, +Position, +Array, -Result)
getNumber(I, P, [[I, V]|_], R):-
    getFromList(P, V, R), !. 

getNumber(I, P, [[_, _]| Rest], R) :- 
    getNumber(I, P, Rest, R).

% Ta funkcja służy do rozwijania wyrazen prostych.
% evalValue(+Wyrazenie, +Variables, +ArrayValues, +Id, -Result) 
% gdzie Wyrazenie moze byc:
% 1) array(Ident, Expr),
% 2) Ident,
% 3) Number
evalValue((array(I, E)), V, A, Id, R) :-
    evalArithmetic(E, Val, V, A, Id),
    getNumber(I, Val, A, R), !.

evalValue(I, V, _, _, R) :-
    atom(I),
    getValue(I, V, R), !.

evalValue(N, _, _, _, N):-
    number(N), !.

% calculate(+Number1, +Operation, +Number2, -Result)
% Funkcja dostaje dwie liczby i wykonuje na nich operacje arytmetyczne
calculate(N1, '+', N2, R) :-
    R is N1 + N2.
calculate(N1, '-', N2, R) :-
    R is N1 - N2.
calculate(N1, '*', N2, R) :-
    R is N1 * N2.
calculate(N1, '/', N2, R) :-
    R is N1 / N2.

% Funkcja dostaje dwie liczby i wykonuje na nich operacje logiczne
% calculateLogical(+Number1, +Operation, +Number2, -Result)
calculateLogical(N1, '<', N2, R) :-
    (N1 < N2 -> R = true ; R = false).
calculateLogical(N1, '=', N2, R) :-
    (N1 =:= N2 -> R = true ; R = false).
calculateLogical(N1, '<>', N2, R) :-
    (N1 \= N2 -> R = true ; R = false).

% Ewaluacja wyrazen arytmetycznych
% evalArithmetic(+Expr, -Result, +Variables, +ArrayValues, +Id)
evalArithmetic(E1 + E2, R, V, A, Id) :-
    evalArithmetic(E1, N1, V, A, Id),
    evalArithmetic(E2, N2, V, A, Id),
    calculate(N1, '+', N2, R), !.

evalArithmetic(E1 - E2, R, V, A, Id) :-
    evalArithmetic(E1, N1, V, A, Id),
    evalArithmetic(E2, N2, V, A, Id),
    calculate(N1, '-', N2, R), !.

evalArithmetic(E1 * E2, R, V, A, Id) :-
    evalArithmetic(E1, N1, V, A, Id),
    evalArithmetic(E2, N2, V, A, Id),
    calculate(N1, '*', N2, R), !.

evalArithmetic(E1 / E2, R, V, A, Id) :-
    evalArithmetic(E1, N1, V, A, Id),
    evalArithmetic(E2, N2, V, A, Id),
    calculate(N1, '/', N2, R), !.

evalArithmetic(pid, Id, _ , _ , Id) :- !.

evalArithmetic(E, R, V, A, Id) :-
    evalValue(E, V, A, Id, R).

% Ewaluacja wyrazen logicznych
% evalLogical(+Expr, -Result, +Variables, +ArrayValues, +Id) 
evalLogical(E1 < E2, R, V, A, Id) :-
    evalArithmetic(E1, N1, V, A, Id),
    evalArithmetic(E2, N2, V, A, Id),
    calculateLogical(N1, '<', N2, R).

evalLogical(E1 = E2, R, V, A, Id)  :-
    evalArithmetic(E1, N1, V, A, Id),
    evalArithmetic(E2, N2, V, A, Id),
    calculateLogical(N1, '=', N2, R).

:- op(500, xfx, <>).
evalLogical(E1 <> E2, R, V, A, Id) :-
    evalArithmetic(E1, N1, V, A, Id),
    evalArithmetic(E2, N2, V, A, Id),
    calculateLogical(N1, '<>', N2, R).

% printPath( +[[Counter, Step]|Rest])
% Counter - ktory proces wykonal ruch,
% Step    - numer wykonanego polecenia. 
printPath([]).
printPath([[C, S]|Rest]):-
    printPath(Rest),
    write('Proces '),
    write(C),
    write(': '),
    write(S),
    nl.

% printPath( +[Section |Rest])
% Wypisuje ktore procesy sa w sekcji krytycznej.
printSections([S]) :-
    write(S),!.
printSections([S|Rest]) :- 
    write(S),
    write(', '),
    printSections(Rest).

% printError(+Sections, +Path)
printError(S, P):-
    write('Program jest niepoprawny.\n'),
    write('Niepoprawny przeplot:\n'),
    printPath(P),
    write('Procesy w sekcji krytycznej: '),
    printSections(S),
    write('.\n'), !.

% getCommand(+Id, +ArrayProcess, -Command)
getCommand(Id, A, C) :-
    nth0(Id, A, C).

% State jest trojka ktora zawiera:
% 1) ArrayProcess - procesowi przyporzadkowuje numer kolejnej instrukcji
% 2) VariablesValues - wartosciowanie zmiennych.
% 3) ArrayValues - wartosciowanie tablic.
% Content natomiast jest trojka:
% 1) Instrukcje,
% 2) Variables - nazwy zmiennych,
% 3) Arrays - nazwy tablic.
% initState(+content(Variables, Arrays, Instructions), +N,
%           -state(ArrayProcess, VariablesValues, ArrayValues ))
initState(content(V, A, _ ), N, state(AP, VV, AV)) :-
    initArrayProcess(N, AP),
    initVariables(V, VV),
    initArray(N, A, AV).

% getInstruction(+Instructions, +ArrayProcess, +Id, -SingleInstruction)
getInstruction(I, A, Id, SI) :- 
    nth0(Id, A, Line),
    Line1 is Line - 1,
    nth0(Line1, I, SI).

% Modyfikuje zmienne
% replace(+Name, +Value, +Array, -NewArray) 
replace(_, _, [], []) :- !.
replace(N, V, [[N, _]], [[N, V]]).
replace(N, V, [[N, _] | Rest ], [[N, V]| Rest]).
replace(N1, V, [[N2, V2]| Rest], [HPrim|RPrim]) :-
    HPrim = [N2, V2],
    replace(N1, V, Rest, RPrim).

% Pobiera tablice o nazwie Ident z listy tablic.
% getArray(+Ident, +ArrayList, -Array)
getArray(Ident, [[Ident, V]| _], V) :- !.
getArray(Ident, [[_, _] | Rest], A):-
    getArray(Ident, Rest, A).

% Dostaje liste tablic i zmienia tylko tablice o nazwie Ident
% replaceArray(+Ident, +Index, +Result, +ArrayValues, -ArrayValuesPrim)
replaceArray(Ident, I, R, AV, AVP):-
    getArray(Ident, AV, Array),
    replaceArrayWithIndex(I, R, Array, NewA),
    replace(Ident, NewA, AV, AVP).

% Zamienia wartosc w tablicy Array o indeksie Index na Result
% replaceArrayWithIndex(+Index, +Result, +Array, -NewArray)
replaceArrayWithIndex(_, _, [], []) :- !.
replaceArrayWithIndex(0, R, [_|T], [R|T]) :- !.
replaceArrayWithIndex(I, R, [H|T], [H|Rest]) :-
    I > 0,
    I1 is I - 1,
    replaceArrayWithIndex(I1, R, T, Rest).

% Aktualizuje wartosciowanie wyrazen prostych.
% Wartosc Result przyporzadkowuje odpowiedniej zmiennej
% updateVariable(+Result, +WyrProste, +Variables, +ArrayValues,
%                -VariablesPrim, -ArrayValuesPrim, +Id)
updateVariable(R, (array(I, E)), V, AV, V, AVP, Id) :-
    evalArithmetic(E, N,  V, AV, Id),
    replaceArray(I, N, R, AV, AVP), !.

updateVariable(R, I, V, AV , VP, AV, _) :-
    replace(I, R, V, VP).

% Wykonuje odpowiedni krok modyfikując przy tym odpowiednie wartosci.
% makeStep(+Step, +Id, +ArrayProcess, +Variables, +ArrayValues,
%          -ArrayProcessPrim, -VariablesPrim, -ArrayValuesPrim)
makeStep(goto(N), Id, AP, V, AV, APP, V, AV) :-
    replaceArrayWithIndex(Id, N, AP, APP).

makeStep(condGoto(E, N), Id, AP, V, AV, APP, V, AV) :-
    evalLogical(E, R, V, AV, Id),
    getCommand(Id, AP, C),
    (R = true -> Jump is N ; Jump is C + 1),
    replaceArrayWithIndex(Id, Jump, AP, APP).

makeStep(assign(SV, E), Id, AP, V, AV, APP, VP, AVP) :-
    evalArithmetic(E, R, V, AV, Id),
    updateVariable(R, SV, V, AV, VP, AVP, Id),
    getCommand(Id, AP, C),
    Jump is C + 1,
    replaceArrayWithIndex(Id, Jump, AP, APP).

makeStep(sekcja, Id, AP, V, AV, APP, V, AV) :-
    getCommand(Id, AP, C),
    Jump is C + 1,
    replaceArrayWithIndex(Id, Jump, AP, APP).

% step(+(_, _, Instructions), +state(ArrayProcess, Variables, ArrayValues), ?Id, 
%      -state(ArrayProcessPrim, VariablesPrim, ArrayValuesPrim)) 
step((_, _, I), state(AP, V, AV), Id, state(APP, VP, AVP)) :-
    getInstruction(I, AP, Id, SI),
    makeStep(SI, Id, AP, V, AV, APP, VP, AVP).

% checkSafetyAll(+Instructions, +State, +VisitedStatesInput, -VisitedStatesOutput,
%                ?Counter, ?Path, +N, -Flag)
% NVSO - NewVisitedStatesOutput
checkSafetyAll(I, S, VSI, VSO, C, P, N, F) :-
    NewC is (C + 1),
    state(AP,_,_) = S,
    nth0(C, AP, Step),
    step((_, _, I), S, C, NewS),
    checkSafety(content(_, _, I), NewS, VSI, NVSO, [[C,Step]|P], N, NewF),
    (NewF -> F = true;
    checkSafetyAll(I, S, NVSO, VSO, NewC, P, N, F)),!.

checkSafetyAll(_,_,VSI,VSI,N, _, N,false) :- !.

% Sprawdza ktore procesy są w sekcji krytycznej.
% checkSafeStatement(+Instructions, ?Acc, ?Section, +ArrayProcess, ?Counter, +N) 
checkSafeStatement(_, Acc, Acc, _, N, N).
checkSafeStatement(I, Acc, S, AP, C, N) :-
    NewC is C + 1,
    getInstruction(I, AP, C, SI),
    (SI = 'sekcja' -> NewAcc = [C | Acc] ; NewAcc = Acc),
    checkSafeStatement(I, NewAcc, S, AP, NewC, N).

% Funkcja ta sprawdza czy program jest bezpieczny. Najpierw sprawdza czy
% konfiguracja w ktorej aktualnie się znajduje jest bezpieczna. Jezeli tak,
% to iteruje sie po wszystkich procesach i wykonuje dla nich step i
% rekurencyjnie wywoluje sama siebie, jeżeli stan jeszcze nie był odwiedzony.
% Dzięki temu, ze stanow jest skonczenie wiele, to program ten sie zakonczy.

% checkSafety(+Content, +state(ArrayProcess, +Variables, +ArrayValues),
%             +VisitedStatesInput, -VisitedStatesOutput, ?Path, +N, -Flag)
checkSafety(_, S, VSI, VSI, _, _, false) :-
    member(S, VSI), !.

checkSafety(content(_, _, I ), state(AP, V, AV) , VSI, VSO, P, N, F):-
    \+ member(state(AP, V, AV), VSI),
    checkSafeStatement(I, [], S, AP, 0, N),
    length(S, Len),
    (Len > 1 -> printError(S, P), F = true;  
    checkSafetyAll(I, state(AP, V, AV), [state(AP, V, AV) |VSI], VSO, 0, P, N, F)).
    
% verify(+N, +Program)    
verify(N, Program) :-
    (N < 1 -> write('Error: parametr '),write(N),
    write(' powinien byc liczba > 0'), !, fail ; true),
    set_prolog_flag(fileerrors, off),
    uploadProgram(Program, Content),
    initState(Content, N, State),
    checkSafety(Content, State, [], _, [], N, Flag),
    (Flag -> true; 
    write('Program jest poprawny (bezpieczny)'), nl), !.

verify(_, Program):-
    write('Error: brak pliku o nazwie - '), write(Program), !.
        