initSingleArray(Name, [Name, Array], N) :-
    length(Array, N),
    maplist(=(0), Array).

initArray(_, [], []).
initArray(N, [Name|RestNames], [ArrayInit|RestArrays]) :-
    initSingleArray(Name, ArrayInit, N),
    initArray(N, RestNames, RestArrays).

uploadProgram(File, Content) :-
    open(File, read, Stream),
    read(Stream, variables(Variables)),
    read(Stream, arrays(Arrays)),
    read(Stream, program(Instructions)),
    close(Stream),
    Content = content(Variables, Arrays, Instructions).

initArrayProcess(N, ArrayProcess) :-
    length(ArrayProcess, N),
    maplist(=(1), ArrayProcess).

initVariables([], []).
initVariables([Variable|Rest], [[Variable, 0]|RestValues]) :-
    initVariables(Rest, RestValues).

getValue(Ident, [[Ident, Value1]|_], Value2):-
    Value2 = Value1, !.
getValue(Ident, [ _ | Rest], Value):- 
    getValue(Ident, Rest, Value).
    
getFromArray(0, [Value|_], Number):-
    Number = Value, !.

getFromArray(Position, [_|Rest], Number) :-
    Position > 0,
    NewPosition is Position - 1,
    getFromArray(NewPosition, Rest, Number).

getNumber(Ident, Position, [[Ident, Values]|_], Number):-
    getFromArray(Position, Values, Number), !. 

getNumber(Ident, Position, [[_, _]| Rest], Number) :- 
    getNumber(Ident, Position, Rest, Number).

evalValue((array(Ident, Expr)), Result, Variables, ArrayValues, Id) :-
    evalArithmetic(Expr, Value, Variables, ArrayValues, Id),
    getNumber(Ident, Value, ArrayValues, Result), !.

evalValue(Ident, Value, Variables, _, _) :-
    Ident \= pid,
    atom(Ident),
    getValue(Ident, Variables, Value), !.
evalValue( Value, Result, _, _, _):-
    number(Value),
    Result = Value, !.

calculate(Number1, '+', Number2, Result) :-
    Result is Number1 + Number2.
calculate(Number1, '-', Number2, Result) :-
    Result is Number1 - Number2.
calculate(Number1, '*', Number2, Result) :-
    Result is Number1 * Number2.
calculate(Number1, '/', Number2, Result) :-
    Result is Number1 / Number2.

calculateLogical(Number1, '<', Number2, Result) :-
    (Number1 < Number2 -> Result = true ; Result = false).
calculateLogical(Number1, '=', Number2, Result) :-
    (Number1 =:= Number2 -> Result = true ; Result = false).
calculateLogical(Number1, '<>', Number2, Result) :-
    (Number1 \= Number2 -> Result = true ; Result = false).

evalArithmetic(Expr1 + Expr2, Result, Variables, ArrayValues, Id) :-
    evalArithmetic(Expr1, N1, Variables, ArrayValues, Id),
    evalArithmetic(Expr2, N2, Variables, ArrayValues, Id),
    calculate(N1, '+', N2, Result), !.

evalArithmetic(Expr1 - Expr2, Result, Variables, ArrayValues, Id) :-
    evalArithmetic(Expr1, N1, Variables, ArrayValues, Id),
    evalArithmetic(Expr2, N2, Variables, ArrayValues, Id),
    calculate(N1, '-', N2, Result), !.

evalArithmetic(Expr1 * Expr2, Result, Variables, ArrayValues, Id) :-
    evalArithmetic(Expr1, N1, Variables, ArrayValues, Id),
    evalArithmetic(Expr2, N2, Variables, ArrayValues, Id),
    calculate(N1, '*', N2, Result), !.

evalArithmetic(Expr1 / Expr2, Result, Variables, ArrayValues, Id) :-
    evalArithmetic(Expr1, N1, Variables, ArrayValues, Id),
    evalArithmetic(Expr2, N2, Variables, ArrayValues, Id),
    calculate(N1, '/', N2, Result), !.

evalArithmetic(pid, Result, _ , _ , Id) :-
    Result = Id, !.

evalArithmetic(Expr, Result, Variables, ArrayValues, Id) :-
    evalValue(Expr,Result, Variables, ArrayValues, Id).

evalLogical(Expr1 < Expr2, Result, Variables, ArrayValues, Id) :-
    evalArithmetic(Expr1, N1, Variables, ArrayValues, Id),
    evalArithmetic(Expr2, N2, Variables, ArrayValues,Id),
    calculateLogical(N1, '<', N2, Result).

evalLogical(Expr1 = Expr2, Result, Variables, ArrayValues, Id)  :-
    evalArithmetic(Expr1,N1, Variables, ArrayValues, Id),
    evalArithmetic(Expr2,N2, Variables, ArrayValues, Id),
    calculateLogical(N1, '=', N2, Result).

:- op(500, xfx, <>).
evalLogical(Expr1 <> Expr2, Result, Variables, ArrayValues, Id) :-
    evalArithmetic(Expr1, N1, Variables, ArrayValues, Id),
    evalArithmetic(Expr2, N2, Variables, ArrayValues, Id),
    calculateLogical(N1, '<>', N2, Result).

checkSafetyAll(Instructions, State, VisitedStatesInput, VisitedStatesOutput, Counter, Path, N, Flag) :-
    NewCounter is (Counter + 1),
    state(ArrayProcess,_,_) = State,
    nth0(Counter,ArrayProcess,Step),
    step((_, _, Instructions), State, Counter, NewState),
    checkSafety(content(_, _, Instructions), NewState, VisitedStatesInput, VisitedStatesOutputPrim, [[Counter,Step]|Path], N, FlagPrim),
    (FlagPrim -> Flag = true;
    checkSafetyAll(Instructions, State, VisitedStatesOutputPrim, VisitedStatesOutput, NewCounter, Path, N, Flag)),!.

checkSafetyAll(_,_,VisitedStatesInput,VisitedStatesInput,N, _, N,false) :- !.

checkSafeStatement(_, Acc, Acc, _, N, N).
checkSafeStatement(Instructions, Acc, Section, ArrayProcess, Counter, N) :-
    NewCounter is Counter + 1,
    getInstruction(Instructions, ArrayProcess, Counter, Instruction),
    (Instruction = 'sekcja' -> NewAcc = [Counter | Acc] ; NewAcc = Acc),
    checkSafeStatement(Instructions, NewAcc, Section, ArrayProcess, NewCounter, N).

printPath([]).
printPath([[Counter, Step]|Rest]):-
    printPath(Rest),
    write('Proces '),
    write(Counter),
    write(': '),
    write(Step),
    nl.

printSections([Section]) :-
    write(Section),!.
printSections([Section|Rest]) :- 
    write(Section),
    write(', '),
    printSections(Rest).


printError(Sections, Path):-
    write('Program jest niepoprawny.\n'),
    write('Niepoprawny przeplot:\n'),
    printPath(Path),
    write('Procesy w sekcji krytycznej: '),
    printSections(Sections),
    write('.\n'), !.

checkSafety(_, State, VisitedStatesInput, VisitedStatesInput, _, _, false) :-
    member(State, VisitedStatesInput), !.

checkSafety(content(_, _, Instructions ), state(ArrayProcess, VariablesValues, ArrayValues) , VisitedStatesInput, VisitedStatesOutput, Path, N, Flag):-
    \+ member(state(ArrayProcess, VariablesValues, ArrayValues), VisitedStatesInput),
    checkSafeStatement(Instructions, [], Sections, ArrayProcess, 0, N),
    length(Sections, Len),
    (Len > 1 -> printError(Sections, Path), Flag = true;  
    checkSafetyAll(Instructions, state(ArrayProcess, VariablesValues, ArrayValues), [state(ArrayProcess, VariablesValues, ArrayValues) |VisitedStatesInput], VisitedStatesOutput, 0, Path, N, Flag)).

getCommand(Id, ArrayProcess, Command) :-
    nth0(Id, ArrayProcess, Command).

initState(content(Variables, Arrays, _ ), N, state(ArrayProcess, VariablesValues, ArrayValues )) :-
    initArrayProcess(N, ArrayProcess),
    initVariables(Variables, VariablesValues),
    initArray(N, Arrays, ArrayValues).

getInstruction(Instructions, ArrayProcess, Id, Instruction) :- 
    nth0(Id, ArrayProcess, Line),
    Line1 is Line - 1,
    nth0(Line1, Instructions, Instruction).

replace(_, _, [], []) :- !.
replace(Name, Value, [[Name, _]], [[NamePrim, ValuePrim]]) :-
    NamePrim = Name,
    ValuePrim = Value.

replace(Name, Value, [[Name, _] | Rest ], [[NamePrim, ValuePrim]|TPrim]) :-
    TPrim = Rest,
    NamePrim = Name,
    ValuePrim = Value.
replace(Name, Value, [[Name1, Value1]| Rest], [HPrim|RPrim]) :-
    HPrim = [Name1, Value1],
    replace(Name, Value, Rest, RPrim).

replace(Name, Value, [(Name1, Value1)| Rest], [HPrim|RPrim]) :-
    HPrim = [Name1, Value1],
    replace(Name, Value, Rest, RPrim).

replace(Name, Value, [(Name, _)], [(NamePrim, ValuePrim)]) :-
    NamePrim = Name,
    ValuePrim = Value.

replace(Name, Value, [(Name, _) | Rest ], [(NamePrim, ValuePrim)|TPrim]) :-
    TPrim = Rest,
    NamePrim = Name,
    ValuePrim = Value.

getArray(Ident, [[Ident, Values]| _], Array) :-
    Array = Values, !.

getArray(Ident, [[_, _] | Rest], Array):-
    getArray(Ident, Rest, Array).

replaceArray(Ident, Index, Result, ArrayValues, ArrayValuesPrim):-
    getArray(Ident, ArrayValues, Array),
    replaceArrayWithIndex(Index, Result, Array, NewArray),
    replace(Ident, NewArray, ArrayValues, ArrayValuesPrim).

replaceArrayWithIndex(_, _, [], []) :- !.
replaceArrayWithIndex(0, Number, [_|T], [Number|T]) :- !.
replaceArrayWithIndex(I, Number, [H|T], [H|R]) :-
    I > 0,
    I1 is I - 1,
    replaceArrayWithIndex(I1, Number, T, R).

updateVariable(Result, (array(Ident, Value)), Variables, ArrayValues, Variables, ArrayValuesPrim, Id) :-
    evalArithmetic(Value, N,  Variables, ArrayValues, Id),
    replaceArray(Ident, N, Result, ArrayValues, ArrayValuesPrim).

updateVariable(Result, Ident, Variables, ArrayValues , VariablesPrim, ArrayValuesPrim, _) :-
    atomic(Ident),
    replace(Ident, Result, Variables, VariablesPrim),
    ArrayValuesPrim = ArrayValues.

makeStep(goto(Number), Id, ArrayProcess, Variables, ArrayValues, ArrayProcessPrim, Variables, ArrayValues) :-
    replaceArrayWithIndex(Id, Number, ArrayProcess, ArrayProcessPrim).

makeStep(condGoto(Expr, Number), Id, ArrayProcess, Variables, ArrayValues, ArrayProcessPrim, Variables, ArrayValues) :-
    evalLogical(Expr, Result, Variables, ArrayValues, Id),
    getCommand(Id, ArrayProcess, Command),
    (Result = true -> Jump is Number ; Jump is Command + 1),
    replaceArrayWithIndex(Id, Jump, ArrayProcess, ArrayProcessPrim).

makeStep(assign(Variable, Expr), Id, ArrayProcess, Variables, ArrayValues, ArrayProcessPrim, VariablesPrim, ArrayValuesPrim) :-
    evalArithmetic(Expr, Result, Variables, ArrayValues, Id),
    updateVariable(Result, Variable, Variables, ArrayValues, VariablesPrim, ArrayValuesPrim, Id),
    getCommand(Id, ArrayProcess, Command),
    Jump is Command + 1,
    replaceArrayWithIndex(Id, Jump, ArrayProcess, ArrayProcessPrim).
makeStep(sekcja, Id, ArrayProcess, Variables, ArrayValues, ArrayProcessPrim, Variables, ArrayValues) :-
    getCommand(Id, ArrayProcess, Command),
    Jump is Command + 1,
    replaceArrayWithIndex(Id, Jump, ArrayProcess, ArrayProcessPrim).

step((_, _, Instructions), state(ArrayProcess, Variables, ArrayValues), Id, state(ArrayProcessPrim, VariablesPrim, ArrayValuesPrim)) :-
    getInstruction(Instructions, ArrayProcess, Id, Instruction),
    makeStep(Instruction, Id, ArrayProcess, Variables, ArrayValues, ArrayProcessPrim, VariablesPrim, ArrayValuesPrim).

verify(N, Program) :-
    (N < 1 -> write('Error: parametr '),write(N), write(' powinien byc liczba > 0'), !, fail ; true),
    (exists_file(Program) -> true ; write('Error: brak pliku o nazwie - '), write(Program), !, fail),
    uploadProgram(Program, Content),
    initState(Content, N, State),
    checkSafety(Content, State, [], _, [], N, Flag),
    (Flag -> true; 
    write('Program jest poprawny (bezpieczny)'), nl), !.


