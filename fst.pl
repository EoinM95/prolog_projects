fst(Input, Trans, Final, Output)
  :- search(Trans, Final, Output, [[Input,q0,[]]]).

search(_, Final, Output, [CurrentNode|_]) :-
    accept(CurrentNode, Final, Output).
%Node = [InputString,CurrentStateName,OutputString]
search(Trans, Final, Output, [CurrentNode|NodeListRest]) :-
    findall(X,canTrans(CurrentNode,Trans,X),Children),
    add2frontier(Children,NodeListRest,NewNodeList),
    search(Trans,Final,Output,NewNodeList).

accept([_,State,Output],Final, Output) :-
    member(State, Final).

canTrans([[NextChar|RestOfInput]|CurrentState],Trans,NextNode) :-
  %Consume character and move to next state/write to output
  arc(NextChar,CurrentState,Trans,NextStateAndOutput)
  , NextNode = [RestOfInput|NextStateAndOutput] ;
  %Elseif epsilon transition preserve input string in its current state
  arc([],CurrentState,Trans,NextStateAndOutput),
  NextNode = [[NextChar|RestOfInput]|NextStateAndOutput].
%Find next state and add to output string if arc exists
arc(NextChar,[StateName,OutputTape],Trans,NextStateAndOutput) :-
  member([StateName,NextChar,ToAppend,NextState],Trans),
  append(OutputTape,[ToAppend],NextOutput),
  NextStateAndOutput = [NextState,NextOutput].

%Builds a FIFO queue to allow for breadth first search
add2frontier(C,[],C).
add2frontier(C,[N|Rest],[N|FNew]):-
  add2frontier(C,Rest,FNew).
