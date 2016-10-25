goal([_,R,Q],Halt_list):- canHalt(R,Q,Halt_list).

/*Can halt if in state Q and reading nextChar and [Q,next] is in halting list*/
canHalt([NextChar|_],Q,Halt_list):- member([Q,NextChar],Halt_list).

canHalt([],Q,Halt_list):- member([Q,b-k],Halt_list).

nTm(Move_right,Move_left,Write_list,Halt_list,Input,Output):-
  search([[[],Input,q0]],Found,Move_right,Move_left,Write_list,Halt_list),
  extract(Found, Output).

search([NLHead|_],NLHead,_,_,_,HL):- goal(NLHead,HL).
search([NLHead|NLRest],Found,MR,ML,WL,HL):-
  findall(X,arc(NLHead,MR,ML,WL,X),Children),
  add2frontier(Children,NLRest,NewNodeList),
  search(NewNodeList,Found,MR,ML,WL,HL).

arc(Node,MR,ML,WL,NextNode):-
  canMoveRight(Node,MR,NextNode) ;
  canMoveLeft(Node,ML,NextNode) ;
  canWrite(Node,WL,NextNode).

canMoveRight([L,[NextChar|Rest],Q],MR,NextNode):-
  member([Q,NextChar,NextState],MR), NextNode = [[NextChar|L],Rest,NextState].

canMoveLeft([[LeftNext|LeftRest],[NextChar|RightRest],Q],ML,NextNode):-
  member([Q,NextChar,NextState],ML),
  NextNode = [LeftRest, [LeftNext,NextChar|RightRest],NextState].

canWrite([L,[NextChar|Rest],Q],WL,NextNode):-
  member([Q,NextChar,NewChar,NextState],WL),
  NextNode = [L,[NewChar|Rest],NextState].

add2frontier(C,[],C).
add2frontier(C,[N|Rest],[N|FNew]):-
  add2frontier(C,Rest,FNew).

extract([L,R,_],Output):-
  extractLeft(L,LeftOutput),
  extractRight(R,LeftOutput,Output).

extractLeft(L,LeftOut):- reverse(L,Reversed), removeInitialBlank(Reversed,LeftOut).

extractRight(R,LeftOutput,Output):-
  append(LeftOutput,R,Appended),
  reverse(Appended,Reversed),
  removeInitialBlank(Reversed,Cleaned),
  reverse(Cleaned,Output).

removeInitialBlank([b-k|Rest],Output):- removeInitialBlank(Rest,Output).
removeInitialBlank(Output,Output):- noInitialBlank(Output).

noInitialBlank([H|_]):- H \= b-k.
