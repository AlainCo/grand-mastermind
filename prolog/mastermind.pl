
:- module(mastermind, 
    [
        chooseSecret/0, chooseSecret/1, evaluateCodeOnSecret/4,
            getEvaluations/1,clearEvaluations/0,clearEvaluation/1,
            guessSecretFromEvaluations/1,addEvaluation/4,
            possibleSecretFromEvaluations/1,guessPossibleSecretFromEvaluations/1,
            guessNearestSecretFromEvaluations/2,guessFarthestSecretFromEvaluations/2,
            theSecret/1,evaluateCode/5,codeLength/1,guessSecretFromEvaluations/3,
            randomCode/1,getEvaluationsQ/1,guessSecretFromEvaluationsQ/4
    ]). 

:- public chooseSecret/0, chooseSecret/1, evaluateCodeOnSecret/4,
            getEvaluations/1,clearEvaluations/0,clearEvaluation/1,
            guessSecretFromEvaluations/1,addEvaluation/4,
            possibleSecretFromEvaluations/1,guessPossibleSecretFromEvaluations/1,
            guessNearestSecretFromEvaluations/2,guessFarthestSecretFromEvaluations/2,
            theSecret/1,evaluateCode/5,codeLength/1,guessSecretFromEvaluations/3,
            randomCode/1,getEvaluationsQ/1,guessSecretFromEvaluationsQ/4.


:- use_module(library(random)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(pairs)).
:- use_module(library(sort)).

%:- set_prolog_flag(clpfd_monotonic, true).

:- dynamic theSecret/1, evaluation/4.

traceMastermind(X):-
	debug(mastermind,"~w",X).
traceMastermind(Message,Params):-
	debug(mastermind,Message,Params).

colorList([red,green,blue,yellow,black]).
shapeList([square,rectangle,circle,triangle,hexagon]).

:- table colorNb(-).
colorNb(N):-colorList(L),length(L,N).

:- table shapeNb(-).
shapeNb(N):-shapeList(L),length(L,N).

:- table color(+).
color(C):-colorList(L),member(C,L).

:- table shape(+).
shape(H):-shapeList(L),member(H,L).
:- table codeLength(-).
codeLength(4).
:- table squareNb(+).
squareNb(NbQ):- colorNb(NC),shapeNb(NS),NbQ is NC * NS.

:- table codeSquareNb(+).
codeSquareNb(NbQCode):- squareNb(NbQ),codeLength(SZ),NbQCode is SZ*NbQ.

:- table lcmCodePartition(-).
lcmCodePartition(LCM):-
    codeLength(N),
    lcmPartition(N,LCM).

lcmPartition(N,R):-
    lcmPartition(1,N,R).
lcmPartition(Acc,1,Acc).
lcmPartition(Acc,N,R):-
    N>1,
    Acc2 is lcm(Acc,N),
    N2 is N-1,
    lcmPartition(Acc2,N2,R).


:- table blueTable(+).
:- table indexTableM(+).
:- table indexTableCode(+).

indexTableM(IT):-
    colorNb(NbC),shapeNb(NbH),NbCH is NbC*NbH,
    length(IT,NbCH),
    foldl({}/[T,I,I2]>>(I2 is I+1,T=I),IT,0,NbCH).

indexTableCode(IC):-
    codeLength(SZ),
    length(IC,SZ),
    foldl({}/[C,I,I2]>>(I2 is I+1,C=I),IC,0,SZ).


blueTable(BT):-
    colorNb(NbC),
    indexTableM(IT),
    same_length(BT,IT),
    maplist({NbC,IT}/[I,B]>>(
        maplist({NbC,I}/[J,Flag]>>(
            Flag#<==>
                 (mod(I-J,NbC)#=0 #\ div(I,NbC)#=div(J,NbC))
                   ),IT,B)
        ),IT,BT).

randomPair(P):-
    P=(C,S),colorList(CL),shapeList(SL),
    random_member(C,CL),
    random_member(S,SL).


randomCode(Code):-
    codeLength(N),
    randomCode(N, Code).

randomCode(N, Code) :-
    randomCode(N, [], Code).
randomCode(0, Acc, Acc).
randomCode(N, Acc, Code) :-
    N>0,
    randomPair(P),
    N2 is N - 1,
    randomCode(N2, [P|Acc], Code).

clearEvaluations:-
        retractall(evaluation(_,_,_,_)),
        traceMastermind(clearEvaluations).

clearEvaluation(PText):-
        retractall(evaluation(PText,_,_,_)),
        traceMastermind(clearEvaluation(PText)).
        
addEvaluation(PText,NbBlack,NbWhite,NbBlue):-
    retractall(evaluation(PText,_,_,_)),
     assert(evaluation(PText,NbBlack,NbWhite,NbBlue)),
     traceMastermind(addEvaluation(PText,NbBlack,NbWhite,NbBlue)).

getEvaluations(Evaluations):-
     findall((PText,NbBlack,NbWhite,NbBlue),
             evaluation(PText,NbBlack,NbWhite,NbBlue),Evaluations).

getEvaluationsQ(EvaluationsQ):-
    getEvaluations(Evaluations),
    maplist({}/[Evaluation,EvaluationQ]>>(
                   Evaluation=(PText,NbBlack,NbWhite,NbBlue),
                   qpairListLabel(PQ,PText),
                   EvaluationQ=(PQ,NbBlack,NbWhite,NbBlue)),
            Evaluations,EvaluationsQ).



chooseSecret(S) :- 
	codeLength(N),
	length(S,N),
	retractall(theSecret(_)),
	clearEvaluations,
	assert(theSecret(S)),
    traceMastermind(chooseSecret(S)).

chooseSecret :- randomCode(S),chooseSecret(S).

%:- table qpairLabel/2.
qpairLabel(Q,(CLabel,HLabel)):-
    colorList(CList),nth0(CIndex,CList,CLabel),
    shapeList(HList),nth0(HIndex,HList,HLabel),
    length(CList,CLength),
    Q #= CIndex+CLength*HIndex.
qpairList(Q):-
    codeLength(SZ),
    length(Q,SZ),
    colorNb(NbC),shapeNb(NbH),
    QN is NbC*NbH-1,
    Q ins 0..QN.

qpairListLabel(PQ,PText):-
    codeLength(SZ),
    length(PText,SZ),
    qpairList(PQ),
    maplist(qpairLabel,PQ,PText).


countCross(P,PQ,SQ,NA,NY):-
    maplist({P}/[S,E]>>(E#<==>(P#=S)),SQ,CrossEq),
    sum(CrossEq,#=,NX),
    maplist({P}/[P2,E]>>(E#<==>(P#=P2)),PQ,AutoEq),
    sum(AutoEq,#=,NA),
    NY #= min(NA,NX).

checkBlue(P,S,PQ,SQ,NColor,IsBlue):-
    maplist({P,S}/[SI,PI,E]>>(E#<==>((P#=SI)#\/(S#=PI))),SQ,PQ,CrossEq),
    sum(CrossEq,#=,NC),
    IsBlue #<==>((NC #=0) #/\  ( (mod(P-S,NColor)#=0) #\ (div(P,NColor)#=div(S,NColor))) ),
    true.


evaluateCodeQ(SQ,PQ,NbBlack,NbWhite,NbBlue) :-
    lcmCodePartition(LCM),
    colorNb(NColor),
    maplist({}/[P,S,E]>>(E#<==>(P#=S)),PQ,SQ,ParEq),
    sum(ParEq,#=,NbBlack),
    maplist({PQ,SQ,LCM}/[P,F]>>(
            countCross(P,PQ,SQ,NA,NY), 
            F#= div(NY*LCM,NA),
            mod(NY*LCM,NA)#=0
        ),PQ,CrossFrac2),
    sum(CrossFrac2,#=,NLCM),
    NbWhite #= div(NLCM,LCM)-NbBlack,
    mod(NLCM,LCM)#=0,
    maplist({PQ,SQ,NColor}/[P,S,E]>>(
            checkBlue(P,S,PQ,SQ,NColor,E)
        ),PQ,SQ,ParBlue),
    sum(ParBlue,#=,NbBlue),
    true.

evaluateCode(SText,PText,NbBlack,NbWhite,NbBlue) :-
    qpairListLabel(SQ,SText),qpairListLabel(PQ,PText),
    evaluateCodeQ(SQ,PQ,NbBlack,NbWhite,NbBlue).


evaluateCodeOnSecret(PText,NbBlack,NbWhite,NbBlue):-
    theSecret(SText),
    evaluateCode(SText,PText,NbBlack,NbWhite,NbBlue),
    addEvaluation(PText,NbBlack,NbWhite,NbBlue),
    traceMastermind(evaluation(PText,NbBlack,NbWhite,NbBlue)).




evaluateRandomCode(PText,NbBlack,NbWhite,NbBlue):-
    randomCode(PText),
    evaluateCodeOnSecret(PText,NbBlack,NbWhite,NbBlue).

diffCodeQEl(GEl,PQ,Diff):-
     maplist({GEl}/[PEl,Diff]>>( Diff #<==> (PEl #\= GEl)),PQ,DiffList),
     sum(DiffList,#=,Diff).


diffCodeQ(GQ,PQ,Diff):-
    maplist({PQ}/[GEl,D]>>(diffCodeQEl(GEl,PQ,D)),GQ,DiffList),
    sum(DiffList,#=,Diff).

diffCodeAutoG(GQ,Diff):-
    diffCodeQ(GQ,GQ,Diff).

diffEvaluationsQ(GQ,EvaluationsQ,Diff):-
     maplist({GQ}/[EvaluationQ,D]>>(EvaluationQ=(PQ,_,_,_),diffCodeQ(GQ,PQ,D) ),EvaluationsQ,DiffList),
     sum(DiffList,#=,Diff).

diffParallelQ(GQ,PQ,Diff):-
     maplist({}/[GEl,PEl,Diff]>>( Diff #<==>(PEl #\= GEl)),GQ,PQ,DiffList),
     sum(DiffList,#=,Diff).

diffParallelCH(GQ,PQ,Diff):-
    colorNb(NbC),
    maplist({NbC}/[GEl,PEl,Diff]>>(
                   DiffC#<==> (mod(PEl,NbC) #\= mod(GEl,NbC)),
                   DiffH#<==>(div(PEl,NbC) #\= div(GEl,NbC)),
                   Diff #= DiffH+DiffC
               ),
            GQ,PQ,DiffList),
     sum(DiffList,#=,Diff).

diffEvaluationsCH(GQ,EvaluationsQ,Diff):-
     maplist({GQ}/[EvaluationQ,D]>>(
                      EvaluationQ=(PQ,_,_,_),
                      diffParallelCH(GQ,PQ,D)
                  ),
             EvaluationsQ,DiffList),
     sum(DiffList,#=,Diff).

diffCodeCHEl(GEl,PQ,Diff):-
    colorNb(NbC),
    maplist({GEl,NbC}/[PEl,Diff]>>(
                       DiffC#<==> (mod(PEl-GEl,NbC) #\= 0),
                       DiffH#<==> (div(PEl,NbC) #\= div(GEl,NbC) ),
                       Diff #= DiffC+DiffH
                   ),
             PQ,DiffList),
     sum(DiffList,#=,Diff).

diffCodeCH(GQ,PQ,Diff):-
    maplist({PQ}/[GEl,D]>>(diffCodeCHEl(GEl,PQ,D)),GQ,DiffList),
    sum(DiffList,#=,Diff).

diffCodeAutoCHG(GQ,Diff):-
    diffCodeCH(GQ,GQ,Diff).


trySolve(GQ,Message):-
    tryConstraint(true,GQ,Message).

showSolve(GQ,Message):-
    (
        trySolve(GQ,Message)->(
            traceMastermind("showSolve OK ~w",[Message])
            );(
                traceMastermind("showSolve KO ~w",[Message])
            )
    ).
              
tryConstraint(P,GQ,_Message):-
    (   
        catch(
            (   call(P),
                copy_term(GQ, GQCopy),
                (   once(labeling([ff],GQCopy))
                ->  true
                ;   (   %traceMastermind("tryConstraint labeling Failed :~w",[Message]),
                        throw(labeling_failed) )
                )
            ),
            labeling_failed,
            fail  % Si le labeling échoue, on fait échouer toute la branche
        )
        ->  (
                %traceMastermind("tryConstraint success :~w",[Message]),
                true
            ) 
        ;   (
                %traceMastermind("tryConstraint Failed :~w",[Message]),
                true
            )
    ).   

tryConstraintSecretFromEvaluationsQ(GQ,EvaluationsQ,Beauty):-
    tryConstraint(constraintSecretFromEvaluationsQ(GQ,EvaluationsQ),GQ,Beauty).



tryAllDistinct(L,Beauty,GQ):-
    tryConstraint(all_distinct(L),GQ,Beauty).

not_in_array(V, A) :-
        maplist(#\=(V), A).

tryNotInArray(V,A,Beauty,GQ):-
    tryConstraint(not_in_array(V,A),GQ,Beauty).

%maximize nothing
optimConstraintNone(_GQ,_EvaluationsQ,Strategy,MoreVars):-
    Strategy=ff,MoreVars=[].

%maximize distance to last proposal
optimConstraintMaxDiffLastProposal(GQ,EvaluationsQ,Strategy,MoreVars):-
    (
        last(EvaluationsQ,LastEvaluationQ)->(
            LastEvaluationQ=(LastPQ,_,_,_),
            diffParallelCH(GQ,LastPQ,Diff),
            Strategy=max(Diff),MoreVars=[Diff]
        );(
            Strategy=ffc,MoreVars=[]
        )
    ).

% try every constrains but relax if failed
beautyConstraint(Beauty,GQ,EvaluationsQ,Strategy,MoreVars):-
    Beauty=20,
    colorNb(NbC),shapeNb(NbH),
    codeLength(SZ),
    length(EvaluationsQ,NbEvaluations),
    maplist({NbC}/[EvaluationQ,PQ,NbQ]>>
        (
               EvaluationQ=(PQ,NbBlack,NbWhite,_NbBlue),
               NbQ#=NbBlack+NbWhite
        ),EvaluationsQ,PListQ0,NbQList),
    PListQ=[GQ|PListQ0],
    maplist({NbC}/[PQ,PC,PH]>>
            (
                   maplist({NbC}/[PCel,PHel,PQel]>>(
                                  PCel #= mod(PQel,NbC),
                                  PHel #= div(PQel,NbC)
                               ),PC,PH,PQ)
            ),PListQ,PListC,PListH),
    PListC=[GC|PListC0],
    PListH=[GH|PListH0],
    append(PListQ0,AllQ0),
    append(PListH0,AllH0),
    append(PListC0,AllC0),
    transpose(PListH0,PListHT0),
    transpose(PListC0,PListCT0),
    transpose(PListQ0,PListQT0),
    traceMastermind("try beautyConstraint ... beauty:~w",[Beauty]),
    (NbEvaluations*SZ<NbC*NbH,sum(NbQList,#<,SZ)->(
        %traceMastermind("NbQList ~w <SZ ~w ",[NbQList,SZ]),
        true);(
            %traceMastermind("NbQList ~w >=SZ ~w , constraintSecretFromEvaluationsQ ",[NbQList,SZ]),
            constraintSecretFromEvaluationsQ(GQ,EvaluationsQ)
            )),
    % avoid repeted pairs colors shapes in the guessed code    
    tryAllDistinct(GQ,Beauty,GQ),
    tryAllDistinct(GH,Beauty,GQ),
    tryAllDistinct(GC,Beauty,GQ),
    % avoid pairs or colors or shapes used anywhere in the previous proposals 
    maplist({GQ,Beauty,AllQ0,AllH0,AllC0}/[GQel,GHel,GCel]>>(
        tryNotInArray(GQel,AllQ0,Beauty,GQ),
        tryNotInArray(GHel,AllH0,Beauty,GQ),
        tryNotInArray(GCel,AllC0,Beauty,GQ)
    ),GQ,GH,GC),
    %avoid pairs at same places in the previous proposals 
    (PListQT0\=[]->
        maplist({NbC,Beauty,GQ}/[PQ,GQel]>>(
                tryNotInArray(GQel,PQ,Beauty,GQ)
            ),PListQT0,GQ);true),
    %avoid colors shaps at same places in the previous proposals 
    (PListHT0\=[]->
        maplist({NbC,Beauty,GQ}/[PH,PC,GHel,GCel]>>(
                tryNotInArray(GHel,PH,Beauty,GQ),
                tryNotInArray(GCel,PC,Beauty,GQ)
            ),PListHT0,PListCT0,GH,GC);true),
    tryConstraintSecretFromEvaluationsQ(GQ,EvaluationsQ,Beauty),
    % decide strategy to find solutions
    optimConstraintNone(GQ,EvaluationsQ,Strategy,MoreVars).



% pairs, colors, shapes are unique anywhere
% shapes and colors are unique at a place, pairs are unique anywhere
beautyConstraint(Beauty,GQ,EvaluationsQ,ff,[]):-
    Beauty=16,
    colorNb(NbC),
    maplist({NbC}/[EvaluationQ,PQ,PC,PH]>>
            (
                   EvaluationQ=(PQ,_NbBlack,_NbWhite,_NbBlue),
                   maplist({NbC}/[PCel,PHel,PQel]>>(
                                  PCel #= mod(PQel,NbC),
                                  PHel #= div(PQel,NbC)
                               ),PC,PH,PQ)
            ),[(GQ,0,0,0)|EvaluationsQ],PListQ,PListC,PListH),
     append(PListQ,AllQ),
     append(PListH,AllH),
     append(PListC,AllC),
     traceMastermind("try beautyConstraint ... beauty:~w",[Beauty]),
     all_distinct(AllQ), all_distinct(AllH), all_distinct(AllC),
     tryConstraintSecretFromEvaluationsQ(GQ,EvaluationsQ,Beauty).

% shapes and colors are unique at a place, pairs are unique anywhere
% less proposed pairs than possible combinations
% less black+white result than pairs in the code
beautyConstraint(Beauty,GQ,EvaluationsQ,ff,[]):-
    Beauty=12,
    colorNb(NbC),shapeNb(NbH),
    codeLength(SZ),
    length(EvaluationsQ,NbEvaluations),
    NbEvaluations*SZ<NbC*NbH,
    maplist({NbC}/[EvaluationQ,PQ,NbQ]>>
            (
                   EvaluationQ=(PQ,NbBlack,NbWhite,_NbBlue),
                   NbQ#=NbBlack+NbWhite
            ),EvaluationsQ,PListQ0,NbQList),
    sum(NbQList,#<,SZ),
    PListQ=[GQ|PListQ0],
    maplist({NbC}/[PQ,PC,PH]>>
            (
                   maplist({NbC}/[PCel,PHel,PQel]>>(
                                  PCel #= mod(PQel,NbC),
                                  PHel #= div(PQel,NbC)
                               ),PC,PH,PQ)
            ),PListQ,PListC,PListH),
    append(PListQ,AllQ),
    all_distinct(AllQ),
    transpose(PListH,PListHT),
    transpose(PListC,PListCT),
    traceMastermind("try beautyConstraint ... beauty:~w",[Beauty]),
    maplist({}/[PH,PC]>>(
                   all_distinct(PH),all_distinct(PC)
               ),
            PListHT,PListCT),
    tryConstraintSecretFromEvaluationsQ(GQ,EvaluationsQ,Beauty).

%pairs, shapes, and colors, are unique at a place
% less proposed pairs than possible combination
% less black+white result than pairs in the code
beautyConstraint(Beauty,GQ,EvaluationsQ,ff,[]):-
    Beauty=8,
    colorNb(NbC),shapeNb(NbH),
    codeLength(SZ),
    length(EvaluationsQ,NbEvaluations),
    NbEvaluations*SZ<NbC*NbH,
    maplist({}/[EvaluationQ,PQ,NbQ]>>
            (
                   EvaluationQ=(PQ,NbBlack,NbWhite,_NbBlue),
                    NbQ#=NbBlack+NbWhite
            ),EvaluationsQ,PListQ0,NbQList),
    sum(NbQList,#<,SZ),
    PListQ=[GQ|PListQ0],
    transpose(PListQ,PListQT),
    traceMastermind("try beautyConstraint ... beauty:~w",[Beauty]),
    maplist({NbC}/[PQ]>>(
                   all_distinct(PQ),
                   maplist({NbC}/[Q,H,C]>>(
                                      H#=div(Q,NbC),
                                      C#=mod(Q,NbC)),
                            PQ,PH,PC),
                   all_distinct(PH),all_distinct(PC)
               ),PListQT),
    tryConstraintSecretFromEvaluationsQ(GQ,EvaluationsQ,Beauty).

% pairs are unique at a place, and it is a possible solution
% less proposed pairs than possible combination
beautyConstraint(Beauty,GQ,EvaluationsQ,ff,[]):-
    Beauty=4,
    colorNb(NbC),shapeNb(NbH),
    codeLength(SZ),
    length(EvaluationsQ,NbEvaluations),
    NbEvaluations*SZ<NbC*NbH,
    maplist({}/[EvaluationQ,PQ]>>
            (
                   EvaluationQ=(PQ,_NbBlack,_NbWhite,_NbBlue)
            ),EvaluationsQ,PListQ0),
    PListQ=[GQ|PListQ0],
    transpose(PListQ,PListQT),
    traceMastermind("try beautyConstraint ... beauty:~w",[Beauty]),
    constraintSecretFromEvaluationsQ(GQ,EvaluationsQ),
    maplist({NbC}/[PQ]>>(
                   all_distinct(PQ)
               ),PListQT),
    tryAllDistinct(GQ,Beauty,GQ).

% eventually no pair repeated in the guess, 
% and it's a possible solution
beautyConstraint(Beauty,GQ,EvaluationsQ,ff,[]):-
    Beauty=0,
    traceMastermind("try beautyConstraint ... beauty:~w",[Beauty]),
    constraintSecretFromEvaluationsQ(GQ,EvaluationsQ),
    tryAllDistinct(GQ,Beauty,GQ).



guessSecretFromEvaluations(GText):-
    time(once(guessSecretFromEvaluations(GText,100,_BeautyOut))).

guessSecretFromEvaluations(GText,BeautyIn,BeautyOut):-
    getEvaluationsQ(EvaluationsQ),
    guessSecretFromEvaluationsQ(GQ,BeautyIn,BeautyOut,EvaluationsQ),
    qpairListLabel(GQ,GText).

guessSecretFromEvaluationsQ(GQ,BeautyIn,BeautyOut,EvaluationsQ):-
    qpairList(GQ),
    BeautyOut#=<BeautyIn,
    beautyConstraint(BeautyOut,GQ,EvaluationsQ,Strategy,MoreVars),
    append(MoreVars,GQ,Vars),
    traceMastermind("guessSecretFromEvaluationsQ labeling... beauty:~w",[BeautyOut]),
    length(EvaluationsQ,EvaluationsLen),
    ((mod(EvaluationsLen,2)=0)->(Direction=up);(Direction=down)),
    labeling([Strategy,Direction],Vars),
    traceMastermind(solver(beauty(BeautyIn,BeautyOut),strategy(Strategy,MoreVars))).

constraintSecretFromEvaluationsQ(GQ,EvaluationsQ):-
    maplist({GQ}/[EvaluationQ]>>(
                      EvaluationQ=(PQ,NbBlack,NbWhite,NbBlue),
                      evaluateCodeQ(GQ,PQ,NbBlack,NbWhite,NbBlue)
                  ),EvaluationsQ).


possibleSecretFromEvaluations(GText):-
    qpairListLabel(GQ,GText),
    getEvaluationsQ(EvaluationsQ),
    constraintSecretFromEvaluationsQ(GQ,EvaluationsQ).


guessNearestSecretFromEvaluations(GText,GStart):-
    getEvaluationsQ(EvaluationsQ),
    qpairList(GQ),
    qpairListLabel(GQ0,GStart),
    constraintSecretFromEvaluationsQ(GQ,EvaluationsQ),
    diffCodeCH(GQ,GQ0,Diff),
    labeling([min(Diff),bisect],[Diff|GQ]),
    traceMastermind(solver(diff(Diff))),
    qpairListLabel(GQ,GText).

guessFarthestSecretFromEvaluations(GText,GStart):-
    getEvaluationsQ(EvaluationsQ),
    qpairList(GQ),
    qpairListLabel(GQ0,GStart),
    constraintSecretFromEvaluationsQ(GQ,EvaluationsQ),
    diffCodeCH(GQ,GQ0,Diff),
    labeling([max(Diff),enum],[Diff|GQ]),
    traceMastermind(solver(diff(Diff))),
    qpairListLabel(GQ,GText).

guessPossibleSecretFromEvaluations(GText):-
    getEvaluationsQ(EvaluationsQ),
    qpairList(GQ),
    constraintSecretFromEvaluationsQ(GQ,EvaluationsQ),
    labeling([ffc,enum],GQ),
    qpairListLabel(GQ,GText).



% not working
possibleResultFromEvaluationsQ(PQ,SQ,EvaluationsQ,NbBlack,NbWhite,NbBlue):-
    qpairList(SQ),
    constraintSecretFromEvaluationsQ(SQ,EvaluationsQ),
    evaluateCodeQ(PQ,SQ,NbBlack,NbWhite,NbBlue),
    append([NbBlack,NbWhite,NbBlue],SQ,Vars),
    labeling([ffc],Vars).

possibleResultFromEvaluations(PText,SText,NbBlack,NbWhite,NbBlue):-
        getEvaluationsQ(EvaluationsQ),
        qpairListLabel(PQ,PText),
        qpairList(SQ),
        constraintSecretFromEvaluationsQ(SQ,EvaluationsQ),
        evaluateCodeQ(SQ,PQ,NbBlack,NbWhite,NbBlue),
        qpairListLabel(SQ,SText).

possibleResultsFromEvaluations(PText,Results):-
    getEvaluationsQ(EvaluationsQ),
    qpairList(SQ),
    qpairListLabel(PQ,PText),
    findall(
        [SQ,NbBlack,NbWhite,NbBlue],
        possibleResultFromEvaluationsQ(PQ,SQ,EvaluationsQ,NbBlack,NbWhite,NbBlue),
        Results).


