:- module(mastermindserver, [start_server/0]). 
:- public start_server/0.

:- use_module('./mastermind.pl').  

:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_cors)).
:- use_module(library(debug)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_path)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_cors)).
:- use_module(library(time)).



server(Port) :-
    http_server(http_dispatch, [port(Port)]).

:- dynamic server_thread/1.

start_server:-
    start_server(8080).
start_server(Port) :-
    (   
        server_thread(ThreadId)
        ->  format('Server is already running in thread ~w~n', [ThreadId])
        ;(
            thread_create(server(Port), ThreadId, []),
            assertz(server_thread(ThreadId)),
            format('Server started on port ~w in thread ~w~n', [Port, ThreadId])
        )
    ).

stop_server :-
    (   retract(server_thread(ThreadId))
    ->  thread_signal(ThreadId, throw(stop_server)),
        thread_join(ThreadId, _),
        ignore((
            http_stop_server(P2, []),
        format('Server stopped port=~w~n',P2)
            )),
		%http_close_keep_alive(_),
        format('Server stopped~n')
    ;   format('No server is currently running~n')
    ).

user:file_search_path(web_dist, Dir) :-
        web_dist(Dir).
    
http:location(api, root(api), []).
    
web_dist(DistPath):-
        source_file(start_server,Path),
	    debug(http,"source path: ~w~n",[Path]),
        directory_file_path(Dir, _, Path),
	    directory_file_path(Base, _, Dir),
        directory_file_path(Base, 'web/dist', DistPath),
        debug(http,"web_dist(~w)~n",[DistPath]).

:- web_dist(X),format("web_dist(~w)~n",X).



api_http_handler(Path, Pred, Methods, Options):-
    %debug(http,"api_http_handler(Path=~w,Pred=~w,Methods=~w,Options=~w)",[Path,Pred,Methods,Options]),
    http_handler(
        Path,
        {Pred}/[Request]>>(
            %debug(http,"lambda handle_api(Request=~w,Pred=~w)",[Request,Pred]),
            handle_api(Request,Pred)),
        [methods([options|Methods])|Options]).



:- api_http_handler(api(choose_secret_random), handle_choose_secret_random, [post],[]).
:- api_http_handler(api(choose_secret), handle_choose_secret, [post],[]).
:- api_http_handler(api(evaluate_code), handle_evaluate_code, [post],[]).
:- api_http_handler(api(list_evaluations), handle_list_evaluations, [get],[]).
:- api_http_handler(api(clear_evaluations), handle_clear_evaluations, [post],[]).
:- api_http_handler(api(clear_evaluation), handle_clear_evaluation, [post],[]).
:- api_http_handler(api(guess_code), handle_guess_code, [post],[]).
:- api_http_handler(api(add_evaluation), handle_add_evaluation, [post],[]).
:- api_http_handler(api(possible_code), handle_possible_code, [post],[]).
:- api_http_handler(api(guess_nearest_code), handle_guess_nearest_code, [post],[]).
:- api_http_handler(api(guess_farthest_code), handle_guess_nearest_code, [post],[]).
:- api_http_handler(api(guess_possible_code), handle_guess_possible_code, [post],[]).
:- api_http_handler(api(get_code), handle_get_code, [get],[]).


:- http_handler(root(.), http_reply_from_files(web_dist(.), []), [prefix,method(get)]).

%:- set_setting(http:cors, [*]).
handle_api(Request,Pred):-
    %debug(http,"handle_api(Request=~w,Pred=~w)",[Request,Pred]),
    member(method(options),Request)
    ->(handle_options(Request))
    ;(
        %debug(http,"handle for: ~w",[Pred]),
        (
            member(content_type('application/json'),Request)
            ->
            (
                http_read_json(Request, JSONIn),
                debug(http,"jsonIn ~w",[JSONIn])
            )
            ;
            (
                JSONIn=[],
                debug(http,"no jsonIn ~w",[JSONIn])
            )
        ),
        emitCors(Request),
        time(call(Pred,JSONIn,JSONOut,Status)),
        debug(http,"status=~w, jsonOut=~w",[Status, JSONOut]),
        reply_json(json([status=Status|JSONOut]))
    ).




emitCors(Request):-
    %debug(http,"emitCors(~w)",[Request]),
    (
        member(origin(Origin),Request)
        ->
        cors_enable,
        %debug(http,"emitCors(Origin=~w)",[Origin]),
        format('Access-Control-Allow-Origin: ~w~n',[Origin]),
        format('Access-Control-Allow-Methods: GET, PUT, DELETE, POST, OPTIONS~n'),
        format('Access-Control-Allow-Headers: Content-Type~n')
        ;
        %debug(http,"emitCors(NoOrigin)",[]),
        true
    ).

handle_options(Request) :-
        %debug(http,"handle_options(~w)",[Request]),
        (
            member(origin(Origin),Request)
            ->
            %cors_enable(Request,[]),
            %debug(http,"handle_options(Origin=~w)",[Origin]),
            format('Access-Control-Allow-Origin: ~w~n',[Origin]),
            format('Access-Control-Allow-Methods: GET, PUT, DELETE, POST, OPTIONS~n'),
            format('Access-Control-Allow-Headers: Content-Type~n')
            ;
            %debug(http,"handle_options(NoOrigin)",[]),
            true
        ),
        %debug(http,"handle_options(common)",[]),
        format('Allowed: OPTIONS, GET, POST, PUT, DELETE~n'),
        format('Content-Type: text/plain~n~n'),
        format('OK~n'),
        %debug(http,"handle_options(ok)",[]),
        true.

% Handler for chooseSecret without parameters
handle_choose_secret_random(_,JSONOut,0) :-
    debug(http,"handle_choose_secret_random",[]),
    chooseSecret,
    theSecret(Code),
    debug(http,"code ~w",[Code]),
    convert_json_code(json(JSONOut), Code )
    .

% Handler for chooseSecret with parameters
handle_choose_secret(JSONIn,[],0) :-
    debug(http,"handle_choose_secret",[]),
    convert_json_code(JSONIn, Code ),
	debug(http,"code ~w",[Code]),
    chooseSecret(Code).

% Handler for get Secret
handle_get_code(_,JSONOut,0) :-
    debug(http,"handle_get_code",[]),
    theSecret(Code)->(
        debug(http,"code ~w",[Code]),
	    convert_json_code(json(JSONOut), Code ));(
            JSONOut=[]
        ).
% Handler for evaluateCodeOnSecret
handle_evaluate_code(JSONIn,[nbBlack=NbBlack, nbWhite=NbWhite, nbBlue=NbBlue],0) :-
	debug(http,"handle_evaluate_code",[]),
    convert_json_code(JSONIn, Code ),
	debug(http,"code ~w",[Code]),
    evaluateCodeOnSecret(Code, NbBlack, NbWhite, NbBlue),
	debug(http,"evaluation ~w ~w ~w",[NbBlack,NbWhite,NbBlue]).

% Handler for getEvaluations
handle_list_evaluations(_,JSONOut,0) :-
    debug(http,"handle_list_evaluations",[]),
    getEvaluations(Evaluations),
	debug(http,"evaluations ~w",[Evaluations]),
	convert_json_evaluations(JsonEvaluations, Evaluations),
	debug(http,"jsonevaluations ~w",[JsonEvaluations]),
	JSONOut=[evaluations=JsonEvaluations].

% Handler for clearEvaluations
handle_clear_evaluations(_,[],0) :-
    debug(http,"handle_clear_evaluations",[]),
    clearEvaluations.

% Handler for clearEvaluation
handle_clear_evaluation(JSONIn,[],0) :-
    debug(http,"handle_clear_evaluation",[]),
    convert_json_code(JSONIn, Code ),
	debug(http,"code ~w",[Code]),
    clearEvaluation(Code).



	

% Handler for guessSecretFromEvaluations
handle_guess_code(_,JSONOut,0) :-
    debug(http,"handle_guess_code",[]),
    guessSecretFromEvaluations(Code),
	debug(http,"code ~w",[Code]),
	convert_json_code(json(JSONOut), Code ).

% Handler for guessSecretFromEvaluations
handle_guess_possible_code(_,JSONOut,0) :-
    debug(http,"handle_guess_possible_code",[]),
    guessPossibleSecretFromEvaluations(Code),
	debug(http,"code ~w",[Code]),
	convert_json_code(json(JSONOut), Code ).

% Handler for addEvaluation
handle_add_evaluation(JSONIn,[],0) :-
    debug(http,"handle_add_evaluation",[]),
    JSONIn=json([evaluation=JSONEval]),
    convert_json_evaluation(JSONEval, Evaluation ),
	debug(http,"code ~w",[Evaluation]),
	Evaluation=(Code,NbBlack,NbWhite,NbBlue),
    addEvaluation(Code,NbBlack,NbWhite,NbBlue).


% Handler for possible_code
handle_possible_code(JSONIn,[],Status) :-
    convert_json_code(JSONIn, Code ),
	debug(http,"code ~w",[Code]),
    (    possibleSecretFromEvaluations(Code) -> Status=0 ; Status=1).


guessRelativeSecretFromEvaluationsWithTimeout(Code,Code0,Timeout,Status,Predicate):-
            catch(
                call_with_time_limit(Timeout,
                    {Code,Code0,Status,Predicate}/[]>>(
                        call(Predicate,Code,Code0),
                        Status=0
                    ),
                    guessRelativeSecretFromEvaluations(Code0)
                    )->Status=0;(Status=1,Code=[]),
                E,
                {E,Code,Status}/[]>>(
                    debug(mastermind,"guessRelativeSecretFromEvaluationsWithTimeout ~w~n",E)
                    ,Code=[],Status=2) ).



handle_guess_nearest_code(JSONIn,JSONOut,Status) :-
    debug(http,"handle_guess_nearest_code",[]),
    convert_json_code(JSONIn, Code0 ),
	debug(http,"code0 ~w",[Code0]),
    guessRelativeSecretFromEvaluationsWithTimeout(Code,Code0,10,Status,guessNearestSecretFromEvaluations), 
    debug(http,"code ~w, status=~w",[Code,Status]),
    (    
        Code\=[]->(
            convert_json_code(json(JSONOut), Code )
        )
        ;
        (
            JSONOut=[]
        )
    ).

handle_guess_farthest_code(JSONIn,JSONOut,Status) :-
        debug(http,"handle_guess_farthest_code",[]),
        convert_json_code(JSONIn, Code0 ),
        debug(http,"code0 ~w",[Code0]),
        guessRelativeSecretFromEvaluationsWithTimeout(Code,Code0,10,Status,guessFarthestSecretFromEvaluations), 
        debug(http,"code ~w, status=~w",[Code,Status]),
        (    
            Code\=[]->(
                convert_json_code(json(JSONOut), Code )
            )
            ;
            (
                JSONOut=[]
            )
        ).
    
       
    

% Helper predicate to convert JSON to Prolog terms

convert_json_code(JSONIn, Code) :-
    JSONIn=json([code=CodeJson]),
    maplist(convert_json_pair, CodeJson, Code).

convert_json_pair(json([color=Color, shape=Shape]), (Color, Shape)).


convert_json_evaluations(JSONIn, Evaluations ):-
    maplist(convert_json_evaluation, JSONIn , Evaluations).

convert_json_evaluation(
	json([code=CodeJson,nbBlack=NbBlack, nbWhite=NbWhite, nbBlue=NbBlue]),
	(Code,NbBlack,NbWhite,NbBlue) ):-
    maplist(convert_json_pair, CodeJson, Code).

    
