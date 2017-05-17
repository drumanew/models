-ifndef (__LOG_INCLUDED).
-define (__LOG_INCLUDED, true).

-define (LOG (F, A),
         io:format(standard_error, "[~s:~b] " ++ F ++ "~n", [?MODULE, ?LINE | A])).
-define (LOG (What), ?LOG(What, [])).

-endif. %% __LOG_INCLUDED
