% Lesson 11: Error Handling in Prolog - catch/throw and failure

:- initialization(main).

% Safe division with catch
safe_divide(X, Y, Result) :-
    catch(
        Result is X / Y,
        error(evaluation_error(zero_divisor), _),
        (writeln('   Error: Division by zero'), fail)
    ).

% Throwing exceptions
divide(_, 0, _) :-
    throw(error(zero_divisor, 'Cannot divide by zero')).
divide(X, Y, Result) :-
    Result is X / Y.

% Using catch
compute(X) :-
    catch(
        divide(10, X, Result),
        error(zero_divisor, Msg),
        (format('   Caught: ~w~n', [Msg]), fail)
    ),
    format('   Result: ~w~n', [Result]).

main :-
    writeln('=== Error Handling in Prolog ===\n'),

    writeln('1. catch/throw:'),
    (safe_divide(10, 2, R) -> format('   divide(10, 2) = ~w~n', [R]) ; true),
    safe_divide(10, 0, _),

    writeln('\n2. Error handling:'),
    compute(2),
    compute(0),

    nl,
    writeln('=== Prolog Error Handling ==='),
    writeln('- catch/throw for exceptions'),
    writeln('- Logical failure'),
    writeln('- Error terms'),

    halt(0).

:- catch(main, E, (writeln(E), halt(1))).
