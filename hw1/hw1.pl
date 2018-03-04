% 1. domácí úloha
%
% a) Implementujte logaritmus o základu 2 (dolní celou část) na unárně
% reprezentovaných číslech.
% 
% logtwo(+N, ?Vysledek)

unary(0, 0) :- !.
unary(N, s(R)) :- N2 is N-1, unary(N2, R).

halve(0, 0).
halve(s(0), 0).
halve(s(s(X)), s(Y)) :- halve(X, Y).

logtwo(s(0), 0).
logtwo(X, s(R)) :- X \= 0, halve(X, H), logtwo(H, R).

% b) Implementujte predikát, který spočte n-té Fibonacciho číslo lépe než
% v exponenciálním čase (ideálně pouze lineárně mnoho sčítání).
%
% fib(+N, ?Vysledek)

fib2(A,_,0,A).
fib2(A,B,N,R) :- N>0, N2 is N-1, AplusB is A+B, fib2(B,AplusB,N2,R).

% Indexujeme od nuly a zaciname 0 1, tedy
% f_0=0, f_1=1, f_2=1, f_3=2, f_4=3, f_5=5 ...
fib(N,R) :- fib2(0,1,N,R).

%
% c) (BONUSOVÁ ÚLOHA) Implementuje predikát pro sčítání dvou binárních čísel.
%
% Můžete použít např. následující reprezentaci:
%
% 13[dec] = 1101[bin] = b(1, b(0, b(1, b(1, e))))
%
% Příklad použití:
% addBin(b(1, b(0, b(1, e))), b(1, b(1, b(0, b(1, e)))), R).
% R = b(0, b(0, b(0, b(0, b(1, e))))).
%
% resp.
%
% addBin([1, 0, 1], [1, 1, 0, 1], R).
% R = [0, 0, 0, 0, 1]. 

% Pricitani jednicky
addc([], [], 0, []) :- !.
addc([], [], 1, [1]) :- !.
addc(AL, [], C, RL) :- addc(AL, [0], C, RL).
addc([], BL, C, RL) :- addc(BL, [], C, RL).
% Scitani dvou cisel
addc([A|AS], [B|BS], C, [R|RS]) :- R is (A+B+C) mod 2, C2 is (A+B+C) div 2, addc(AS, BS, C2, RS).

% Bez parametru na carry
addBin(AL,BL,R) :- addc(AL,BL,0,R).
