#1

Conflict SHIFT-REDUCE

The first conflict is illustrated with the following:

LETFUN f p:t term1 = term2 (whitespace)

which can be reduced to term

LETFUN f p:t term1 = term2 (whitespace)

which can lead to a reduction to an application to term2, i.e.:

LETFUN f p:t term1 = term2 (whitespace) term3.

Correction:

Give parentheses to application: a1 a2 thus needs to be enclosed in parentheses as (a1 a2).

#2

