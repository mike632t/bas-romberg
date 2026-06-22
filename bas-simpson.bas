100 REM SIMPSON RULE
110 DEF FNF#(X#) = EXP(X#)                      ' Function to be integrated

120 LET A# = -3#                                ' Lower bound of integration
130 LET B# = 3#                                 ' Upper bound of integration
140 LET M = 14                                  ' Maximum number of refinements  (Do not exceed 14)
150 LET T# = 1E-8                               ' Convergence tolerance

160 LET N = 2                                   ' Start with the minimum valid number of intervals (must be even)
170 LET H# = (B# - A#) / N

180 LET P# = FNF#(A#) + FNF#(B#) + 4# * FNF#(A# + H#)  ' Initial Simpson approximation using 2 intervals
190 LET P# = P# * H# / 3#

200 LET I = 0                                   ' Iteration counter
210 LET D# = 1

220 WHILE (I < M) AND (D# >= T#)                ' Stop when max iterations or limit reached
230   LET I = I + 1

240   LET N = N * 2                             ' Double number of intervals each iteration
250   LET H# = (B# - A#) / N                    ' Recompute step size

260   LET S0# = 0#
270   LET S1# = 0#

280   FOR J = 1 TO N - 1                        ' Evaluate function at each interval
290      X# = FNF#(A# + J * H#)
300      IF J MOD 2 = 0 THEN LET S1# = S1# + X# ELSE LET S0# = S0# + X#
310   NEXT J

320   LET C# = FNF#(A#) + FNF#(B#) + 4# * S0# + 2# * S1#  'Simpson's rule f(a) + f(b) + 4*odd + 2*even
330   LET C# = C# * H# / 3#

340   LET D# = ABS(C# - P#)                     ' Delta from previous iteration
350   LET P# = C#                               ' Update previous estimate 

360   PRINT USING "I=##     C= ######.#############"; I; C#
370 WEND

380 PRINT
390 IF D# <= T# THEN PRINT "Converged early" ELSE PRINT "Max iterations"
400 PRINT USING    "Integral =  ######.#############"; C#
410 END
