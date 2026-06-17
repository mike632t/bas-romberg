100 REM ROMBERG INTEGRATION
110 DEF FNF#(X#) = 1/X#                         ' Function to be integrated
120 DIM R#(12,12)

130 LET A# = 1#                                 ' Lower limit
140 LET B# = 9#                                 ' Upper limit
150 LET T# = .000005                            ' Required error

160 LET H# = B# - A#                            ' Initial step size 
170 LET S0# = FNF#(A#) + FNF#(B#)               ' Initial endpoint sum
180 LET N = 1                                   ' Initial step size

190 LET I = 0
200 LET R#(0,0) = S0# * H0# / 2#                ' First trapezoid value

210 LET DIFF# = 1#                              ' Set initial difference to be greater then limit 
240 WHILE (I < 12) AND (DIFF# >= T#)            ' Stop when max iterations or limit reached
250    LET I = I + 1                            ' Number of iterations
260    LET N = 2 * N                            ' Double number of intervals each iteration
270    LET H# = H# / 2#                         ' New step size 
280    LET S# = S0# / 2#                        ' Start with half f(a) + f(b)

290    FOR J = 1 TO N - 1                       ' Compute interior points
300       LET S# = S# + FNF#(A# + J * H#)
310    NEXT J

320    LET R#(I,0) = S# * H#                    ' Initial estimate 
330    LET F# = 1#
340    FOR K = 1 TO I                           ' Find Richardson extrapolation R[i,k] = (4^k * R[i,k-1] - R[i-1,k-1]) / (4^k - 1)
350       LET F# = 4# * F#                      ' fa = 4, 16, 64, ... 
360       LET R#(I,K) = (F# * R#(I,K-1) - R#(I-1,K-1)) / (F# - 1#)  
370    NEXT K

380    LET DIFF# = ABS(R#(I,I) - R#(I-1,I-1))   ' Difference from last estimate 
390    PRINT USING "I=##     R= ######.#############"; I; R#(I,I)
400 WEND

410 PRINT
420 IF DIFF# < T# THEN PRINT "Converged early" ELSE PRINT "Max iterations"
430 PRINT USING    "Integral =  ######.#############"; R#(I,I)
440 END
