' noop is prompt

\ very simple assembler example 

\ Return index of topmost bit
code bitcount8 ( x^ -- n^ )
        0 # SWP ( n x )
        1 &
        DUP 0 # EQU ( n x x=0 )
        2 , JCN ( n x )
        1 # SFT ( n x>>1 )
        SWP INC SWP ( n+1 x>>1 )
        1 , JMP
        2 &
        POP ( n )
        JMP " r
end-code

h# 39 bitcount8 6 <> [if] .( failed )  cr  1 halt  [then]

see bitcount8
