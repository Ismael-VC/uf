\ mostly taken from:
\ https://web.archive.org/web/20181118215700/http://excamera.com/files/j1demo/docforth/nuc.fs.html

: d=                        ( a b c d -- f )
    >r                      \ a b c
    rot xor                 \ b a^c
    swap r> xor             \ a^c b^d
    or 0= ;
: d+                        ( augend . addend . -- sum . )
    rot + >r                ( augend addend)
    over +                  ( augend sum)
    dup rot                 ( sum sum augend)
    u< if                   ( sum)
        r> 1+
    else
        r>
    then ;                        ( sum . )
: s>d dup 0<  if  -1  else  0  then ;
: dnegate
    invert swap invert swap
    1 0 d+ ;
: dabs ( d -- ud ) dup 0< if dnegate then ;
: d- dnegate d+ ;
: d<            \ ( al ah bl bh -- flag )
    rot         \ al bl bh ah
    2dup =
    if
        2drop u<
    else
        2nip >
    then ;
: d0= or 0= ;
: d0< 0 0 d< ;
: d2* 2dup d+ ;
: d2/ dup 31 lshift >r 2/ swap 2/ r> or swap ;
: dmax 2over 2over d< if 2swap then 2drop ;
: dmin 2over 2over d< 0= if 2swap then 2drop ;

variable scratch
: um*  ( u1 u2 -- ud )
    scratch !
    0 0
    16 0 do
        2dup d+
        rot dup 0< if
            2* -rot
            scratch @ 0 d+
        else
            2* -rot
        then
    loop
    rot drop ;
: abssgn    ( a b -- |a| |b| negf )
    2dup xor 0< >r abs swap abs swap r> ;
: m*  abssgn >r um* r> if dnegate then ;
: divstep
    ( divisor dq hi )
    2*
    over 0< if 1+ then
    swap 2* swap
    rot                     ( dq hi divisor )
    2dup < 0= if
        tuck                ( dq divisor hi divisor )
        -
        swap                ( dq hi divisor )
        rot 1+              ( hi divisor dq )
        rot                 ( divisor dq hi )
    else
        -rot
    then ;

: um/mod ( ud u1 -- u2 u3 )
    -rot  16 0 do  divstep  loop
    rot drop swap ;
: sm/rem ( d n -- r q )  ( symmetric )
  over >r >r  dabs r@ abs um/mod
  r> r@ xor 0< if negate then  r> 0< if >r negate r> then ;
: */mod >r m* r> sm/rem ;
: */    */mod nip ;
: tf  if  -1  else  0  then ;
: t2*  over >r >r d2* r> 2* r> 0< tf 1 and + ;

variable divisor
: m*/mod
    divisor !
    tuck um* 2swap um*   ( hi. lo. )
                         ( m0 h l m1 )
    swap >r 0 d+ r>   ( m h l )
    -rot                 ( l m h )
    16 0 do
        t2*
        dup divisor @ >= if
            divisor @ -
            rot 1+ -rot
        then
   loop ;
: m*/  ( d1 n1 +n2 -- d2 ) m*/mod drop ;

\ double printing
: (digit)  ( u -- c ) 9 over < 7 and + [char] 0 + ;
: (d#)  ( d1 -- d2 )
    0 base @ um/mod >r base @ um/mod swap (digit) hold r> ;
: d#s ( d1 -- d2 )  begin  (d#) 2dup or 0=  until ;
: d#> ( d -- a u )  drop #> ;
: (d.)  ( d -- a n ) 2dup dabs <# d#s 2swap nip sign d#> ;
: d.  ( n -- ) (d.) type space ;
