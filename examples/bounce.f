' noop is prompt

\ bouncing balls
\
\ start like this to create a standalone .rom file:
\ uxncli ufx.rom <examples/bounce.f
\ uxnemu bounce.rom

1000 constant #balls

begin-structure ball
    cfield: ball-color
    field: ball-x       field: ball-y
    field: ball-dx      field: ball-dy
end-structure

create bitmap  hex
    3c c, 7e c, ff c, ff c, ff c, ff c, 7e c, 3c c,
decimal

#balls ball * buffer: balls
: select  ( u -- a ) ball * balls + ;
: negate!  ( a -- ) dup @ negate swap ! ;
: check-x  ( a n -- a )
  dup 1 u<  if  drop  1  over ball-dx negate!  then
  dup 504 u>  if  drop 504  over ball-dx negate!  then 
  over ball-x ! ;
: check-y  ( a n -- a )
  dup 1 u<  if  drop  1  over ball-dy negate!  then
  dup 312 u>  if  drop  312  over ball-dy negate!  then
  over ball-y ! ;
: draw  ( c a -- )
  dup ball-x @ swap ball-y @ position  
  bitmap spritedata  sprite ;
: move
  #balls  0  do  
    i select 0 over draw
    dup ball-dx @ over ball-x @ + check-x
    dup ball-dy @ over ball-y @ + check-y
    dup ball-color @ swap draw
  loop  brk ;
: n0  ?dup 0=  if  1  then ;
: init  randomize
  #balls  0  do  
    i select >r 
    rnd 7 umod 3 - n0 r@ ball-dx !
    rnd 7 umod 3 - n0 r@ ball-dy !
    rnd 500 umod 1+ r@ ball-x !
    rnd 300 umod 1+ r@ ball-y !
    rnd 3 umod 1+ r> ball-color !
  loop ;
: wait  jkey 27 =  if  ['] noop dup jvector svector  
  else  brk  then ;
: bounce  ['] move svector  ['] wait jvector  brk ;
init
true [if]
: boot  
    h# 0b75 h# 0da6 h# 0db8 colors
    bounce  bye ;
save bounce.rom
bye
[then]
.( enter "bounce" to start ... )
