\ local variables
\
\ This is a hack. Space for headers of local variable accessors
\ is not recovered, and only the basic "{ ... }" sequence is
\ supported.
\
\ usage:
\
\ : foo  { a b -- sum } a b + ;
\
\ Everything after "--" is ignored up to the next `}` and be be
\ omitted. Multiple local variable declarations are allowed but
\ are not scoped by control-structures - they are visible until the
\ end of the definition is reached, either by `;` or `-;`.
\
\ Local variables are stored in the `locals` vocabulary, which
\ is active, during compilation (and thus needs a slot on the
\ vocabulary stack). After the definition is ended, the 
\ `locals` vocabulary is cleared.

vocabulary locals
also locals  vocs @ constant lvoc  previous

: ?locals  vocs @ lvoc <> if  also locals  then ;

: unlocals  lvoc off
  vocs @ lvoc = if  previous  then ;

: lfinish  ( a1 pfa1 ... u -- ) here >r
  0  ?do  literal  postpone !  loop  r> swap ! ;

: skip}  ( | ... } -- ) [char] } parse 2drop ;

: head1  ( a u -- pfa )
  significant here dup>r place  here c@ 1+ allot
  lvoc @ ,  r> lvoc !
  ['] (constant) compile,  here  0 , ;

: {  ( ... | local ... -- ... } -- )
  ?locals  ['] (else) compile,  here  0 ,
  0  begin  bl word count 2dup
    s" --" compare 0=  if  2drop  skip}  lfinish  |
    2dup s" }" compare 0=  if  2drop  lfinish  |
    head1  swap 1+
  again ; immediate

: -;  unlocals  postpone -; ; immediate
: ;  unlocals  postpone ; ; immediate
