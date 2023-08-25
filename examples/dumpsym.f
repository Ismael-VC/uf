' noop is prompt
\ An example of a simple command line tool written in UF
\
\ This program dumps an uxntal .sym file. You build the
\ ROM like this:
\
\   $ uxncli uf.rom <examples/dumpsym.f
\
\ and use it like this:
\
\   $ uxncli dumpsym.rom kernel.rom.sym

: h2.  ( u -- ) dup 16 u<  if  [char] 0  emit  then
  hex  (u.) type  decimal ;
: h4.  ( u -- ) dup 8 rshift h2.  255 and  h2. ;

: typesym  ( a1 -- a2 )
  begin  count ?dup  while  emit  repeat ;

: dumpsym  ( a u -- )
  filename  pad unused 2/ fileread 
  dup 0= if  ." can not read file" cr  1 halt  then
  pad swap bounds 
  begin  2dup >  while
    @+ h4.  space  typesym cr
  repeat ;

\ `query` reads standard input (or the first command line argument)
\ into the TIB, `0 parse` parses the whole string and returns address
\ and length:
: boot  query  0 parse  dumpsym  0 halt ;

\ Now save and exit
save dumpsym.rom
cr
bye
