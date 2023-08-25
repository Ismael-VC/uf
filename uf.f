\ UF - Forth part
\
\ Once the kernel runs, it can be used to load and interpret this file, which
\ adds more words and saves various ROM files during the process, resulting
\ in Forth interpreters with varying functionality.
\
\ Stack comments have the same meaning as in "kernel.tal". Note that
\ the "(...)" Forth comment syntax is not yet available and will be added
\ below.
\
\ `literal` ( x -- ) Compile code to push a 16-bit value using LIT2k
: literal  160 c, , ;

\ Create next definitions in `compiler` vocabulary. During compilation
\ these immediate words generate inline UXN code instead of compiling to 
\ procedure calls as is done for "normal" Forth words.
\ You can use it for your own optimizations: just define an immediate
\ word in the `compiler` vocabulary in addition to your normal non-immediate
\ word.
\
\ (idiom) `also` duplicates item on the vocabulary stack, `compiler` changes
\ topmost item to the `compiler` vocabulary, `definitions` makes the topmost
\ entry (now `compiler`) the one where future definitions are added.
also compiler definitions

\ `drop` ( x -- ) Pop topmost stack element, compiles a POP2
: drop  34 c, ; immediate

\ `nip` ( x y -- y ) Pop second stack element, compiles a NIP2
: nip  35 c, ; immediate

\ `swap` ( x y -- y x ) Swap stack elements, compiles SWP2
: swap  36 c, ; immediate

\ `rot` ( x y z -- y z x ) Rotate 3rd item to the top, compiles to ROT2
: rot  37 c, ; immediate

\ `-rot` ( x y z -- z x y ) Rotate top item to 3rd place, compiles to ROT2 ROT2
: -rot  37 c,  37 c, ; immediate

\ `dup` ( x -- x x) Duplicate top item, compiles to DUP2
: dup  38 c, ; immediate

\ `over` ( x y -- x y x ) Copies 2nd item to the top, compiles to OVR2
: over  39 c, ; immediate

\ `2dup` ( x y -- x y x y ) Duplicate 2 topmost items, compiles to OVR2 OVR2
: 2dup  39 c,  39 c, ; immediate

\ `2drop` ( x y -- ) Drops 2 topmost items, compiles to POP2 POP2
: 2drop  34 c, 34 c, ; immediate

\ `+` ( n1 n2 -- n3 ) Add 2 items, compiles to ADD2
: +  56 c, ; immediate

\ `-` ( n1 n2 -- n3 ) Subtract top item from the 2nd, compiles to SUB2
: -  57 c, ; immediate

\ `*` ( n1 n2 -- n3 ) Multiply, compiles to MUL2
: *  58 c, ; immediate

\ `u/` ( u1 u2 -- u3 ) Unsigned division, compiles to DIV2
: u/  59 c, ; immediate

\ `and` ( x1 x2 -- x3 ) Bitwise AND, compiles to AND2
: and  60 c, ; immediate

\ `or` ( x1 x2 -- x3 ) Bitwise OR, compiles to ORA2
: or  61 c, ; immediate

\ `xor` ( x1 x2 -- x3 ) Bitwise XOR, compiles to EOR2
: xor  62 c, ; immediate

\ `=` ( x1 x2 -- f ) Pushes 1 if x1 is equal to x2 or 0 otherwise, compiles to EQU2 DUP
: =  40 c, 6 c, ; immediate

\ `<>` ( x1 x2 -- f ) Pushes 1 if x1 is not equal to x2 or 0 otherwise, compiles 
\   to NEQ2 DUP
: <>  41 c, 6 c, ; immediate

\ `u>` ( u1 u2 -- f ) Pushes 1 if u1 is higher than u2 or 0 otherwise, compiles 
\   to GTH2 DUP
: u>  42 c, 6 c, ; immediate

\ `u<` ( u1 u2 -- f ) Pushes 1 if u1 is below u2 or 0 otherwise, compiles to LTH2 DUP
: u<  43 c, 6 c, ; immediate

\ `0<` ( n -- f ) Pushes $8000 if n is negative or 0 otherwise, compiles to #15 SFT2
: 0<  128 c, 15 c, 63 c, ; immediate

\ `>r` ( x -- ) Pop a value from the data stack and pushes it onto the return stack,
\   compiles to STH2
: >r  47 c, ; immediate

\ `dup>r` ( x -- x ) Push the top item from the data stack and pushes it onto the 
\   return stack, compiles to STH2k
: dup>r  175 c, ; immediate

\ `r>` ( -- x ) Pops the top item from the return stack and pushes it onto the data
\   stack, compiles to STH2r
: r>  111 c, ; immediate

\ `r>drop` ( -- ) Drops the top item from the return stack, compiles to POP2r
: r>drop  98 c, ; immediate

\ `r@` ( -- x ) Pushes the top item from the return stack onto the data stack,
\   compiles to STH2kr
: r@  239 c, ; immediate

\ `1+` ( n1 -- n2 ) Adds 1 to the topmost stack item, compiles to INC2
: 1+  33 c, ; immediate

\ `1-` ( n1 -- n2 ) Subtracts 1 to the topmost stack item, compiles to #0001 SUB2
: 1-  160 c, 1 , 57 c, ; immediate

\ `cell+` ( n1 -- n2 ) Adds 2 to the topmost stack item, compiles to INC2 INC2
: cell+  33 c, 33 c, ; immediate

\ `exit` ( -- ) Returns from the current word, compiles to JMP2r
: exit  108 c, ; immediate

\ `@` ( a -- x ) Fetches the short from the given address and pushes it on the data stack,
\   compiles to LDA2
: @  52 c, ; immediate

\ `!` ( x a -- ) Stores the 2nd item from the stack at the address at the top of the
\   stack, compiles to STA2
: !  53 c, ; immediate

\ `c@` ( a -- c ) Fetches a byte, compiles to LDA #00 SWP
: c@  20 c, 128 c, 0 c, 4 c, ; immediate

\ `c!` ( c a -- ) Stores a byte, compiles to STA POP
: c!  21 c, 2 c, ; immediate

\ `2*` ( n1 -- n2 ) Multiples by 2, compiles to #10 SFT2
: 2*  128 c, 16 c, 63 c, ; immediate

\ `2/` ( n1 -- n2 ) Divides by 2, compiles to #1 SFT2
: 2/  128 c, 1 c, 63 c, ; immediate

\ `cells` ( n1 -- n2 ) Multiples by size of short (i.e. 2), same as `2*`
: cells  128 c, 16 c, 63 c, ; immediate

\ `2>r` ( x y -- ) Takes 2 values from the data stack and pushes them on the return
\   stack, compiles to SWP2 STH2 STH2
: 2>r  36 c,  47 c,  47 c, ; immediate

\ `2r>` ( -- x y ) Takes 2 values from the return stack and pushes them on the data
\   stack, compiles to STH2r STH2r SWP2
: 2r>  111 c,  111 c,  36 c, ; immediate

\ `execute` ( xt -- ) Calls a word (any UXN code, in fact), compiles to JSR2
: execute  46 c, ; immediate

\ `brk` ( -- ) Stops UXN VM, waiting for events, compiles to BRK
: brk  0 c, ; immediate

\ `[_']` ( | <word> -- xt ) Compiles the address of the next word in the input stream
\   as a literal value, compiles to LIT2k
: [_']  ' literal ; immediate

\ `[']` ( | <word> -- xt ) Similar to `[_']`, but ignores the topmost entry in the
\   vocabulary stack; this is done to make `[']` to skip the `compiler` vocabulary -
\   when we want to fetch word addresses during compile time, we want the real Forth
\   word addresses, not the immediate code generators that compile code inline.
: [']  vocs @ >r  null vocs !  ' literal  r> vocs ! ; immediate

\ We are done with `compiler` words, now switch back to the `forth` vocabulary
only definitions

\ `on` ( a -- ) Store -1 at the given address
: on  -1 swap ! ;

\ `off` ( a -- ) Store 0 at the given address
: off  0 swap ! ;

\ The following are just "proper" variants of the inline words defined earlier in
\ the `compiler` vocabulary, these are invoked in interpreted mode or when referred
\ to via `[']`/`'`.
: +  + ;
: -  - ;
: *  * ;
: u/  u/ ;
: u<  u< ;
: u>  u> ;
: r>  r> ;
: >r  >r ;
: dup>r  dup>r ;
: r>drop  r>drop ;
: r@  r@ ;
: c!  c! ;
: c@  c@ ;
: ! ! ;
: @ @ ;
: and  and ;
: or  or ;
: xor  xor ;
: =  = ;
: <>  <> ;
: drop  drop ;
: nip  nip ;
: dup  dup ;
: swap  swap ;
: rot  rot ;
: over  over ;
: 1+  1+ ;
: 1-  1- ;
: 2*  2* ;
: 2/  2/ ;
: 0<  0< ;
: 2r>  2r> ;
: 2>r  2>r ;
: -rot  -rot ;
: 2drop  2drop ;
: 2dup  2dup ;
: brk  brk ;
: cells  cells ;
: cell+  cell+ ;

\ `0=` ( x -- f ) Compares with zero
: 0=  0 = ;

\ `>=` ( n1 n2 - f ) Push 1 if first argument is greater or equal to second or 0 otherwise
: >=  1- swap < ;

\ `<=` ( n1 n2 - f ) Push 1 if first argument is less or equal to second or 0 otherwise
: <=  1+ swap > ;

\ `2r@` ( -- x y ) Pushes 2 topmost items from the return stack on the data stack
: 2r@  r> 2r> over over 2>r >r ;

\ `here` ( -- a ) Push address of first unused byte
: here  h @ ;

\ `forth` ( -- ) Set topmost entry in vocabulary stack to default `forth` vocabulary
: forth  dp vocs ! ;

\ `]` ( | ... -- ) push `compiler` vocabulary on vocabulary stack and start compiling 
\   further words in input stream
: ]  also  compiler  (compile) ;

\ `invert` ( x1 -- x2 ) Invert bits of topmost stack item
: invert  -1 xor ;

\ `negate` ( n1 -- n2 ) Negate topmost stack item
: negate  0 swap - ;

\ `2nip` ( x y v w -- v w ) Drop 4d and 4th stack item
: 2nip  rot drop rot drop ;                  

\ `2swap` ( x y v w -- v w x y ) Swap topmost item pairs on stack
: 2swap  rot >r rot r> ;                 

\ `2over` ( x y v w -- x y v w x y ) Copy 2nd item pair to top
: 2over  >r >r 2dup r> -rot r> -rot ;

\ `2rot` ( x y v w p q -- v w p q x y ) Rotate item pairs
: 2rot  >r >r 2swap r> r> 2swap ;

\ `(` ( | ...) ) Skip characters in input stream until next ")"
: (  41 parse 2drop ; immediate

\ `char` ( | <word> -- c ) Parse next word in input stream and push the ASCII code
\   of its first character
: char  32 word 1+ c@ ;

\ `sliteral` ( -- a u ) compile a string literal into the code area, the string
\   will be embedded and skipped over, leaving address and length on the stack
: sliteral  ['] (slit) compile,  tuck here place  1+ allot ;

\ `unloop` ( -- ) pop 2 items from the return stack, making sure to keep the return 
\   address, used to exit prematurely out of DO ... LOOP constructs
: unloop  r>  r>drop r>drop  >r ;

\ `execute` ( xt -- ) `execute`, but making sure to keep the return address
: execute  r>drop  execute ;

\ `cr` ( -- ) Write a newline using `emit`
: cr  10 emit ;

\ Further words for the `compiler` vocabulary, mostly control structures, compiler
\ state manipulation and compiling inline literal
also compiler definitions

\ `[char]` ( | <word> -- c ) Generate code to push the first character of the
\   next word in the input stream at run time
: [char]  char literal ; immediate

\ `s"` ( | ..." -- a u ) Compile a string constant, pushing address and length
: s"  ( | ..." -- a u ) [char] " parse sliteral ; immediate

\ `."` ( | ..." -- ) Compile a string literal and write it out using `type`
: ."  ( | ..." -- ) 
  [char] " parse sliteral  ['] type compile, ; immediate

\ `[` ( -- ) Switch to interpreted state and change topmost vocabulary stack entry
\   to the `forth` vocabulary
: [  state off  forth ; immediate

\ `if` ( f | ... -- ) Conditional branch, compiles a call to `(if)` followed by the
\   address to branch to, initially 0, later patched by `else` or `then`. The patch
\   address will be kept on the data stack at compile time
: if  ['] (if) compile,  here  0 , ; immediate

\ `else` ( | ... -- ) Compile an unconditional branch and patch up the pending
\   jump from the previous `if`, keep new patch address on stack
: else  ['] (else) compile,  here  0 ,  swap here swap ! ; immediate

\ `then` ( -- ) Patch previous jump, completing the `if` sequence
: then  here swap ! ; immediate

\ `cjump,` ( a -- ) Compile a conditional backward jump (JCN[2]), used for loops
: cjump,  jumpaddr,  if  45  else  13  then  c, ;

\ `jump,` ( a -- ) Compile an unconditional jump (JMP[2])
: jump,  jumpaddr,  if  44  else  12  then  c, ;

\ `begin` ( -- ) Start loop, just holding address on data stack during compile time
: begin  here ; immediate

\ `again` ( -- ) Compile jump to address on data stack at compile time
: again  jump, ; immediate

\ `until` ( f -- ) Compile a conditional (absolute) jump to address pushed on data
\   stack by previous `begin` at compile time
: until  ['] (if) compile,  , ; immediate

\ `while` ( f -- ) Comple a conditional forward jump and leave patch address on data
\   stack for BEGIN ... WHILE ... REPEAT loop
: while  ['] (if) compile,  here  0 , ; immediate

\ `repeat` ( -- ) Patch up branch and compile backwards jump
: repeat  swap jump,  here swap ! ; immediate

\ `(?abort`) ( f a u -- ) If flag is true, show string using `type` and abort,
\   used for `abort"`
: (?abort)  rot  if  type  cr  abort  else  2drop  then ;

\ `abort"` ( f | ..." -- ) Show message and abort if flag is true
: abort"  [char] " parse sliteral ['] (?abort) compile, ; immediate

\ `|` ( -- ) Compile `exit` and patch existing jump address on data stack
: |  [_'] exit execute  here swap ! ; immediate

\ `->` ( x y | ... -- x ) Compare topmost stack entry with 2nd and skip code until
\   next `|` (or `then`) if they don't match, otherwise drop topmost item and
\   continue
: ->  [_'] over execute  [_'] = execute  ['] (if) compile,  here 0 , 
  [_'] drop execute ; immediate

\ `postpone` ( | <word> -- ) Look up the next word in the input stream and compile
\   it - it is immediate, compile a call, if not then compile code that compiles a
\   call
: postpone  also forth
  32 word find  previous  0  ->  undefd  |
  1  ->  compile,  |  drop  literal ['] compile, compile, ; 
  immediate

\ `(does>)` ( -- ) Change the code field of the most recently created word to
\   Jump to the code following this word; this requires that the code field
\   starts with a JSI sequence
: (does>)  r>  current @ @ count 63 and + 2 +  1+  tuck - 2 - swap ! ;

\ `does>` ( | ... -- a ) Compiles a call to `(does>)` followed by STH2r, effectively
\   changing the previously defined word to execute the following code, with the 
\   parameter field address pushed on the stack (of the data following the branch to 
\   the current location)
: does>  ['] (does>) compile,  111 c, ; immediate

\ `do` ( u1 u2 | ... loop -- ) Start a DO ... LOOP construct, pushing the start and
\   limit on the return stack, compiling SWP2 STH2 STH2; leaves a 0 and the current code
\   address on the data stack at compile time
: do  36 c, 47 c, 47 c,  0  here ; immediate

\ `?do` ( u1 u2 | ... loop -- ) Variant of `do` that checks limits before first
\   iteration, compiling SWP2 STH2 STH2 #0000 #0000 JMP2; the latter jump address
\   will be patched up to jump to the end of the loop before entering it the 
\   first time
: ?do  36 c, 47 c, 47 c,  0 literal  0 literal  here 2 -  44 c,  here ; immediate

\ `patchloop` ( f a -- a ) Patches up a forward jump if the flag is true
: patchloop  swap ?dup  if  here swap !  then ;

\ `+loop` ( u -- ) Patches up forward branch and compiles `(loop)` which increases
\   the loop index by u and branches back to the start of the loop if the limit is not
\   reached yet
: +loop  patchloop  ['] (loop) compile,  cjump, ; immediate

\ `loop` ( -- ) Like `+loop`, increasing the loop index by 1
: loop  1 literal  patchloop  ['] (loop) compile,  cjump, ; immediate

\ `-;` ( -- ) Changes last compiled instruction from call to jump,
\   changing JSI to JMI; if the previous instruction is not a call then
\   compile a JMP2r (normal return)
: tailjump  here 3 - dup c@  96 =  if  64 swap c!  |  drop  108 c, ;
: -;  current @ @ here <>  if  tailjump  else  108 c,  then
  state off  reveal ; immediate

\ back to `forth` vocabulary
only definitions

\ `constant` ( x | <word> -- ) Define a constant word in the dictionary
: constant  head ['] (constant) compile, , ;

\ `variable` ( | <word> -- ) Define a variable
: variable  head ['] (variable) compile, 0 , ;

\ `create` ( | <word> -- ) Create a header without any code (yet)
: create  head ['] (variable) compile, ;

\ `buffer:` ( u | <word> -- ) Create a variable header for a buffer of u bytes
: buffer:  create  allot ;

\ `false` ( -- 0 ) The global "false" value
0 constant false

\ `true` ( -- -1 ) The canonical "true" value
-1 constant true

\ `bl` ( -- 32 ) The space character constant
32 constant bl

\ `"` ( | ..." -- a u ) Define a string constant at interpretation time, this
\   is not for compiled code, use `s"` there!
: "  [char] " parse >r  here r@ cmove  here r> dup allot ;

\ `under+` ( n1 x n2 -- n3 x ) Add top item on data stack to the 3rd item
: under+  rot + swap ;

\ `th` ( a1 u -- a2 ) Compute address of u-th short at address a1
: th  2* + ;

\ `min` ( n1 n2 -- n1/2 ) Minimum value
: min  2dup <  if  drop  else  nip  then ;

\ `max` ( n1 n2 -- n1/2 ) Maximum value
: max  2dup >  if  drop  else  nip  then ;

\ `pad` ( -- a ) Temporary address as scratch memory, valid until the next `allot`,
\   `,`, `c,`
: pad  here 256 + ;

\ internal buffer for filenames
256 buffer: fnbuf

\ `filename` ( a u -- ) Set filename slot in file device to the string given, 
\   terminated by a zero character
: filename  255 min fnbuf place  0 fnbuf dup c@ + 1+ c! 
  fnbuf 1+ 168 deo2 ;

\ `filewrite` ( a u -- u2 ) Write buffer to file, return number of bytes written
: filewrite  ( a u -- u2 ) 0 167 deo  170 deo2  174 deo2 162 dei2 ;

\ `fileappend` ( a u -- u2 ) Append buffer to file, return number of bytes written
: fileappend  ( a u -- u2 ) 1 167 deo  170 deo2  174 deo2 162 dei2 ;

\ `fileread` ( a u -- u2 ) Read bytes from file, return number of bytes read
: fileread  ( a u -- u2 ) 170 deo2  172 deo2  162 dei2 ;

\ `filedelete` ( -- ) Delete file designated by filename device slot
: filedelete  1 166 deo ;

\ `saved` ( a1 u1 a2 u2 -- ) Write buffer at a1/u1 to file named by a2/u2 and
\   report error when writing was unsuccessful (0 bytes were written)
: saved  filename  filewrite 0= abort" saving file failed" ;

\ `save` ( | <word> -- ) Write area from 0100 to top of used memory to file named
\   by the next word in the input stream
: save  256 here 256 -  bl word count saved ;

\ `loadrom` ( a u -- ) Load ROM file named by a/u and start executing it
: loadrom  ( a u -- ) filename (loadrom) ;

\ `crash` ( ... -- ) Show error indicating an uninitialized deferred word
: crash  ." uninitialized execution vector" cr  abort ;

\ `defer` ( | <word> -- ) Define a "deferred" word, which can be changed later,
\   the initial behaviour is to call `crash` via a ";crash JSR2" sequence,
\   that way we can easily change the (absolute) target address
: defer  head ['] crash literal  44 c, ;

\ `defer!` ( xt1 xt2 -- ) Change the deferred word xt2 to call xt1
\   we add a check for LIT2, otherwise accessing non-deferred words this way
\   will be very hard to debug
: ?defer  ( xt -- a ) count 160 <> abort" not a deferred word" ;
: defer!  ?defer ! ;

\ `defer@` ( xt1 -- xt2 ) Fetch the execution token that is called when the 
\   deferred word xt1 is invoked
: defer@  ?defer @ ;

\ `is` ( xt | <word> -- ) Change the deferred word given in the input stream to
\   call xt when invoked
: is  ' defer! ;

\ `bye` ( -- ) Deferred word to terminate UXN
defer bye  
: (bye)  1 15 deo  brk ;
' (bye) is bye

\ A "vocabulary" is a separate chain of definition headers that can be added
\ to the vocabulary stack to make definitions visible (via `find`).
\ The vocabularies are chained together to be able to list them and restore
\ them in bulk to a previous state. The structure is as follows
\
\   +--------+--------+--------+
\   | <link> | <name> | <next> |
\   +--------+--------+--------+
\
\ The <link> points to the header of the first defined word in the vocabulary or
\ is zero to indicate the end of the chain. <Name> points to the counted string
\ of the name of the vocabulary and <next> points to the next vocabulary (or 0)
\ `find` searches vocabularies in the order in which they are pushed on the
\ vocabulary stack, the latest (topmost) is searched first.

\ `>voc` ( -- a ) A chain connecting all defined vocabularies
variable >voc  ' cdp 3 + >voc !

\ `vocabulary` ( | <word> -- ) Creates a new vocabulary, when <word> is invoked,
\   the current vocabulary will be set to this
: vocabulary  create  >voc @  here >voc !  0 , current @ @ ,  ,  does>  vocs ! ;

\ `hex` ( -- ) Switch numeric base for conversion to 16
: hex  16 base ! ;

\ `decimal` ( -- ) Switch numeric base for conversion to 10
: decimal  10 base ! ;

\ `+!` ( n a -- ) Increase value at address by n
: +!  dup>r @ + r> ! ;

\ `@+` ( a1 -- a2 x ) Fetch short from a1 and increase address by 2
: @+  dup @ >r 2 + r> ;

\ `!+` ( a1 x -- a2 ) Store short x at a1, increase the address by 2
: !+  over ! 2 + ;

\ `space` ( -- ) Write a space character using `emit`
: space  bl emit ;

\ `emits` ( c u -- ) Emit c u times
: emits  begin  ?dup  while  over emit  1-  repeat  drop ;

\ `spaces` ( u -- ) Emit u spaces
: spaces  bl swap emits ;

\ `erase` ( a u -- ) Fill an area of memory with zero bytes
: erase  0 fill ;

\ `blank` ( a u -- ) Fill an area of memory with space characters
: blank  bl fill ;

\ `0<>` ( n -- f ) Compare value with 0, push -1 if non-null or 0 otherwise
: 0<>  if  -1  else  0  then ;

\ `bounds` ( a1 n -- a2 a1 ) Take address and length and convert to end and start
\   addresses (the latter is directly above the last byte)
: bounds  over + swap ;

\ `?exit` ( f -- ) Exit colon word when f is true
: ?exit  if r>drop  then ;

\ `.(` ( | ... ) ) Read string from the input stream terminated by ")" and print
\   it, this is a variant of `."` for use outside of colon definitions
: .(  [char] ) parse type ;

\ `-trailing`  ( a u1 -- a u2 ) Shorten a string (address + length) by deducing the
\   trailing space characters from the length
: -trailing  begin  1- dup 0<  if  1+  |  2dup + c@  bl <>  until  1+ ;

\ `clamp` (n min max -- n2 ) Ensure n is between min and max
: clamp  rot min max ;

\ `2@` ( a - x y ) Read 2 shorts at address, the higher short first
: 2@  dup cell+ @ swap @ ;

\ `2!` ( x y a -- ) Write 2 shorts to address, the topmost item is placed first
: 2!  swap over ! cell+ ! ;

\ `:noname` ( | ... -- xt ) Compile the rest of the input stream and push an execution
\   token that can be used to invoke it
: :noname  also compiler  here (compile) ;

\ `aligned` ( u1 -- u2 ) align to 2 boundary
: aligned  dup 1 and  if  1+  then ;

\ `align` ( -- ) Align the current free memory pointer (`h`)
: align  here aligned h ! ;

\ `diff` ( n1 n2 -- n3 ) "Reverse" subtraction
: diff  swap - ;

\ `2variable` ( | <word> -- ) Define a variable pointing to a double word
: 2variable  create 0 , 0 , ;

\ `2constant` ( x y | <word> -- ) Define a constant that pushes a double word when
\   called
: 2constant  head ['] (2constant) compile, swap , , ;

\ Numeric formatting: a simple way of formatting numeric data, intermixed
\ with text. After starting the formatting with `<#`, consecutive characters and digits
\ are stored in the `pad` from back to front until `#>` is executed, which leaves 
\ the address and length of the formatted string on the stack.

\ `>num` ( -- a ) Variable holding the pointer to the end of the formatted string
variable >num

\ `<#` ( -- ) Start formatting
: <#  pad >num ! ;

\ `#` ( u1 -- u2 ) Take the last digit of u1 (in the numeric base designed by `base`) 
\   and store it in the "hold" area
: #  base @ u/mod swap dup 9 u>  if  
  [char] a + 10 -  else  [char] 0 +  then  >num @ 1- dup >num ! c! ;

\ `#s` ( u -- 0 ) Store consecutive digits in the "hold" area using `#` until n is 0
: #s  begin  # dup  while  repeat ;

\ `#>` ( x -- a n ) Drop x and push the contents of the "hold" area
: #>  drop >num @ dup pad swap - ;

\ `hold` ( c -- ) Add single character to "hold" area, at the front
: hold  >num @ 1- dup>r c! r> >num ! ;

\ `holds` ( a u -- ) Move the string given by a/u to the front in the "hold" area
: holds  dup>r  negate >num +!  >num @ r> cmove ;

\ `sign` ( n -- ) "Holds" a "-" character if n is negative
: sign  ( n -- ) 0<  if  [char] - hold  then ;

\ `(u.)` ( u1 -- a u2 ) Converts the unsigned number u1 to a string in the `pad`
\   and pushes its address and length
: (u.)  <# #s #> ;

\ `u.`  ( u -- ) Converts u to a string and prints it using `type`
: u.  (u.) type space ;

\ `(.)`  ( n -- a u ) Converts the signed number n to a string in the `pad` and
\   pushes its address and length
: (.)  dup abs <# #s swap sign #> ;

\ `.`  ( n -- ) Converts n to a string and prints it using `type`
: .  (.) type space ;

\ `h.`  ( u -- ) Prints u in hexadecimal base using `type`
: h.  base @ >r  hex  u.  r> base ! ;

\ `u.r`  ( u1 u2 -- ) Converts u1 to a string and prints it using `type`, padded
\   on the left with spaces up to a total length of u2
: u.r  >r <# #s #> r> over - 0 max spaces type ;

\ `.r`  ( n u -- ) Converts n to a string and prints it using `type`, padded
\   on the left with spaces up to a total length of u
: .r  >r dup abs <# #s swap sign #> r> over - 0 max spaces type ;

\ `.s`  ( ... -- ... ) Prints all elements in the data stack in reverse order using `.`
: .s  depth ?dup 0=  if  ." stack empty "  |  
  dup 0  do  dup i - pick .  loop  drop ;

\ `search`  ( a1 u1 a2 u2 -- a3 u3 f ) Search the string a2/u2 in the string a1/u1
\    and push the address and length of location where it was found and a flag
\    indicating success or failure
variable /search
: search
  /search !  swap dup>r /search @ - 1+  0  do
    over i + over /search @ swap /search @ compare 0=  if
      drop i +  i  unloop  r> swap -  true  |  loop  drop  r>  false ;

\ `scan`  ( a1 u1 c -- a2 u2 ) Search for byte c in the string a1/u1 and push
\   the address and remaining length of the location where it was found
: scan
  >r  begin  dup  while  over c@ r@ =  if  r>drop  |
    1 /string  repeat  r>drop ;

\ Some more compiler words
also compiler definitions

\ `is`  ( xt | <word> -- ) See `is` above, this compiles inline code
: is  ' literal  ['] defer! compile, ; immediate

\ `recurse`  ( -- ) compile a recursive call to the currently defined colon definition
\   (as the current definition is "smudged" and not visible during compilation)
: recurse  current @ @ count + 2 + compile, ; immediate

\ Save the basic UF ROM file "uf0.rom", containing only the absolute minimum
\ for a working, non-graphical Forth system
only definitions

\ enable prompt when ROM is loaded
' (prompt) is prompt
.( saving uf0.rom ... ) cr
save uf0.rom

\ disable prompt again during further loading of this file
' noop is prompt

\ Some handy things

\ `?`  ( a -- ) Print contents of address a using `.`
: ?  @ . ;

\ `based`  ( u1 u2 | <word> -- n ) Convert next word in input stream to a number using
\   the base u2 and push it on the stack, reset `base` afterwards to u1
: based  base !  bl word number r> r> base ! >r ?exit
  count type  ."  bad number"  cr  abort ;

\ `h#`  ( | <word> -- n ) Convert next word in input stream as hex number and push
\   it on the stack
: h#  base @ >r 16 based ;

\ `d#`  ( | <word> -- n ) Convert next word in input stream as decimal number and push
\   it on the stack
: d#  base @ >r 10 based ;

\ `h#` and `d#` for compile time use inside colon definitions
also compiler definitions
: h#  h# literal ; immediate
: d#  d# literal ; immediate
only definitions

\ `heaptop`  ( -- a ) Holds topmost address of usable heap, minus block buffers
h# ec40 constant heaptop
: unused  heaptop here - ;

\ Loading of source code
\ `include` ( | <filename> -- )
\ `included` ( a u -- ) 
\ Both of these words redirect the input stream to the contents of the file with 
\ the name given either as address/length pair or directly following the `include` form. 
\ As available memory is constrained, the length of the source code may not exceed
\ half of the remaining space between `h` and `heaptop`. If you want to load longer
\ files (like this one), you can simple pass it as standard input when invoking
\ "uxncli" or "uxnemu":
\
\    uxncli ufc.rom < FILENAME
\
variable >include   variable incend  
variable oldquery   variable oldabort
: endinclude  oldquery @  ['] query  defer!  ['] (prompt) is prompt
  oldabort @ ['] abort defer!  >limit @ >in !  >include off ;
: abortinc  endinclude  abort ;
: eol  ( a1 -- a2 f )
  count  13  ->  dup 1- bl swap c!  false  |
  10  ->  true  |  drop  false ;
: inc-line  >include @ dup incend @ >=  if  drop  endinclude  |
  begin
    dup incend @ >=  if  drop  endinclude  |
    eol  if  1- >include @ - >r  >include @ tib r@ cmove
      tib >in !  tib r@ + >limit !  r> 1+ >include +!  |
  again ;
: included  ( a u -- )
  >include @ abort" nested `include` is not supported"
  filename  heaptop here - 2/ heaptop over - dup >include !
  swap fileread  0  ->  true abort" no such file"  |
  >include @ + dup incend !  heaptop = abort" file too big"
  ['] query defer@ oldquery !
  ['] inc-line is query  ['] noop is prompt  ['] abort defer@
  oldabort !  ['] abortinc is abort ;
: include  ( | <name> -- ) bl word count included ;

\ "Varvara" device interface

\ System device:

\ `evector`  ( xt -- ) Set "catch" vector, used by UF to catch machine errors
\   Note that the vector can be an arbitrary Forth execution token
: evector  0 deo2 ;

\ Default handler for the "catch" vector, set during boot time
: catcher  ( inst/code -- )
  255 and
  1 ->  ." stack underflow"  cr  abort |
  2 ->  ." stack overflow"  cr  abort |
  3 ->  ." division by zero"  cr  abort |
  ." unknown machine error"  cr  abort ;

\ `colors`  ( r g b -- ) Set red/green/blue shorts
: colors  12 deo2  10 deo2  8 deo2 ;

\ `halt`  ( status -- ) Exit UXN VM with status code
: halt  h# 80 or 15 deo  brk ;

\ Console device:

\ `cvector`  ( xt -- ) Set console vector, used in the editor to handle
\   additional console input
: cvector  16 deo2 ;

\ `input-type`  ( -- u ) Return input "type" byte from console 
: input-type  ( -- u ) h# 17 dei ;

\ Screen device:

\ `svector`  ( xt -- ) Set screen vector
: svector  32 deo2 ;

\ `screensize@`  ( -- u1 u2 ) Retrieve the screen size in pixels with the width
\   in u1 and the height in u2
: screensize@  34 dei2  36 dei2 ;

\ `screensize!`  ( u1 u2 -- ) Sets the screensize to width u1 and height u2
: screensize!  swap 34 deo2  36 deo2 ;

\ `position`  ( u1 u2 -- ) Sets the x and y position slots in the screen device
\   to u1 and u2, respectively
: position  42 deo2  40 deo2 ;

\ `pixel`  ( u -- ) Sets the pixel mode
: pixel  46 deo ;

\ `auto`  ( u -- ) Sets the "auto" byte
: auto  38 deo ;

\ `spritedata`  ( a -- ) Sets the Screen/addr slot
: spritedata  44 deo2 ;

\ `sprite`  ( u -- ) Sets the Screen/sprite slot
: sprite  47 deo ;

\ Audio device:

\ `devaudio` ( -- a ) Variable holding the currently selected audio device
variable devaudio  h# 30 devaudio !

\ `audio`  ( u -- ) Sets the current audio device, all further device access
\   operates on the selected one
: audio  4 lshift h# 30 + devaudio ! ;

\ `sample`  ( a u -- ) Sets the sample address
: sample  devaudio @ 10 + dup>r deo2 r> 2 + deo2 ;

\ `play`  ( u -- ) Sets the Audio/pitch slot
: play  devaudio @ 15 + deo ;

\ `adsr`  ( u -- ) Sets the Audio/adsr slot
: adsr  devaudio @ 8 + deo2 ;

\ `volume`  ( u -- ) Sets the Audio/volume slot
: volume  devaudio @ 14 + deo ;

\ `output`  ( -- u ) Reads the current Audio/output slot
: output  devaudio @ 4 + dei ;

\ Controller device:

\ `jvector`  ( xt -- ) Sets the controller vector
: jvector  128 deo2 ;

\ `jbutton`  ( -- u ) Reads out the Controller/button slot
: jbutton  130 dei ;

\ `jkey`  ( -- u ) Reads out the Controller/key slot
: jkey  131 dei ;

\ Mouse device

\ `mvector`  ( xt -- ) Sets the mouse vector
: mvector  144 deo2 ;

\ `mouse`  ( -- u1 u2 ) Reads out the mouse x and y position as u1 and u2, respectively
: mouse  146 dei2  148 dei2 ;

\ `mscroll`  ( -- u1 u2 ) Reads out the mouse x and y scroll values as u1 and u2, 
\   respectively
: mscroll  154 dei2  156 dei2 ;

\ `mstate`  ( -- u ) Reads out the Mouse/state slot
: mstate  150 dei ;

\ Daytime device

\ `year`, `month`, `day`, `hour`, `minute`, `second`, `dotw`, `doty` and `isdst`
\   return the associated numeric value from the daytime device
: year  192 dei2 ;      : month  194 dei ;      : day  195 dei ;
: hour  196 dei ;       : minute  197 dei ;     : second  198 dei ;
: dotw  199 dei ;       : doty  200 dei2 ;      : isdst  202 dei ;

\ `wait`  ( -- ) Set Screen/vector and wait for events from other devices,
\   during this time the word `tick` is called 60 times per second (or less),
\   which should keep the stacks as they are on return 
defer tick  ' noop is tick
: waiting  tick  brk ;
: wait  r>drop  ['] waiting svector  brk ;

\ Support for theme and snarf conventions:

\ `apply-theme`  ( a -- ) Sets the System colors with the values found at the
\   3 consecutive shorts at address a
: apply-theme  @+ swap @+ swap @+ nip colors ;

\ `theme`  ( -- ) Read the file ".theme" in the current directory into `pad`
\   and apply the colors found there using `apply-theme`
: theme  s" .theme" filename  pad 6 fileread  if  pad apply-theme  then ;

\ `/snarfed`  ( -- a ) Variable holding number of previously snarfed bytes
variable /snarfed

\ `snarf`  ( a u -- ) Write the u bytes at the address a to the file ".snarf"
\   in the current directory, set `/snarfed`
: snarf  dup /snarfed !  s" .snarf" filename filewrite drop ;

\ `yank`  ( -- a u ) Load the data from the file ".snarf" in the current
\   directory to `pad` (if the file exists) and returns address and length
: yank  s" .snarf" filename pad  unused 4000 min  fileread  pad swap ;

\ Hex dumps
\
\ `dump`  ( a u -- ) Writes a hex dump using `emit`, `.` and `type` from the
\   data at the given address and with the given length
: dumpascii  ( a u -- ) space
  0 do  count dup 33 128 within 0=  if  drop  [char] .  then
    emit  loop  drop ; 
: dumpbyte  ( c -- ) dup 16 <  if   [char] 0  emit  then  
  base @ >r  hex u.   r> base ! ;
: dumprow   ( a u -- a )
  over u. space  0  do  dup i + c@ dumpbyte  loop ;
: dumprest  ( a u -- ) dup>r  dumprow  8 r@ - 3 * spaces
  dup r> dumpascii ;
: dump  ( a u -- )  8 u/mod swap >r  0  ?do
    8 dumprow  dup 8 dumpascii  cr  8 +  loop
  r>  ?dup  if  dumprest then  drop  cr ;

\ Tools to inspect the dictionary

\ `order`  ( -- ) Write the items on the vocabulary stack, the vocabulary
\   where definitions are created is shown in parantheses
: order  4 0 do  
  vocs i th @ cell+ @ ?dup  if  count type  space  then  loop 
  current @ cell+ @ ?dup if  [char] ( emit  count type  ." ) "  then ;

\ `.vocs`  ( -- ) Write out all existing vocabularies
: .vocs  >voc @  begin  ?dup  while  cell+ @+ count type space  @  
  repeat ;

\ `significant`  ( u1 -- u2 ) Rounds down the length in u1 to the number
\   of significant characters in a dictionary entry
: significant  ( u1 -- u2 ) h# 3f and ;

\ `words`  ( -- ) Show all visible words in the order found in all vocabularies
\   on the vocabulary stack
: words  4 0  do  vocs i th @ @
    begin  ?dup  while
      count significant 2dup type space  + @  repeat
  loop ;

\ `marker`  ( | <word> -- ) Creates a "marker", invoking this word restores
\   all vocabularies and the vocabulary stack to the state that existed when
\   the marker was created
: marker-save
  >voc @ ,  current @ ,
  vocs  here  4 cells cmove  4 cells allot
  >voc @  begin  ?dup  while
    @+ ,  cell+ @  repeat  here cell+ , ;
: marker-restore  ( a -- )
  @+ >voc !  @+ current !
  dup vocs 4 cells cmove  4 cells +
  >voc @  begin  ?dup  while
    over @ over !  2 under+  2 cells + @  repeat
  @ h ! ;
: marker  ( | <word> -- )
  create  marker-save  does>  marker-restore ;

\ Interpreter conditionals
\
\ These words provide conditional execution/compilation and work both
\ in interpreted and compiled code.

: processword  ( n1 a n2 -- n3 )
  2dup s" [if]" compare 0=  if  2drop 1+ |
  2dup s" [else]" compare 0=  if  2drop dup 1 =  if  1-  then |
  s" [then]" compare 0=  if  1-  then ;
: skipwords  ( | ... -- )
  1  begin  bl word dup c@  0=  if  drop  query
      else  count processword  then
    ?dup 0=  until ;

\ `[if]`  ( f | ... -- ) Skip characters in input stream until the next
\    `[else]` or `[then]`
: [if]  0=  if  skipwords  then ; immediate

\ `[else]`  ( | ... -- ) Skip characters in input stream until the next `[then]`
: [else]  skipwords ; immediate

\ `[then]`  ( -- ) End of conditional block
: [then] ; immediate

\ `[defined]`  ( | <word> -- f ) Pushes true or false on the stack, depending on
\   wether the next word in the input stream is currently visible in the dictionary
\   or not
: [defined]  bl word find nip ; immediate

\ `[undefined]`  ( | <word> -- f ) Pushes true or false on the stack, depending on
\   wether the next word in the input stream is currently invisible in the dictionary
\   or not
: [undefined]  bl word find 0= nip ; immediate

\ Structure definitions
\
\ Allows for defining slightly more convenient data structures:
\
\ begin-structure point
\   field: x
\   field: y
\ end-structure
\
\ point p1
\ 123 p1 x !  456 p1 y !  \ set x and y fields of structure p1

\ `begin-structure`  ( | <word> -- a 0 ) Start structure definition, creates a wird
\   that defines a dictionary entry that holds the address of a buffer for a structure
\   of this type
: begin-structure  \ -- addr 0 ; -- size 
   create  here 0 0 ,      \ mark stack, lay dummy 
   does> @ ;            \ -- rec-len 

\ `end-structure`  ( a u -- ) End structure definition
: end-structure  \ addr n -- 
   swap ! ;          \ set len

\ `+field`  ( u | <word> -- ) Creates a structure field of u bytes and creates a
\   dictionary entry that will accept a structure address and returns the address 
\   of the field inside that structure
: +field  \ n <"name"> -- ; exec: addr -- 'addr 
   create over , +  does> @ + ;

\ `field:`  ( | <word> -- ) Creates an aligned 2 byte field
: field:    ( n1 "name" -- n2 ; addr1 -- addr2 ) aligned 2 +field ;

\ `cfield:`  ( | <word> -- ) Creates a single byte field
: cfield:   ( n1 "name" -- n2 ; addr1 -- addr2 ) 1   +field ;

\ The assembler
\
\ This is an assembler for a Forthish variant of Uxntal allowing to
\ define words coded in the native code of the UXN VM.
\ All instructions are supported, the short, keeping and return 
\ variants are indicated by the `"`, `k` and `r` markers.
\ See `rnd` below for an example.
\
\ Note that assembly takes place in interpreted mode: calling
\ normal Forth words will invoke them instantly, to compile a
\ call to a Forth word inside a code definition, use "` <word>"
\ or "' <word> #" JSR2".
\
\ Local numeric labels can be defined with `&` and referenced with `,`,
\ at most 16 labels can be defined in a single code definition.
\
\ The assembler has its own vocabulary, active when using `code`
vocabulary assembler
only also assembler definitions

: op  ( c | <inst> -- ) create  c,  does>  c@ c, ;
0 op BRK    1 op INC    2 op POP    3 op NIP    4 op SWP
5 op ROT    6 op DUP    7 op OVR    8 op EQU    9 op NEQ
10 op GTH   11 op LTH   12 op JMP   13 op JCN   14 op JSR
15 op STH   16 op LDZ   17 op STZ   18 op LDR   19 op STR
20 op LDA   21 op STA   22 op DEI   23 op DEO   24 op ADD
25 op SUB   26 op MUL   27 op DIV   28 op AND   29 op ORA
30 op EOR   31 op SFT   32 op JCI   64 op JMI   96 op JSI
h# 80 op LIT

: fixup  ( u -- ) here 1- dup>r c@ or r> c! ;

\ `"`  ( -- ) Modifies the previous instruction to a short version
: "  h# 20 fixup ;

\ `#"`  ( x -- ) Compiles LIT2 xxxx
: #"  LIT " , ;

\ `#`  ( c -- ) Compiles LIT xx
: #  ( c -- ) LIT c, ;

\ `k`  ( -- ) Modifies the previous instruction to a "keep" version
: k  h# 80 fixup ;

\ `r`  ( -- ) Modifies the previous instruction to a "return" version
: r  h# 40 fixup ;

\ `$`  ( u -- ) Advances the code pointer by u bytes (an alias for `allot`)
: $  ( u -- ) allot ;

\ Label resolution
16 cells buffer: lbls       variable fwdrefs
: fwdref  ( u -- a ) fwdrefs @ swap 16 * th ;
: patch  ( a -- ) here over - 2 - swap c! ;
: resolve  ( u -- ) fwdref
  16  0  do  @+ ?dup  if  patch  then  loop  drop ;
: addref  ( u -- ) fwdref
  16  0  do  dup @ 0=  if  here 1- swap !  unloop  |  cell+  loop 
  true abort" too many forward references" ;

\ `&`  ( u -- ) Defines label with the index u, if referenced earlier,
\   then jumps to this label are patched
: &  here over lbls swap th !  resolve ;

\ ```  ( | <word> -- ) Compile an immediate call (JSI) to a Forth word
: `  JSI  ' here - 2 + , ;

\ `!`  ( | <word> -- ) Compile an immediate jump (JNI) to a Forth word
: !  JMI  ' here - 2 + , ;

\ `?`  ( | <word> -- ) Compile an immediate conditional jump (JCI) to a Forth word
: ?  JCI  ' here - 2 + , ;

\ `,,`  ( x -- ) Compile a raw short (an alias for `,`, which has a different
\    meaning in the assembler)
: ,,  , ;

\ `,`  ( u -- ) Compile a relative reference to label number u
: ,  >r lbls r@ th @ ?dup  if  here 3 + - #  r>drop |  0 #  r> addref ;

also forth definitions 
\ These words are visible in the `forth` vocabulary:

\ `code`  ( | <word> ... -- ) Start definition of a code word by pushing the
\   `assembler` vocabulary on the vocabulary stack and creating a dictionary entry
: code  ( | <word> -- ) head  also assembler
  lbls 16 cells erase  pad 256 + fwdrefs !
  fwdrefs @ 256 erase ;

\ `end-code`  ( -- ) Ends a code definition (simply pops the `assembler` vocabulary
\   from the vocabulary stack)
: end-code  previous ;

only forth

\ A Forth word decompiler
\
\ This tool can be used to look at code or colon definitions, it disassembles
\ code in a particular word or at a given address. Since the end of words is
\ not specially marked, some heuristics are used to detect the end of a
\ definition: normally JSR2 (`exit`) is a got indicator, as is any other 
\ unconditional direct brach, assuming no forward branches are still unresolved.
\ For this reason the decompiler tries to keep track of forward branches.
\ Forth is very flexible and allows many tricks and since this system compiles
\ to native code, the described heuristics may fail in certain cases.

\ `rfind` ( xt -- a -1 | xt 0) Handy word to find the name field of an execution
\   token in the dictionary ("reverse find")
: skipname  ( a1 -- a2 ) count significant + ;
: (rfind)  ( xt dp -- a -1 | xt 0 )
  begin  ?dup 0=  if  false  |
    dup>r skipname 2 + over =  if  drop  r>  true  |
    r> skipname @
  again ;
: rfind  ( xt -- a -1 | xt 0 )
  vocs dup 8 + swap  do
    i @ @ (rfind) dup  if  unloop  |  drop
  2 +loop  false ;

vocabulary decompiler
also decompiler definitions
: decode-op  ( c -- )
  0  ->  ." BRK"  |  h# 1f and
  0  ->  ." LIT"  |  1  ->  ." INC"  |  2  ->  ." POP"  |
  3  ->  ." NIP"  |  4  ->  ." SWP"  |  5  ->  ." ROT"  |
  6  ->  ." DUP"  |  7  ->  ." OVR"  |  8  ->  ." EQU"  |
  9  ->  ." NEQ"  |  10  ->  ." GTH"  |  11  ->  ." LTH"  |
  12  ->  ." JMP"  |  13  ->  ." JCN"  |  14  ->  ." JSR"  |
  15  ->  ." STH"  |  16  ->  ." LDZ"  |  17  ->  ." STZ"  |
  18  ->  ." LDR"  |  19  ->  ." STR"  |  20  ->  ." LDA"  |
  21  ->  ." STA"  |  22  ->  ." DEI"  |  23  ->  ." DEO"  |
  24  ->  ." ADD"  |  25  ->  ." SUB"  |  26  ->  ." MUL"  |
  27  ->  ." DIV"  |  28  ->  ." AND"  |  29  ->  ." ORA"  |
  30  ->  ." EOR"  |  drop  ." SFT" ;
: decode-bits  ( op -- )
  dup h# 20 and  if  [char] 2 emit  then
  dup h# 80 and  if  [char] k emit  then
  h# 40 and  if  [char] r  emit  then ;
: h2.  ( u -- ) dup 16 u<  if  [char] 0  emit  then
  hex  (u.) type  decimal ;
: h4.  ( u -- ) dup 8 rshift h2.  255 and  h2. ;
: decode-jump  ( a1 -- a2 ) @+ space h4. ;
: decode-bytes  ( a1 -- a2 ) 
  count 0  ?do  count space h2.  loop ;
: decode-special  ( a1 xt -- a2 )
  ['] (slit)  ->  decode-bytes  |
  ['] (if)  ->  decode-jump  |  
  ['] (else)  -> decode-jump  |  drop ;
: ijmprune  ( op -- ) 
  32  ->  [char] ? emit  |  64  ->  [char] ! emit  |  drop ;
: ijmpop  ( op -- ) 
  32  ->  ." JCI"  |  64  ->  ." JMI"  |  drop ." JSI" ;
: rel16  ( a1 -- a2 ) dup @ + 2 + ;
: decode-ijmp  ( a1 -- a2 ) dup 1- c@ >r
  dup rel16 rfind  if  
    r> ijmprune count significant type  2 + |
  r> ijmpop  space  over @ h4.  ."  ( " h4. ."  ) " 2 + ;
: decode-jsr  ( a1 xt -- a2 )
  over c@ h# 2e =  if
    ."  JSR2" 1 under+ decode-special  else  drop  then ;
: decode-litk2  ( a1 -- a2 )
  @+ dup>r rfind  if  [char] ; emit  count significant type
    r> decode-jsr  |  [char] # emit  h4.  r>drop ;
variable lit1   variable atend
: end?  ( op -- f ) 0  ->  true  |  64  ->  true  |
  h# 1f and h# 0c = ; 
: decode  ( a1 -- a2 ) count  dup end? atend !
  h# 80  ->  [char] # emit  count dup lit1 c!  h2. |  lit1 off
  32  ->  decode-ijmp  |  64  ->  decode-ijmp  |
  96  ->  decode-ijmp  |  h# a0  ->  decode-litk2  |
  dup decode-op  decode-bits ;
: jump?  ( op -- f ) h# 1f and 12 15 within ;
: chkjump  ( 0 ... a1 -- 0 ... [a2] a1 )
  dup c@ jump? 0= ?exit  lit1 c@ h# 80 and ?exit
  lit1 c@ ?dup 0= ?exit  over 1+ + swap ;
: finished?  ( 0 ... a1 a2 -- -1 | 0 ... a1 a2 0 )
  atend @ dup 0= ?exit  drop over 0=  if  2drop true  |  false ;
: dropfwds  ( 0 ... a -- 0 ... a )
  begin  over 0= ?exit
    2dup u> 0=  if  nip  else  exit  then
  again ;
only forth definitions also decompiler

\ `decompile`  ( xt -- ) Decompiles the code at the given address
: decompile  0 swap ( marker )
  lit1 off  begin
    atend off  chkjump  dropfwds
    decode  space  finished? ?exit
  again ;

\ `see`  ( | <word> -- ) Decompiles the definition of the next word in the input 
\   stream
: see  
  bl word find dup 0= abort" not found"
  1 =  if  ." (immediate) "  then
  decompile ;
only forth

\ Random numbers (taken from prng.tal)

\ `seed`  ( a -- ) Variable holding the current random state
variable seed

\ `randomize`  ( -- ) Initializes the random state to something
: randomize  second seed ! ;

\ `rnd`  ( -- n ) Returns a 16-bit pseudo random number
code rnd
    \ returns the next number in a 65,535-long sequence,
    \ which is never zero but every other 16-bit number
    \ appears once before the sequence repeats )
    \ http://www.retroprogramming.com/2017/07/xorshift-pseudorandom-numbers-in-z80.html
    ' seed #" LDA "
    DUP " h# 70 # SFT " EOR "
    DUP " 9 # SFT " EOR "
    DUP " h# 80 # SFT " EOR "
    ' seed #" STA " k POP "
    JMP " r
end-code

\ `new`  ( -- ) Marker resetting the dictionary to the base system + tools
marker new

\ Save the extended base system to "uf.rom", which holds the base system and
\ additional tools.

.( saving uf.rom ... ) cr
' (prompt) is prompt
\ redefine `boot` to catch machine errors and enter interactive loop on the console
: boot  ['] catcher evector  prompt  quit ;
save uf.rom
' noop is prompt

\ The graphical console
\
\ The full UF system provides a graphical console that has the same functionality
\ as the terminal console, you can enter text, press ENTER to execute it and move
\ the cursor by using the cursor keys or left-blicking with the mouse at a 
\ particular position. The content scrolls upwards as text lines are entered, 
\ pressing ENTER on any line sends the line to the Forth interpreter.
\ Right-clicking a space-delimited word is equivalent to entering it, followed
\ by ENTER, i.e. the word is executed.
\ 
\ A number of control- and shift-key combinations are available on the console:
\ ("^" meaning the Control key, "Sh" the Shift key):
\
\   ^a      Jump to beginning of line
\   ^e      Jump to end of line
\   ^c      Terminate Forth
\   ^d      Delete next char
\   ^k      Kill to end of line or marked region (and write to ".snarf" file)
\   ^u      Kill to beginning of line or marked region (and snarf)
\   ^x      Copy line or marked region to ".snarf" file
\   ^v, ^y  Paste contents of ".snarf" file
\   ^f      Find next occurrence f word below cursor
\   ^l      Clear screen
\   ^ENTER  Toggle region mark on/off
\   Sh-Up   Recall the last entered line
\
\ Note that the terminal console is still functional in the graphical environment,
\ entering text (or redirecting standard input) is fully equivalent to entering
\ it in the graphical console.
\
\ The block editor
\
\ The system also provides a "block" editor for screen-sized bodies of code.
\ Blocks are stored in the file system in the current directory with the name
\ being equal to the block number, an arbitrary integer. A block contains 2496 
\ bytes (39 lines with 64 columns).
\
\ Entering the editor takes place by using the `edit` word, giving it the 
\ number of the block to open. You can also edit a block by clicking with the
\ right mouse button on a string of the form "#nnnn" where "nnnn" is a decimal 
\ integer.
\
\ Additional or changed control- and shift-key combinations in the editor:
\
\   ^s      Save changes in currently edited block
\   ^g      Read digits and jump to indicated block on ENTER (abort on ESC)
\   ^r      Read digits and copy current block to new one on ENTER
\   ^m      Read digits and move current block to new one on ENTER
\           (and delete the old block)
\
\   Sh-Down         Jump to next block
\   Sh-Left/Right   Jump between code and documentation ("shadow") block
\   Sh-Up           Jump to previous block
\
\ Moving the scroll wheel will jump to the previous or next block, when 
\ currently editing an unmodified block.
\ The ESC key will exit or enter editing (unless the current block is modified). 
\ The bottom row contains counters indicating current row, column, block number, 
\ the length of the most recently copied text, the amount of dictionary space 
\ left and a marker indicating whether the current block is modified. Blocks 
\ can be loaded using `load` and `thru`. Note that block-loading is done by temporarily
\ changing the word for reading user-input (`query`) and takes effect when the 
\ current input line has been fully processed. That implies that loading words can 
\ not be invoked recursively.
\
\ Notable conventions used in the editor:
\ 
\ * Blocks 1000- are used as "shadow" blocks, providing documentation, so
\   block #1001 would contain documentation for block #1.
\
\ * The topmost line of a newly created empty block is used to name the block and 
\   contains a "menu" in the form of right-clickable editor commands

\ Console and editor code
\
\ Basic I/O primitives - we save them here in `cin` and `cout` to ensure
\ we have unchanged primitives directly accessing the console. `key` and `emit`
\ will later be changed for the graphical console:
defer cin   defer cout
' key defer@ is cin    ' emit defer@ is cout
: ctype  ( a u -- ) 0  ?do  count cout  loop  drop ;

\ `edit`  ( u -- ) Start editing block u
defer edit

\ `stdin`  ( -- ) Hook that is invoked when non-graphical console input is available
defer stdin

\ `page`  ( -- ) Clear screen
defer page

\ The `editor` vocabulary holds most of the editor stuff to avoid polluting
\ the visible namespace
vocabulary editor
also editor definitions

\ Font used for graphical interface (BBC micro font, I believe, found somewhere
\ in the uxn source tree)
create font  hex
00 c, 00 c, 00 c, 00 c, 00 c, 00 c, 00 c, 00 c, 
18 c, 18 c, 18 c, 18 c, 18 c, 00 c, 18 c, 00 c, 
6c c, 6c c, 6c c, 00 c, 00 c, 00 c, 00 c, 00 c, 
36 c, 36 c, 7f c, 36 c, 7f c, 36 c, 36 c, 00 c, 
0c c, 3f c, 68 c, 3e c, 0b c, 7e c, 18 c, 00 c, 
60 c, 66 c, 0c c, 18 c, 30 c, 66 c, 06 c, 00 c, 
38 c, 6c c, 6c c, 38 c, 6d c, 66 c, 3b c, 00 c, 
0c c, 18 c, 30 c, 00 c, 00 c, 00 c, 00 c, 00 c, 
0c c, 18 c, 30 c, 30 c, 30 c, 18 c, 0c c, 00 c, 
30 c, 18 c, 0c c, 0c c, 0c c, 18 c, 30 c, 00 c, 
00 c, 18 c, 7e c, 3c c, 7e c, 18 c, 00 c, 00 c, 
00 c, 18 c, 18 c, 7e c, 18 c, 18 c, 00 c, 00 c, 
00 c, 00 c, 00 c, 00 c, 00 c, 18 c, 18 c, 30 c, 
00 c, 00 c, 00 c, 7e c, 00 c, 00 c, 00 c, 00 c, 
00 c, 00 c, 00 c, 00 c, 00 c, 18 c, 18 c, 00 c, 
00 c, 06 c, 0c c, 18 c, 30 c, 60 c, 00 c, 00 c, 
3c c, 66 c, 6e c, 7e c, 76 c, 66 c, 3c c, 00 c, 
18 c, 38 c, 18 c, 18 c, 18 c, 18 c, 7e c, 00 c, 
3c c, 66 c, 06 c, 0c c, 18 c, 30 c, 7e c, 00 c, 
3c c, 66 c, 06 c, 1c c, 06 c, 66 c, 3c c, 00 c, 
0c c, 1c c, 3c c, 6c c, 7e c, 0c c, 0c c, 00 c, 
7e c, 60 c, 7c c, 06 c, 06 c, 66 c, 3c c, 00 c, 
1c c, 30 c, 60 c, 7c c, 66 c, 66 c, 3c c, 00 c, 
7e c, 06 c, 0c c, 18 c, 30 c, 30 c, 30 c, 00 c, 
3c c, 66 c, 66 c, 3c c, 66 c, 66 c, 3c c, 00 c, 
3c c, 66 c, 66 c, 3e c, 06 c, 0c c, 38 c, 00 c, 
00 c, 00 c, 18 c, 18 c, 00 c, 18 c, 18 c, 00 c, 
00 c, 00 c, 18 c, 18 c, 00 c, 18 c, 18 c, 30 c, 
0c c, 18 c, 30 c, 60 c, 30 c, 18 c, 0c c, 00 c, 
00 c, 00 c, 7e c, 00 c, 7e c, 00 c, 00 c, 00 c, 
30 c, 18 c, 0c c, 06 c, 0c c, 18 c, 30 c, 00 c, 
3c c, 66 c, 0c c, 18 c, 18 c, 00 c, 18 c, 00 c, 
3c c, 66 c, 6e c, 6a c, 6e c, 60 c, 3c c, 00 c, 
3c c, 66 c, 66 c, 7e c, 66 c, 66 c, 66 c, 00 c, 
7c c, 66 c, 66 c, 7c c, 66 c, 66 c, 7c c, 00 c, 
3c c, 66 c, 60 c, 60 c, 60 c, 66 c, 3c c, 00 c, 
78 c, 6c c, 66 c, 66 c, 66 c, 6c c, 78 c, 00 c, 
7e c, 60 c, 60 c, 7c c, 60 c, 60 c, 7e c, 00 c, 
7e c, 60 c, 60 c, 7c c, 60 c, 60 c, 60 c, 00 c, 
3c c, 66 c, 60 c, 6e c, 66 c, 66 c, 3c c, 00 c, 
66 c, 66 c, 66 c, 7e c, 66 c, 66 c, 66 c, 00 c, 
7e c, 18 c, 18 c, 18 c, 18 c, 18 c, 7e c, 00 c, 
3e c, 0c c, 0c c, 0c c, 0c c, 6c c, 38 c, 00 c, 
66 c, 6c c, 78 c, 70 c, 78 c, 6c c, 66 c, 00 c, 
60 c, 60 c, 60 c, 60 c, 60 c, 60 c, 7e c, 00 c, 
63 c, 77 c, 7f c, 6b c, 6b c, 63 c, 63 c, 00 c, 
66 c, 66 c, 76 c, 7e c, 6e c, 66 c, 66 c, 00 c, 
3c c, 66 c, 66 c, 66 c, 66 c, 66 c, 3c c, 00 c, 
7c c, 66 c, 66 c, 7c c, 60 c, 60 c, 60 c, 00 c, 
3c c, 66 c, 66 c, 66 c, 6a c, 6c c, 36 c, 00 c, 
7c c, 66 c, 66 c, 7c c, 6c c, 66 c, 66 c, 00 c, 
3c c, 66 c, 60 c, 3c c, 06 c, 66 c, 3c c, 00 c, 
7e c, 18 c, 18 c, 18 c, 18 c, 18 c, 18 c, 00 c, 
66 c, 66 c, 66 c, 66 c, 66 c, 66 c, 3c c, 00 c, 
66 c, 66 c, 66 c, 66 c, 66 c, 3c c, 18 c, 00 c, 
63 c, 63 c, 6b c, 6b c, 7f c, 77 c, 63 c, 00 c, 
66 c, 66 c, 3c c, 18 c, 3c c, 66 c, 66 c, 00 c, 
66 c, 66 c, 66 c, 3c c, 18 c, 18 c, 18 c, 00 c, 
7e c, 06 c, 0c c, 18 c, 30 c, 60 c, 7e c, 00 c, 
7c c, 60 c, 60 c, 60 c, 60 c, 60 c, 7c c, 00 c, 
00 c, 60 c, 30 c, 18 c, 0c c, 06 c, 00 c, 00 c, 
3e c, 06 c, 06 c, 06 c, 06 c, 06 c, 3e c, 00 c, 
18 c, 3c c, 66 c, 42 c, 00 c, 00 c, 00 c, 00 c, 
00 c, 00 c, 00 c, 00 c, 00 c, 00 c, 00 c, ff c, 
1c c, 36 c, 30 c, 7c c, 30 c, 30 c, 7e c, 00 c, 
00 c, 00 c, 3c c, 06 c, 3e c, 66 c, 3e c, 00 c, 
60 c, 60 c, 7c c, 66 c, 66 c, 66 c, 7c c, 00 c, 
00 c, 00 c, 3c c, 66 c, 60 c, 66 c, 3c c, 00 c, 
06 c, 06 c, 3e c, 66 c, 66 c, 66 c, 3e c, 00 c, 
00 c, 00 c, 3c c, 66 c, 7e c, 60 c, 3c c, 00 c, 
1c c, 30 c, 30 c, 7c c, 30 c, 30 c, 30 c, 00 c, 
00 c, 00 c, 3e c, 66 c, 66 c, 3e c, 06 c, 3c c, 
60 c, 60 c, 7c c, 66 c, 66 c, 66 c, 66 c, 00 c, 
18 c, 00 c, 38 c, 18 c, 18 c, 18 c, 3c c, 00 c, 
18 c, 00 c, 38 c, 18 c, 18 c, 18 c, 18 c, 70 c, 
60 c, 60 c, 66 c, 6c c, 78 c, 6c c, 66 c, 00 c, 
38 c, 18 c, 18 c, 18 c, 18 c, 18 c, 3c c, 00 c, 
00 c, 00 c, 36 c, 7f c, 6b c, 6b c, 63 c, 00 c, 
00 c, 00 c, 7c c, 66 c, 66 c, 66 c, 66 c, 00 c, 
00 c, 00 c, 3c c, 66 c, 66 c, 66 c, 3c c, 00 c, 
00 c, 00 c, 7c c, 66 c, 66 c, 7c c, 60 c, 60 c, 
00 c, 00 c, 3e c, 66 c, 66 c, 3e c, 06 c, 07 c, 
00 c, 00 c, 6c c, 76 c, 60 c, 60 c, 60 c, 00 c, 
00 c, 00 c, 3e c, 60 c, 3c c, 06 c, 7c c, 00 c, 
30 c, 30 c, 7c c, 30 c, 30 c, 30 c, 1c c, 00 c, 
00 c, 00 c, 66 c, 66 c, 66 c, 66 c, 3e c, 00 c, 
00 c, 00 c, 66 c, 66 c, 66 c, 3c c, 18 c, 00 c, 
00 c, 00 c, 63 c, 6b c, 6b c, 7f c, 36 c, 00 c, 
00 c, 00 c, 66 c, 3c c, 18 c, 3c c, 66 c, 00 c, 
00 c, 00 c, 66 c, 66 c, 66 c, 3e c, 06 c, 3c c, 
00 c, 00 c, 7e c, 0c c, 18 c, 30 c, 7e c, 00 c, 
0c c, 18 c, 18 c, 70 c, 18 c, 18 c, 0c c, 00 c, 
18 c, 18 c, 18 c, 18 c, 18 c, 18 c, 18 c, 00 c, 
30 c, 18 c, 18 c, 0e c, 18 c, 18 c, 30 c, 00 c, 
31 c, 6b c, 46 c, 00 c, 00 c, 00 c, 00 c, 00 c, 
ff c, ff c, ff c, ff c, ff c, ff c, ff c, ff c, 

decimal

\ Variables holding editor state
variable row    variable col
variable rows   variable columns
variable screen  variable loadbuf
variable locked     variable mark
variable pointerx   variable pointery
variable modified
defer noedit  ( -- f )
defer terminate

\ Limits
512 constant width      320 constant height
4 constant tabwidth     1000 constant #shadow
64 buffer: rtib       variable /rtib

\ Words to compute various sizes and locations in the edited block
: /screen  ( -- u ) screen @ negate ;
: /block  ( -- u ) /screen columns @ - ;
: point  ( -- r c ) row @ col @ ;
: >screen  ( r c -- a ) swap columns @ * + screen @ + ;
: >row  ( r -- a ) 0 >screen ;
: offset  ( -- u ) point >screen screen @ - ;
: at  ( r c -- ) 3 lshift  swap 3 lshift  position ;
: locate  point at ;
: remaining  ( -- u ) columns @ col @ - ;
: line  ( -- a u ) point >screen  remaining ;
: prepare  ( a u -- ) >r  dup tib  r@ cmove
  rtib r@ cmove  tib dup >in !  r@ + >limit !  r> /rtib ! ;
: screen>buf  screen @ loadbuf @ /block cmove ;
: buf>screen  loadbuf @ screen @ /block cmove ;
: range  ( n1 n2 -- n3 n4 ) 2dup min -rot  max ;
: between ( n1 n2 n3 -- f ) range 1+ within ;
: bottomrow  ( -- u ) rows @ 1- ;
: marked?  ( row -- f ) mark @ dup  if
    1- row @  between  else  nip  then ;

\ Colors and some basic display attributes
variable textcolor
\  theme: color #0 background, #1 text, #2 modeline, #3 highlight
create default-theme  h# 0b75 , h# 0da6 , h# 0db8 ,
: reverse  h# 4e textcolor ! ;
: normal  h# 41 textcolor ! ;
: highlite  h# 43 textcolor ! ;
: initscreen  width height screensize!
  height 3 rshift rows !  width 3 rshift columns !
  rows @ columns @ * dup negate screen !  
  screen @ /block - loadbuf !
  screen @ swap blank 
  pointerx off  pointery off ;
: initcolors  default-theme apply-theme  normal ;

\ Character drawing
: glyph  ( c -- a ) dup 32 128 within 0=  if  drop  127  then
  bl - 3 lshift font + spritedata ;
: drawchar  ( c -- ) glyph  textcolor @ sprite ;
: drawrow  ( row -- ) dup  0 at  dup>r >row  1 auto
  r> marked?  if  reverse  else 
    dup c@ [char] \ =  if  highlite  then  then
  columns @  0  do  count drawchar  loop  drop  normal ;

\ The input cursor
30 constant blinks
variable ccount  1 ccount !
variable cursorcol  1 cursorcol !
: drawcursor  ( c -- ) locate  127 glyph  sprite ;
: cursor  ( f -- )
  if  blinks ccount !  1 dup cursorcol !  else  0  then
  drawcursor ;
: blink  ccount @ 1- ?dup  if  ccount !  |  blinks ccount !
  cursorcol @  if  0  else  1  then  dup cursorcol ! drawcursor ; 
' blink is tick

\ The simulated mouse pointer
: pointer  ( f -- ) pointerx @ 3 lshift pointery @ 3 lshift position  
  127 glyph  if  h# 03 sprite  |
  h# 00 sprite  1 cursor ;
: home  0 cursor  row off  col off  mark off  1 cursor ;

\ Screen drawing and scrolling
: clrscr  bl glyph  1 auto
  rows @  0  do  
    i 0 at  columns @  0  do  h# 40 sprite  loop  
  loop ;
: redraw  clrscr  rows @  0  do  i drawrow  loop ;
: scroll  1 >row  screen @  columns @ bottomrow * cmove
  bottomrow >row columns @ blank  redraw ;
: newline  locked @  if  1 cursor  |
  col off  row @ 1+ dup rows @ =  if  drop  scroll  
  else  row !  then  1 cursor ;
: scrolldown  row @ 1+ >row dup columns @ +
  rows @ 2 - row @ - columns @ * cmove>
  row @ 1+ >row columns @ blank ;
: scrollup  ( row -- ) dup>r 1+ >row dup columns @ -
  rows @ 2 - r> - columns @ *  cmove  
  rows @ 2 - >row columns @ blank ;

\ mark, snarf + yank
: toggle-mark  0 cursor
  mark @  if  mark off  else  row @ 1+ mark !  then
  redraw  1 cursor ;
: redraw/mark  mark @  if  redraw  then ;
: snarf-lines  here >r  mark @ 1- row @ range 1+  swap  do
    i >row columns @ -trailing >r here r@ cmove 
    r> allot  10 c,
  loop  r@ dup here diff snarf  r> h ! ;
: copy-marked  mark @  if  snarf-lines    mark off  redraw  |
  row @ >row columns @ -trailing snarf ;
: cut-marked  snarf-lines
  mark @ 1- row @ range  over row !
  1+ swap  ?do  row @ scrollup  loop 
  mark off  redraw ;
: endpaste  redraw  1 cursor  modified on ;
: nextline  ( a1 u1 -- a2 u2 a3 u3 )
  over >r 10 scan  over r@ - r> swap ;
: paste-rest  ( a u -- ) >r
  point >screen  dup r@ +  remaining r@ - 0 max  cmove>
  line r@ min cmove
  r> col @ + columns @ 1- min col !  endpaste ;
: paste  0 cursor  yank  
  2dup 10 scan nip 0=  if  paste-rest  |
  begin
    row @ bottomrow =  if  -1 row +!  2drop  endpaste  |
    ?dup 0=  if  drop  endpaste  |
    nextline  col off
    row @ >row  swap 64 min dup>r cmove
    row @ r@ >screen r> 64 diff blank
    1 row +!  dup  if  1 /string  then
  again ;

\ Textual operations on the characters in a block
: split  row @ rows @ 3 - >= ?exit  0 cursor
  scrolldown  line >r  row @ 1+ >row r> cmove
  line blank  col off  1 row +!  modified on  redraw ;
: enter  locked @  if  split  1 cursor  |  0 cursor  
  row @ >row columns @ -trailing prepare  newline ;
: advance  col @ 1+ dup columns @ =  if  drop  newline  |  
  col !  1 cursor ;
: insert  ( c -- ) col @ columns @ =  if  drop  |
  0 cursor  line 1- >r dup 1+ r> cmove>  point >screen c!
  row @ drawrow  advance  modified on ;
: left  col @  if  0 cursor  -1 col +!  1 cursor  then ;
: right  col @ columns @ 1- = ?exit  0 cursor  advance ;
: up  0 cursor  row @ 1- 0 max row !  redraw/mark  1 cursor ;
: down  0 cursor  row @ 1+ bottomrow 
  locked @  if  1-  then  min row !  redraw/mark  1 cursor ;
: blankend  bl row @ columns @ 1- >screen c! 
  row @ drawrow  1 cursor  modified on ;
: backup  row @ 0= ?exit  0 cursor
  line drop  row @ 1- >row columns @ -trailing dup col ! dup>r +
    columns @ r> -  cmove  row @ scrollup  -1 row +!
  modified on  redraw  1 cursor ;
: rubout  col @ 0=  if  locked @  if  backup  then  |
  0 cursor  -1 col +!  line 1- >r dup 1+ swap r> cmove  blankend ;
: join  row @ bottomrow 1- =  if  blankend  |
  row @ 1+ >row  remaining  line drop swap  cmove
  row @ 1+ scrollup  redraw  1 cursor  modified on ;
: delete  0 cursor  line -trailing 0=  if  drop  join  |
  remaining 1- >r dup 1+ swap r> cmove  blankend ;
: top  0 cursor  row off  1 cursor ;
: start  0 cursor  col off  1 cursor ;
: end  0 cursor  row @ >row columns @ -trailing nip
  columns @ 1- min col !  1 cursor ;
: tab  0 cursor  col @ dup>r tabwidth u/ 1+ tabwidth *
  columns @ 1- min col !
  row @ r@ >screen  col @ r> - 2dup over +  remaining cmove>
  bl fill  row @ drawrow  1 cursor ;
: rkill  0 cursor  mark @  if  cut-marked  |
  line -trailing dup col @ or 0=  if
    2drop  row @ scrollup  redraw  else
    2dup snarf blank  row @ drawrow  then
  1 cursor  modified on ;
: lkill  col @ columns @ 1- = ?exit 
  0 cursor  mark @  if  cut-marked  |
  row @ >row col @ 2dup snarf 
  line  row @ >row  swap  cmove
  dup>r + columns @ r> - blank  col off
  row @ drawrow  1 cursor  modified on ;
: recall  locked @ ?exit
  0 cursor  rtib row @ >row /rtib @ cmove  /rtib @ col !
  redraw ;

variable dirty
: gemit  ( c -- )  dirty on
  0 cursor  10  ->  newline  |  locate  dup point >screen c! 
  drawchar  advance ;

defer status
: update  locked @  if  status  then ;

\ Key handling
defer ctrl-key  ( key -- key|0 )
defer other-key  ( key -- )
defer handle-button  ( key but -- key|0 )
: input  0 pointer
  jkey  jbutton handle-button  other-key  update  wait ;
: no-events  0 jvector  0 mvector  0 cvector  0 svector ;

\ Block I/O
variable loading    variable block
variable endload
: >loadbuf  ( r c -- a ) swap columns @ * + loadbuf @ + ;
: newblock
  block @ #shadow <  if
    s" \  Load  Save  Snarf  Paste  Prev  Next  Abort  Doc"
  else
    <# s"  Save  Snarf  Paste  Prev  Next  Abort  Code" holds
       block @ #shadow - #s  s" \s #" holds  #>
  then  screen @ swap cmove ;
: fileblock  ( a u1 -- u2 ) (u.) filename  
  /block 2dup blank fileread ;
: loadblock  ( u -- ) screen @ /screen blank  dup block !
  screen @ swap fileblock  0=  if  newblock  then
  modified off ;
: save-block
  modified 0= ?exit  block @ 0= abort" no block specified"
  block @ (u.) filename  screen @ /block filewrite
  /block <> abort" error while writing block"
  modified off  update ;
: writeblock
  bottomrow  0  do  i 0 >loadbuf columns @ -trailing ctype  10 cout  
  loop ;

\ Remembering the last input location
create editpos 0 , 0 ,
: savepos  editpos row @ !+ col @ swap ! ;
: restorepos  editpos @+ row ! @ col ! ;
: enteredit  redraw  0 cursor  dirty off  mark off
  locked on  ['] noop is prompt  restorepos  1 cursor  update ;
: exitedit  ( -- f ) modified @  if  false  |
  savepos  0 cursor  locked off  bottomrow row !  col off
  mark off  line blank  bottomrow drawrow  1 cursor
  ['] (prompt) is prompt  true ;

\ Fetch word below mouse position
: mouse>loc  ( -- r c ) mouse 3 rshift  swap 3 rshift ;
: scanleft  ( r c -- a )  0  ->  >row  |
  begin  
    1-
    2dup >screen c@ 33 <  if  1+ >screen  |
    0  ->  >row  |
  again ;
: scanright  ( r c -- a ) begin
    columns @  ->  columns @ >screen  |
    2dup >screen c@ 33 <  if  >screen  |
    1+
  again ;
: (below)  ( r c -- a u ) 2dup scanright >r  scanleft r> over - ;
: below-point  ( -- a u ) point (below) ;
: below  ( -- a u ) mouse>loc (below) ;
: blockref?  ( a u1 -- u2 1 | a u1 0 )
  over c@ [char] # <>  if  false  |
  1 /string pad place  pad number 0=  if  count  false  |
  true ;

\ Mouse input handling
: warp  0 cursor  mouse>loc col ! 
  locked @  if  rows @ 2 - min  then  
  row !  1 cursor  update ;
: loadref  ( u -- )
  locked @ 0<>  modified @ 0<>  and  if  drop  |  edit ;
: clicked  mstate
  1  ->  warp  |  
  4  ->  below ?dup  if
    blockref?  if  loadref  else
      locked @  if  dirty off  screen>buf  then
      prepare  no-events  r>drop  then 
    else  drop  then  |  drop ;
: shiftblock  ( n -- ) locked @ 0=  if  drop  |  modified @ ?exit 
  block @ + 1 max edit ;
: mouseinput  0 pointer  mouse>loc pointerx !  pointery !
  mscroll nip ?dup  if  shiftblock  then
  1 pointer  clicked  wait ;

\ Waiting for events
: handlecin  ( c -- )
  0  ->  |  9  ->  tab  |
  10  ->  enter  no-events  r>drop  |  insert ;
: (stdin)  18 dei  handlecin  wait ;
: events  ['] input jvector  ['] stdin cvector
  ['] mouseinput mvector ;
: listen  events
  dirty @ 0<>  locked @ 0<>  and  if
    buf>screen  redraw  update  dirty off  then
  wait ;

\ Block loading (evaluation)
: load1  ( u -- ) ."  #" dup . 
  loadbuf @ over fileblock 0= abort" no such block"
  block ! ;
: ?load  ( u -- f ) loadbuf @ swap fileblock ;
: nextblock  ( u -- f ) begin
    endload @ over u>  if
      1+ dup ?load  if  ."  #" dup .  block !  true  |
    else  drop  false  |  again ;
: endread  block @ nextblock  if  loading off  |
  ['] listen is query  tib dup >in ! >limit !
  locked @ 0=  if  ['] (prompt) is prompt  then ;
: readblock   loading @ dup bottomrow =  if  drop  endread  |
  0 >loadbuf columns @ -trailing >r tib r@ cmove
  tib >in !  tib r> + >limit !  1 loading +! ;
: deleteblock  ( u -- ) (u.) filename filedelete ;
: evalblock
  ['] readblock is query  loading off
  ['] noop is prompt  >limit @ >in !  interpret ;
: switch-block  ( u -- )  modified @  if  drop  |  savepos  edit ;
: next-block  block @ 1+ switch-block ;
: previous-block  block @ 1- ?dup  if  switch-block  then ;
: toggle-block  block @ #shadow 2dup <  if  +  else  -  then  
  switch-block ;
: save-and-eval-block
  locked @  if  save-block  noedit  drop  then
  screen>buf  evalblock ;

\ Search
: addr>point  ( a -- ) screen @ - columns @ u/mod row ! col ! ;
: findrange  ( -- u ) locked @  if  /block  else  /screen  then ;
: find-from  ( a1 u -- a2 f )
  point >screen 1+ findrange offset -  2swap search nip ;
: find-to  ( a1 u -- a2 ) 
  screen @  findrange  2swap search 2drop ;
: findword  below-point ?dup 0=  if  drop  |
  0 cursor  2dup find-from  if  nip nip  else  drop  find-to  then
  addr>point  redraw/mark  1 cursor ;
variable seen   2variable wstr
: getblock  ( u -- f ) loadbuf @ swap fileblock ;
: (where)  ( u -- ) >r  loadbuf @ /block  begin
    wstr 2@  search 0=  if
      2drop r>drop  |
    seen @ r@ <>  if  [char] # emit  r@ dup . cr seen !  then
    over loadbuf @ - columns @ u/ 0 >loadbuf columns @ type
    1 /string
  again ;

\ Error handling and exit
: error  ['] (prompt) is prompt  modified off
  locked @  if  noedit drop  then
  loading @  if ['] listen is query  loading off  endload off
    then  (abort) ;
: exitforth  locked @ 0= modified @ 0= or  if  bye  then ;
: toggle  locked @ 0=  if  
    block @ ?dup if  
      loadblock  enteredit  restorepos  then  |
  noedit  if  abort  then ;

\ Status bar
: drawstatus  reverse  bottomrow drawrow  normal ;
: editstatus  bottomrow >row dup>r columns @ blank
  row @ 1+ (u.) r@ swap cmove
  col @ 1+ (u.) r@ 3 + swap cmove
  <# block @ #s  [char] # hold #>  r@ 6 + swap cmove
  /snarfed @ ?dup  if  <#  [char] ) hold  #s  [char] ( hold #>
    r@ 14 + swap cmove  then
  unused (u.) r@ 28 + swap cmove
  modified @  if  127  else  bl  then  r> columns @ 1- + c!
  drawstatus ;

\ Input grab
variable bdigits    variable >bdigits
defer grabber  ( f -- )
: grab-goto  ( f -- )
  if  bdigits @ edit  else  status  then ;
: grab-copy  ( f -- )
  if  bdigits @ block !  modified on  then  status ;
: grab-move  ( f -- )
  if  block @ deleteblock  bdigits @ block !  modified on
    then  status ;
: grab-cursor  127 >bdigits @ c!  drawstatus ;
: ungrab  ( f -- ) grabber
  1 cursor  events ;
: add-digit  ( c -- )
  bdigits @ 10 * over [char] 0 - + bdigits !
  >bdigits @ c!  1 >bdigits +!  grab-cursor ;
: grab-decode  ( c -- )
  dup [char] 0  [char] 9 1+ within  if  add-digit  true  |
  27  ->  false ungrab  false  |
  13  ->  true ungrab  false  |
  drop  true ;
: grabbing  jkey grab-decode  if  brk  then ;
: limbo  no-events  ['] grabbing jvector  
  0 pointer  0 cursor  brk ;
: grab  ( xt a u -- ) locked @ 0= ?exit
  bottomrow >row swap cmove  is grabber
  bdigits off  bottomrow 7 >screen >bdigits !
  grab-cursor  limbo ;

\ Key handlers
: (ctrl-key/locked)  ( key -- key|0 )
  [char] r  ->  ['] grab-copy  s" copy:"  grab  0  |
  [char] m  ->  ['] grab-move  s" move:"  grab  0  |
  [char] g  ->  ['] grab-goto  s" goto:"  grab  0  |
  [char] s  ->  save-block  0  |  drop  0 ;
: (ctrl-key)  ( key -- key|0 )
  [char] a  ->  start  0  |  
  [char] c  ->  terminate 0  |
  [char] e  ->  end  0 | 
  [char] f  ->  findword  0  |
  [char] k  ->  rkill  0  |  
  [char] u  ->  lkill  0  |  
  [char] v  ->  paste  0  |
  [char] x  ->  copy-marked  0  |  
  [char] y  ->  paste  0  |
  [char] d  ->  delete  0  |
  13  ->  toggle-mark  0  |
  locked @  if  r>drop  (ctrl-key/locked)  |
  [char] l  ->  page  0  |  drop  0 ;
: recall/previous  locked @  if  previous-block  else  recall then ;
: (handle-button)  ( key but -- key|0 )
  h# 1  ->  ctrl-key  |  
  8  ->  home  drop 0  |
  h# 14  ->  recall/previous  drop 0  |  
  h# 24  ->  next-block  drop 0  |  
  h# 44  ->  toggle-block  drop 0  |
  h# 84  ->  toggle-block  drop 0  |
  h# 10  ->  up  drop 0  |  
  h# 20  ->  down  drop 0  |  
  h# 40  ->  left  drop 0  |  
  h# 80  ->  right  drop 0  |  drop ;
: (other-key)  ( key -- )
  0  ->  | 
  9  ->  tab  |
  27  ->  toggle  |
  8  -> rubout  |  
  13  ->  mark @  if  copy-marked  else  
    enter  no-events  update  r>drop  then  |  
  insert ;

\ Set hooks
' exitedit is noedit  ' editstatus is status
' exitforth is terminate
' (ctrl-key) is ctrl-key
' (other-key) is other-key
' (handle-button) is handle-button

\ External interface and helper words
only definitions also editor
' (stdin) is stdin

\ `blk`  ( -- a ) Return address of currently edited block
: blk  ( -- a ) block ;

\ `(page)`  ( -- ) Default action for `page` which clears the screen
: (page)  screen @ rows @ columns @ * blank  redraw  home ;
' (page) is page

\ `at-xy`  ( x y -- ) Position cursor
: at-xy  ( x y -- ) 0 cursor
  0 rows @ clamp row !  0 columns @ clamp col ! 
  1 cursor ;

\ "Clickable" editor commands 
: Save  save-block ;
: Snarf  copy-marked ;
: Paste  paste ;
: Load  save-and-eval-block ;
: Next  next-block ;
: Prev  previous-block ;
: Abort  modified off  noedit  drop ;
: Doc  block @ #shadow <  if  toggle-block  then ;
: Code  block @ #shadow >=  if  toggle-block  then ;

\ `load`  ( u -- ) Load and evaluate block u
: load  ( u -- )  endload off  load1 evalblock  ;

\ `thru`  ( u1 u2 -- ) Load and evaluate blocks u1 to u2 (inclusive)
: thru  ( u1 u2 -- ) endload !  1- nextblock drop  evalblock ;

\ `(edit)`  ( u -- ) Default behaviour of `edit`
: (edit)  ( u -- ) loadblock  enteredit ;
' (edit) is edit

\ `export`  ( u1 u2 -- ) Load blocks u1 to u2 (inclusive) and write them to the
\   textual console output channel
: export  ( u1 u2 -- )
  1+ swap  ?do  
    loadbuf @ i fileblock 0= abort" no such block"
    writeblock
  loop ;

\ `\s`  ( | ... -- ) Skip block - comments out rest of block
: \s  >limit @ >in !
  loading @  if  bottomrow loading !  then ;

\ `list`  ( u -- ) List block u
: list  load1 loadbuf @  rows @  1  do
    dup columns @ -trailing cr type  columns @ +  loop  drop ;

\ `where`  ( u1 u2 | <word> -- ) Scan blocks u1 to u2 for the next word in the input
\   Stream and list blocks containing occurrences and their respective lines
: where  seen off
  bl word count wstr 2! 1+ swap  ?do
    i getblock  if  i (where)  then
  loop ;

\ `banner`  ( -- ) Print UF banner
defer banner
: _banner  10 4 at-xy  ." UF/" version .
  ." - "  copyright type  cr  10 spaces
  unused u. ." bytes free."
  icon spritedata  6 6 position 
  h# 76 auto  8 0  do  h# 41 sprite  loop  0 auto
  0 10 at-xy  prompt ;
' _banner is banner

\ `boot`  ( -- ) Boot code for full environment, sets up graphics and event handlers
: boot  ['] catcher evector  initscreen  initcolors  theme
  dirty off  locked off  modified off
  ['] error is abort  ['] listen is query  ['] gemit is emit
  home  banner  quit ;

only
\ Overwrite `new` to wipe everything from here
marker new

\ Save full graphical system as "ufx.rom"

.( saving ufx.rom ... )
' (prompt) is prompt
save ufx.rom
bye
