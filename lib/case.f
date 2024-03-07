\ case/of
\ http://www.calcentral.com/~forth/forth/eforth/e4.src/LIB.SHTML

0 constant case ( -- 0 ) immediate

: of ( -- sys )
  postpone over postpone = postpone if postpone drop ; immediate

: endof ( sys -- sys ) postpone else ; immediate

: esac ( 0 i*sys -- )
  begin  ?dup while postpone then  repeat ;

: endcase ( 0 i*sys -- ) postpone drop  esac ; immediate
