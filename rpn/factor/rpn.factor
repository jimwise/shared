#! /usr/local/lib/factor/factor

USING: accessors assocs fry io kernel locals math math.functions math.parser prettyprint sequences system vectors ;

IN: rpn

TUPLE: op doc act ;

: <op> ( quot doc -- op ) op new swap >>doc swap >>act ;

: signal-error ( str -- ) "*** ERROR: " prepend print ;

: literal ( oldstack str -- newstack )
  string>number dup [ >float over push ] [ drop "unknown operation" signal-error ] if ;

: unary-arg-setup ( oldstack -- newstack x ) dup pop ;
: binary-arg-setup ( oldstack -- newstack x y ) dup [ pop ] [ pop ] bi swap ;
: ret ( oldstack value -- newstack ) over push ;

! nullary/unary/binary take the body of a unary operation (as a quotation),
! and return a quotation implementing that op with proper underflow checking
! and arg setup

: nullary ( quot -- quot ) ;
: unary ( quot -- quot )
  '[ dup length 1 >= [ unary-arg-setup @ ] [ "stack underflow" signal-error ] if ] ;
: binary ( quot -- quot )
  '[ dup length 2 >= [ binary-arg-setup @ ] [ "stack underflow" signal-error ] if ] ;

: add-op ( assoc quot doc sym -- assoc ) [ <op> ] dip rot dup [ set-at ] dip ;

: show-help ( ops stack -- ops stack )
  over dup keys length number>string write " Commands: " print
  [ swap write " -- " write doc>> print ] assoc-each
  ;

: setup-ops ( -- assoc )
  H{ }
  [ dup . ret ] unary "display the top value on the stack" "." add-op
  [ dup length . ] nullary "display the number of values on the stack" "#" add-op
  [ + ret ] binary "replace the top two values on the stack with their sum" "+" add-op
  [ - ret ] binary "replace the top two values on the stack with their difference" "-" add-op
  [ * ret ] binary "replace the top two values on the stack with their product" "*" add-op
  [ / ret ] binary "replace the top two values on the stack with their quotient" "/" add-op
  [ ^ ret ] binary
    "replace the top two values on the stack, x and y, with x to the yth power" "^" add-op
  [ drop ] unary "remove the top value from the stack" "drop" add-op
  [ dup [ over push ] dip ret ] unary "duplicate the top value on the stack" "dup" add-op
  [ swap [ ret ] dip ret ] binary "swap the top two values on the stack" "swap" add-op
  [ show-help ] nullary "display this help" "help" add-op
  ;

: setup-stack ( -- stack ) V{ } ;

: action ( ops oldstack str -- ops newstack )
  3dup swap drop
  ! ops oldstack str ops str
  swap at
  ! ops oldstack str op-or-f
  dup
  [ swap drop act>> call( oldstack -- newstack ) ]
  [ drop literal ]
  if
  ;

: prompt ( -- ) "> " write flush ;

: main ( -- )
  setup-ops setup-stack
  prompt
  readln dup [ action main ] [ drop ] if
  "" print flush
  0 exit ;

main
