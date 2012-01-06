#! /usr/local/lib/factor/factor

USING: assocs fry io kernel locals math math.functions math.parser prettyprint sequences vectors ;

IN: rpn

: signal-error ( str -- ) "*** ERROR: " prepend print ;

: literal ( oldstack str -- newstack )
  string>number dup [ >float over push ] [ drop "unknown operation" signal-error ] if ;

: unary-arg-setup ( oldstack -- newstack x ) dup pop ;
: binary-arg-setup ( oldstack -- newstack x y ) dup [ pop ] [ pop ] bi swap ;
: ret ( oldstack value -- newstack ) over push ;

! nullary/unary/binary take the body of a unary operation (as a quotation,
! and return a quotation implementing that op with proper underflow checking
! and arg setup

: nullary ( quot -- quot ) ;
: unary ( quot -- quot ) '[ dup length 1 >= [ unary-arg-setup @ ] [ "stack underflow" signal-error ] if ] ;
: binary ( quot -- quot ) '[ dup length 2 >= [ binary-arg-setup @ ] [ "stack underflow" signal-error ] if ] ;


: add-op ( assoc quot sym -- assoc ) rot dup [ set-at ] dip ;

: setup-ops ( -- assoc )
  H{ }
  [ dup . ret ]                 unary   "." add-op
  [ dup length . ]              nullary "#" add-op
  [ + ret ]                     binary  "+" add-op
  [ - ret ]                     binary  "-" add-op
  [ * ret ]                     binary  "*" add-op
  [ / ret ]                     binary  "/" add-op
  [ ^ ret ]                     binary  "^" add-op
  [ drop ]                      unary   "drop" add-op
  [ dup [ over push ] dip ret ] unary   "dup" add-op
  [ swap [ ret ] dip ret ]      binary  "swap" add-op
  ;

: setup-stack ( -- stack )
  V{ }
  ;

: action ( ops oldstack str -- ops newstack )
  3dup swap drop
  ! ops oldstack str ops str
  swap at
  ! ops oldstack str quot-or-f
  dup
  [ swap drop call( oldstack -- newstack ) ]
  [ drop literal ]
  if
  ;

: main ( -- )
  setup-ops setup-stack
  "> " write flush
  readln dup [ action main ] [ drop ] if
  "" print
  drop drop ;

main
