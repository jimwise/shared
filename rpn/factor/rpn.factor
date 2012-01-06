#! /usr/local/lib/factor/factor

USING: assocs io kernel locals math math.functions math.parser prettyprint sequences vectors ;

IN: rpn

: signal-error ( str -- ) "*** ERROR: " prepend print ;

: literal ( oldstack str -- newstack )
  string>number dup [ >float over push ] [ drop "unknown operation" signal-error ] if ;

: nullary ( oldstack -- oldstack ) ;
: unary ( oldstack -- newstack x ) dup pop ;
: binary ( oldstack -- newstack x y ) dup [ pop ] [ pop ] bi swap ;
: ret ( oldstack value -- newstack ) over push ;

: add-op ( assoc quot sym -- assoc ) rot dup [ set-at ] dip ;

: setup-ops ( -- assoc )
  H{ }
  [ unary dup . ret ] "." add-op
  [ nullary dup length . ] "#" add-op
  [ binary + ret ] "+" add-op
  [ binary - ret ] "-" add-op
  [ binary * ret ] "*" add-op
  [ binary / ret ] "/" add-op
  [ binary ^ ret ] "^" add-op
  [ unary drop ] "drop" add-op
  [ unary dup [ over push ] dip ret ] "dup" add-op
  [ binary swap [ ret ] dip ret ] "swap" add-op
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
