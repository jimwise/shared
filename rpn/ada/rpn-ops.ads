package RPN.Ops is
   pragma Elaborate_Body;
   Dict : Op_Dict;
private
   procedure Make_Op
     (Key    : in Unbounded_String;
      Doc    : in Unbounded_String;
      Arity  : in Natural;
      Action : in Op_Action);
end RPN.Ops;
