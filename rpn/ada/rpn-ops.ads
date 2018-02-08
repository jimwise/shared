package RPN.Ops is
   pragma Elaborate_Body;
   Dict : Op_Dict;
private
   procedure Make_Op
     (Key    : in Unbounded_String;
      Doc    : in Unbounded_String;
      Arity  : in Natural;
      Action : in Op_Action);

   procedure Show (S : in out Num_Stack);
   procedure Size (S : in out Num_Stack);
   procedure Plus (S : in out Num_Stack);
   procedure Minus (S : in out Num_Stack);
   procedure Times (S : in out Num_Stack);
   procedure Div_By (S : in out Num_Stack);
   procedure Expt (S : in out Num_Stack);
   procedure Drop (S : in out Num_Stack);
   procedure Dup (S : in out Num_Stack);
   procedure Swap (S : in out Num_Stack);
   procedure Help (S : in out Num_Stack);

   procedure Command_Help (C : in Op_Dicts.Cursor);
end RPN.Ops;
