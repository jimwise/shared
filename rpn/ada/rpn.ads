with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps;
with Ada.Long_Float_Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;

with Stacks;

package RPN is
   subtype Num is Long_Float;
   package Num_IO renames Ada.Long_Float_Text_IO;
   package Num_Functions is new Ada.Numerics.Generic_Elementary_Functions(Num);

   package Num_Stacks is new Stacks(Element_Type => Num);
   type Num_Stack is new Num_Stacks.Stack;
   type Op_Dict is private;

   procedure Signal_Error (S : in Unbounded_String);
   procedure Signal_Error (S : in String);

   procedure Act (Dict	: in     Op_Dict;
		  Stack	: in out Num_Stack;
		  S	: in     Unbounded_String);

private
   type Op_Action is access procedure (Stack : in out Num_Stack);
   type Op is record
      Doc : Unbounded_String;
      Arity : Natural;
      Action : Op_Action;
   end record;

   package Op_Dicts is new Ada.Containers.Ordered_Maps(Key_Type => Unbounded_String,
						       Element_Type => Op);
   type Op_Dict is new Op_Dicts.Map with record null; end record;

   procedure Operate (Stack : in out Num_Stack;
		      O	    : in     Op);

end;
