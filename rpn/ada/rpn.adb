with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;
use Ada.Strings.Unbounded.Text_IO;

package body RPN is
   procedure Signal_Error (S : in Unbounded_String) is
   begin
      Put_Line(S);
   end;
   procedure Signal_Error (S : in String) is
   begin
      Put_Line(S);
   end;

   procedure Act (Dict	: in     Op_Dict;
		  Stack	: in out Num_Stack;
		  S	: in     Unbounded_String) is
      C : Op_Dicts.Cursor;
      N : Num;
      use Op_Dicts;
   begin
      C := Dict.Find(S);
      if C /= Op_Dicts.No_Element
      then
	    Operate(Stack, Element(C));
      else
	 begin
	    N := Num'Value(To_String(S));
	    Push(Stack, N);
	 exception
	    when Constraint_Error =>
	       Signal_Error("Unknown Operation: " & S);
	 end;
      end if;
   end Act;

   procedure Operate (Stack : in out Num_Stack;
		      O	    : in     Op) is
   begin
      if Size(Stack) < O.Arity
      then
	 Signal_Error("Stack Underflow");
      else
	 O.Action(Stack);
      end if;
   end;


end RPN;
