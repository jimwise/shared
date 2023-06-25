with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

package body RPN is
   procedure Signal_Error (S : Unbounded_String) is
   begin
      Put_Line (S);
   end Signal_Error;
   procedure Signal_Error (S : String) is
   begin
      Put_Line (S);
   end Signal_Error;

   procedure Act
     (Dict  :        Op_Dict;
      Stack : in out Num_Stack;
      S     :        Unbounded_String)
   is
      C : Op_Dicts.Cursor;
      N : Num;
      use Op_Dicts;
   begin
      C := Dict.Find (S);
      if C /= Op_Dicts.No_Element then
         Operate (Stack, Element (C));
      else
         begin
            N := Num'Value (To_String (S));
            Push (Stack, N);
         exception
            when Constraint_Error =>
               Signal_Error ("Unknown Operation: " & S);
         end;
      end if;
   end Act;

   procedure Operate (Stack : in out Num_Stack; O : Op) is
   begin
      if Size (Stack) < O.Arity then
         Signal_Error ("Stack Underflow");
      else
         O.Action (Stack);
      end if;
   end Operate;

end RPN;
