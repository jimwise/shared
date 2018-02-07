with Ada.Text_IO;                   use Ada.Text_IO;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with RPN; use RPN;
with RPN.Ops;

procedure Main is
   Dict : Op_Dict renames Ops.Dict;
   Stack : Num_Stack;
   S     : Unbounded_String;
begin
   Put ("> ");
   Flush;
   while not End_Of_File loop
      S := Get_Line;
      Act (Dict, Stack, S);
      Put ("> ");
      Flush;
   end loop;
end Main;
