with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Integer_Text_IO;           use Ada.Integer_Text_IO;
with Ada.Text_IO;                   use Ada.Text_IO;

package body RPN.Ops is
   use Num_Functions;
   use Num_IO;
   use Op_Dicts;

   procedure Make_Op
     (Key    : in Unbounded_String;
      Doc    : in Unbounded_String;
      Arity  : in Natural;
      Action : in Op_Action)
   is
      O : constant Op := (Doc => Doc, Arity => Arity, Action => Action);
   begin
      Dict.Insert (Key, O);
   end Make_Op;

   function Str
     (S : String) return Unbounded_String renames
     To_Unbounded_String;

   procedure Show (S : in out Num_Stack) is
      X : constant Num := Pop (S);
   begin
      Num_IO.Put (X, Fore => 0, Exp => 0);
      New_Line;
      Push (S, X);
   end Show;

   procedure Size (S : in out Num_Stack) is
   begin
      Put (Size (S), Width => 0);
      New_Line;
   end Size;

   procedure Plus (S : in out Num_Stack) is
      Y : constant Num := Pop (S);
      X : constant Num := Pop (S);
   begin
      Push (S, X + Y);
   end Plus;

   procedure Minus (S : in out Num_Stack) is
      Y : constant Num := Pop (S);
      X : constant Num := Pop (S);
   begin
      Push (S, X - Y);
   end Minus;

   procedure Times (S : in out Num_Stack) is
      Y : constant Num := Pop (S);
      X : constant Num := Pop (S);
   begin
      Push (S, X * Y);
   end Times;

   procedure Div_By (S : in out Num_Stack) is
      Y : constant Num := Pop (S);
      X : constant Num := Pop (S);
   begin
      Push (S, X / Y);
   end Div_By;

   procedure Expt (S : in out Num_Stack) is
      Y : constant Num := Pop (S);
      X : constant Num := Pop (S);
   begin
      Push (S, X**Y);
   end Expt;

   procedure Drop (S : in out Num_Stack) is
      X : constant Num := Pop (S);
      pragma Unreferenced (X);
   begin
      null;
   end Drop;

   procedure Dup (S : in out Num_Stack) is
      X : constant Num := Pop (S);
   begin
      Push (S, X);
      Push (S, X);
   end Dup;

   procedure Swap (S : in out Num_Stack) is
      Y : constant Num := Pop (S);
      X : constant Num := Pop (S);
   begin
      Push (S, Y);
      Push (S, X);
   end Swap;

   procedure Help (S : in out Num_Stack) is
      pragma Unreferenced (S);
   begin
      Put (Integer (Dict.Length), 0);
      Put_Line (" commands:");
      Iterate (Dict, Command_Help'Access);
   end Help;

   procedure Command_Help (C : in Op_Dicts.Cursor) is
   begin
      Put_Line (Key (C) & " -- " & Element (C).Doc);
   end Command_Help;

begin
   Make_Op (Str ("."), Str ("display top value on the stack"), 1, Show'Access);
   Make_Op
     (Str ("#"),
      Str ("display number of values on the stack"),
      0,
      Size'Access);

   Make_Op
     (Str ("+"),
      Str ("replace top two values on the stack with their sum"),
      2,
      Plus'Access);
   Make_Op
     (Str ("-"),
      Str ("replace top two values on the stack with their difference"),
      2,
      Minus'Access);
   Make_Op
     (Str ("*"),
      Str ("replace top two values on the stack with their product"),
      2,
      Times'Access);
   Make_Op
     (Str ("/"),
      Str ("replace top two values on the stack with their quotient"),
      2,
      Div_By'Access);

   Make_Op
     (Str ("^"),
      Str ("replace top two values on the stack," &
             "x and y, with x to the yth power"),
      2,
      Expt'Access);

   Make_Op
     (Str ("drop"),
      Str ("remove top value from the stack"),
      1,
      Drop'Access);
   Make_Op
     (Str ("dup"),
      Str ("duplicate top value on the stack"),
      1,
      Dup'Access);
   Make_Op
     (Str ("swap"),
      Str ("swap top two values on the stack"),
      2,
      Swap'Access);

   Make_Op (Str ("help"), Str ("show this help"), 0, Help'Access);
end RPN.Ops;
