with Ada.Containers.Doubly_Linked_Lists;

generic
   type Element_Type is private;
package Stacks is
   type Stack is private;
   procedure Push (S : in out Stack;
 X                   : in     Element_Type);
   function Pop (S : in out Stack) return Element_Type;
   function Size (S : in Stack) return Natural;

   Stack_Underflow : exception;
private
   package Element_Stack is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type);
   type Stack is new Element_Stack.List with record
      null;
   end record;
end Stacks;
