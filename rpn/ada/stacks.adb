package body Stacks is
   procedure Push (S : in out Stack;
		     X : in Element_Type) is
     begin
	S.Append(X);
     end Push;

     function Pop (S: in out Stack) return Element_Type is
	Res: Element_Type;
     begin
	if Is_Empty(S)
	then
	   raise Stack_Underflow;
	end if;
	Res := Last_Element(S);
	S.Delete_Last;
	return Res;
     end Pop;

     function Size (S: in Stack) return Natural is
     begin
	return Natural(Length(S));
     end Size;
end Stacks;
