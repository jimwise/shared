with Ada.Numerics.Generic_Complex_Types;
with Ada.Text_IO.Complex_IO;

--  with Ada.Text_IO;
--  use Ada.Text_IO;
--  with Ada.Integer_Text_IO;
--  use Ada.Integer_Text_IO;

with Png_IO;

procedure Mandelbrot is
   package Complex_Types is new Ada.Numerics.Generic_Complex_Types (Long_Float);
   use Complex_Types;
   package Complex_IO is new Ada.Text_IO.Complex_IO (Complex_Types);
   use Complex_IO;

   Width : constant Positive := 1280;
   Height : constant Positive := 800;

   Cap : constant Positive := 1000;
   Density_Factor : constant Positive := 192;

   function Escape (C : in Complex) return Natural is
      Z : Complex := Compose_From_Cartesian (Re => 0.0, Im => 0.0);
   begin
      for I in 0 .. Cap
      loop
	 Z := (Z ** 2) + C;
	 if abs Z > 2.0 then return I; end if;
      end loop;
      return 0;
   end;

   procedure Mandelbrot_Image (File_Name : in String;
			       Width	 : in Positive;
			       Height	 : in Positive;
			       X_Min	 : in Long_Float := -2.5;
			       X_Max	 : in Long_Float := 1.0;
			       Y_Min	 : in Long_Float := -1.0;
			       Y_Max	 : in Long_Float := 1.0) is
      X_Step : constant Long_Float := abs (X_Max - X_Min) / Long_Float(Width);
      Y_Step : constant Long_Float := abs (Y_Max - Y_Min) / Long_Float(Height);

      type Image is array (0 .. Width-1, 0 .. Height-1) of Natural;
      I : Image;

      function To_Red (I : Image; R, C : Png_IO.Coordinate) return Natural is
	 V : constant Natural := I(C, R) * Density_Factor mod 256;
      begin
	 --  Put(I(C, R)); Put(" I"); New_Line; Put(V); Put(" R"); New_Line;
	 return V;
      end;

      function To_Green (I : Image; R, C : Png_IO.Coordinate) return Natural is
	 V : constant Natural := (I(C, R) * (Density_Factor**2) / 256) mod 256;
      begin
	 --  Put(V); Put(" G"); New_Line;
	 return V;
      end;

      function To_Blue (I : Image; R, C : Png_IO.Coordinate) return Natural is
	 V : constant Natural := (I(C, R) * (Density_Factor**3) / (256**2) )mod 256;
      begin
	 --  Put(V); Put(" B"); New_Line;
	 return V;
      end;

      procedure Write_PNG2 is new Png_IO.Write_PNG_Type_2(Image, Natural,
							  To_Red, To_Green, To_Blue);
   begin
      for Y in 0 .. Height - 1
      loop
	 declare
	    Y_1 : constant Long_Float := Long_Float(Y) * Y_Step + Y_Min;
	 begin
	    for X  in 0 .. Width - 1
	    loop
	       declare
		  X_1 : constant Long_Float := Long_Float(X) * X_Step + X_Min;
		  C : constant Complex := Compose_From_Cartesian(Re => X_1, Im => Y_1);
		  N : constant Natural := Escape(C);
	       begin
		  I(X, Y) := N;
	       end;
	    end loop;
	 end;
      end loop;
      Write_PNG2(File_Name, I, Width, Height);
   end Mandelbrot_Image;

begin
   Mandelbrot_Image("mandelbrot-ada.png", Width, Height);
   Mandelbrot_Image("mandelzoom-ada.png", Width, Height, -0.5, 0.5, 0.0, 0.75);
end Mandelbrot;
