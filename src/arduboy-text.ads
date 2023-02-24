with Interfaces;

package Arduboy.Text is

   procedure Set_Cursor (X, Y : Interfaces.Integer_16);

   procedure Put (C : Character);
   procedure Put (Str : String);
   procedure Put (I : Interfaces.Integer_32);

   procedure New_Line;

   procedure Put_Line (C : Character);
   procedure Put_Line (Str : String);
   procedure Put_Line (I : Interfaces.Integer_32);

end Arduboy.Text;
