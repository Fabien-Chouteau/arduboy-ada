with Interfaces; use Interfaces;

with Arduboy.Screen;
with Arduboy.Font;

package body Arduboy.Text is

   Cursor_X : Integer_16 := 0;
   Cursor_Y : Integer_16 := 0;
   Text_Size : Arduboy.Screen.Char_Size := 1;

   Char_Height : constant Integer_16 := 7;
   Char_Width  : constant Integer_16 := 5;

   ----------------
   -- Set_Cursor --
   ----------------

   procedure Set_Cursor (X, Y : Interfaces.Integer_16) is
   begin
      Cursor_X := X;
      Cursor_Y := Y;
   end Set_Cursor;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
      use Arduboy.Font;

   begin
      if C = ASCII.CR then
         return;
      end if;

      if C = ASCII.LF then
         New_Line;
      else
         Screen.Draw_Char ((Cursor_X, Cursor_Y), C, Size => Text_Size);
         Cursor_X := Cursor_X + Full_Char_Width * Text_Size;
      end if;

   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Str : String) is
   begin
      for C of Str loop
         Put (C);
      end loop;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (I : Interfaces.Integer_32) is
      Line_Str : String (1 .. 11) := (others => '0');
      L : Integer_32;
      Index : Integer := Line_Str'Last;
   begin

      if I = 0 then
         Put ("0");
      elsif I = Integer_32'First then
         Put ("-2147483648");
         return;
      end if;

      L := abs I;
      while L /= 0 loop
         Line_Str (Index) :=
           Character'Enum_Val (Character'Enum_Rep ('0') + L mod 10);
         L := L / 10;
         Index := Index - 1;
      end loop;

      if I < 0 then
         Line_Str (Index) := '-';
         Index := Index - 1;
      end if;

      Put (Line_Str (Index + 1 .. Line_Str'Last));
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (U : Interfaces.Unsigned_16; Base : Natural := 10) is
   begin
      null;
   end Put;

   --------------
   -- New_Line --
   --------------

   procedure New_Line is
   begin
      Cursor_X := 0;
      Cursor_Y := Cursor_Y + Char_Height * Text_Size;
   end New_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (C : Character) is
   begin
      Put (C);
      New_Line;
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (Str : String) is
   begin
      Put (Str);
      New_Line;
   end Put_Line;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (I : Interfaces.Integer_32) is
   begin
      Put (I);
      New_Line;
   end Put_Line;

end Arduboy.Text;
