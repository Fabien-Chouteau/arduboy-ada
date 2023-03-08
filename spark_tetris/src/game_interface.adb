with Arduboy.Screen;  use Arduboy.Screen;
with Arduboy.Buttons; use Arduboy.Buttons;
with Arduboy.Frames;  use Arduboy.Frames;
with Arduboy.Text;

package body Game_Interface
with SPARK_Mode => Off
is

   ----------------
   -- Setup_Game --
   ----------------

   procedure Setup_Game is
   begin
      Set_Frame_Rate (60);

      Boot_Logo;
   end Setup_Game;

   ---------------------
   -- Wait_Next_Frame --
   ---------------------

   procedure Wait_Next_Frame (State : out Button_State) is
   begin
      Arduboy.Screen.Display (Clear => True);

      while not Next_Frame loop
         null;
      end loop;

      Poll_Buttons;
      State := 0;
   end Wait_Next_Frame;

   ----------------
   -- Draw_Pixel --
   ----------------

   procedure Draw_Pixel (X, Y : Interfaces.Integer_16) is
   begin
      Draw_Pixel ((X, Y));
   end Draw_Pixel;

   ---------------------
   -- Set_Text_Cursor --
   ---------------------

   procedure Set_Text_Cursor (X, Y : Interfaces.Integer_16) is
   begin
      Arduboy.Text.Set_Cursor (X, Y);
   end Set_Text_Cursor;

   ---------
   -- Put --
   ---------

   procedure Put (Str : String) is
   begin
      Arduboy.Text.Put (Str);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (V : Interfaces.Integer_32) is
   begin
      Arduboy.Text.Put (V);
   end Put;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   procedure Draw_Vertical_Line (X, Y : Interfaces.Integer_16;
                                 Height : Interfaces.Integer_16)
   is
   begin
      Draw_Vertical_Line ((X, Y), Height);
   end Draw_Vertical_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   procedure Draw_Horizontal_Line (X, Y : Interfaces.Integer_16;
                                   Width : Interfaces.Integer_16)
   is
   begin
      Draw_Horizontal_Line ((X, Y), Width);
   end Draw_Horizontal_Line;

   -----------------
   -- Get_Time_Ms --
   -----------------

   procedure Get_Time_Ms (Ms : out Interfaces.Unsigned_32) is
   begin
      Ms := Arduboy.Clock_Ms;
   end Get_Time_Ms;

   ------------------
   -- To_AB_Button --
   ------------------

   function To_AB_Button (Button : Button_Kind)
                          return Arduboy.Buttons.Button_Kind
   is (case Button is
          when A => Arduboy.Buttons.A,
          when B => Arduboy.Buttons.B,
          when Up => Arduboy.Buttons.Up,
          when Down => Arduboy.Buttons.Down,
          when Left => Arduboy.Buttons.Left,
          when Right => Arduboy.Buttons.Right);

   -------------
   -- Pressed --
   -------------

   function Pressed (State : Button_State;
                     Button : Button_Kind)
                     return Boolean
   is
      pragma SPARK_Mode (Off);
      pragma Unreferenced (State);
   begin
      return Arduboy.Buttons.Pressed (To_AB_Button (Button));
   end Pressed;

   ------------------
   -- Just_Pressed --
   ------------------

   function Just_Pressed (State : Button_State;
                          Button : Button_Kind)
                          return Boolean
   is
      pragma SPARK_Mode (Off);
      pragma Unreferenced (State);
   begin
      return Arduboy.Buttons.Just_Pressed (To_AB_Button (Button));
   end Just_Pressed;

end Game_Interface;
