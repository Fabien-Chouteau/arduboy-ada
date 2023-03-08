with Interfaces;

package Game_Interface
with SPARK_Mode => On,
     Abstract_State => (Time),
     Initializes => (Time)
is
   type Button_State is private;

   procedure Setup_Game;

   procedure Wait_Next_Frame (State : out Button_State);

   procedure Draw_Pixel (X, Y : Interfaces.Integer_16);

   procedure Set_Text_Cursor (X, Y : Interfaces.Integer_16);
   procedure Put (Str : String);
   procedure Put (V : Interfaces.Integer_32);
   procedure Draw_Vertical_Line (X, Y : Interfaces.Integer_16;
                                 Height : Interfaces.Integer_16);
   procedure Draw_Horizontal_Line (X, Y : Interfaces.Integer_16;
                                   Width : Interfaces.Integer_16);

   procedure Get_Time_Ms (Ms : out Interfaces.Unsigned_32);

   type Button_Kind is (B, A, Down, Left, Right, Up);

   function Pressed (State : Button_State;
                     Button : Button_Kind)
                     return Boolean;

   function Just_Pressed (State : Button_State;
                          Button : Button_Kind)
                          return Boolean;

private
   type Button_State is new Interfaces.Unsigned_8;
end Game_Interface;
