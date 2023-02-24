with Interfaces; use Interfaces;
with AVR.MCU; use AVR.MCU;
with AVR.Wait;

with System.Machine_Code;

package body Arduboy.Buttons is

   Current_Buttons_State : Unsigned_8 := 0;
   Previous_Buttons_State : Unsigned_8 := 0;

   -------------------
   -- Buttons_State --
   -------------------

   function Buttons_State return Unsigned_8 is
      Buttons : Unsigned_8;
   begin

      Buttons := (not PINF) and (Up'Enum_Rep or Right'Enum_Rep or
                                   Left'Enum_Rep or Down'Enum_Rep);

      if (A_Button_Portin and A_Button_Mask) = 0 then
         Buttons := Buttons or A'Enum_Rep;
      end if;

      if (B_Button_Portin and B_Button_Mask) = 0 then
         Buttons := Buttons or B'Enum_Rep;
      end if;

      return Buttons;
   end Buttons_State;

   -------------
   -- Pressed --
   -------------

   function Pressed (Button : Button_Kind) return Boolean is
   begin
      return Pressed (Button'Enum_Rep);
   end Pressed;

   -------------
   -- Pressed --
   -------------

   function Pressed (Button_Mask : Unsigned_8) return Boolean is
   begin
      return (Buttons_State and Button_Mask) = Button_Mask;
   end Pressed;

   -----------------
   -- Any_Pressed --
   -----------------

   function Any_Pressed (Button_Mask : Unsigned_8) return Boolean is
   begin
      return (Buttons_State and Button_Mask) /= 0;
   end Any_Pressed;

   ------------------
   -- None_Pressed --
   ------------------

   function None_Pressed (Button_Mask : Unsigned_8) return Boolean is
   begin
      return (Buttons_State and Button_Mask) = 0;
   end None_Pressed;

   ------------------
   -- Poll_Buttons --
   ------------------

   procedure Poll_Buttons is
   begin
      Previous_Buttons_State := Current_Buttons_State;
      Current_Buttons_State := Buttons_State;
   end Poll_Buttons;

   ------------------
   -- Just_Pressed --
   ------------------

   function Just_Pressed (Button : Button_Kind) return Boolean
   is (((Previous_Buttons_State and Button'Enum_Rep) = 0)
       and then
         ((Current_Buttons_State and Button'Enum_Rep) /= 0));

   -------------------
   -- Just_Released --
   -------------------

   function Just_Released (Button : Button_Kind) return Boolean
   is (((Previous_Buttons_State and Button'Enum_Rep) /= 0)
       and then
         ((Current_Buttons_State and Button'Enum_Rep) = 0));

   ---------------------
   -- Wait_No_Buttons --
   ---------------------

   procedure Wait_No_Buttons is
   begin
      loop
         Delay_Ms (50);
         exit when Buttons_State = 0;
      end loop;
   end Wait_No_Buttons;

end Arduboy.Buttons;
