with Arduboy.LEDs;

package body Arduboy.Frames is
   Each_Frame_Millis : Unsigned_8 := 16;
   Last_Frame_Duration : Unsigned_8 := 0;
   This_Frame_Start : Unsigned_8 := 0;
   Just_Rendered : Boolean := False;
   Frame_Count : Unsigned_16 := 0;

   --------------------
   -- Set_Frame_Rate --
   --------------------

   procedure Set_Frame_Rate (Rate : Unsigned_8) is
   begin
      Each_Frame_Millis := Unsigned_8 ((1000 / Unsigned_16 (Rate)) and 16#FF#);
   end Set_Frame_Rate;

   ------------------------
   -- Set_Frame_Duration --
   ------------------------

   procedure Set_Frame_Duration (Duration : Unsigned_8) is
   begin
      Each_Frame_Millis := Duration;
   end Set_Frame_Duration;

   ----------------
   -- Next_Frame --
   ----------------

   function Next_Frame (Show_Overload : Boolean := False) return Boolean is
      Now : Unsigned_8 := Unsigned_8 (Clock_Ms and 16#FF#);
      Frame_Duration : constant Unsigned_8 := Now - This_Frame_Start;
   begin
      if Just_Rendered then
         Last_Frame_Duration := Frame_Duration;
         Just_Rendered := False;
         return False;
      elsif Frame_Duration < Each_Frame_Millis then
         Idle;
         return False;
      end if;

      Just_Rendered := True;
      This_Frame_Start := Now;
      Frame_Count := Frame_Count + 1;

      if Show_Overload then
         if Last_Frame_Duration > Each_Frame_Millis then
            LEDs.On (LEDs.TX);
         else
            LEDs.Off (LEDs.TX);
         end if;
      end if;

      return True;
   end Next_Frame;

   --------------------
   -- Every_X_Frames --
   --------------------

   function Every_X_Frames (Frames : Unsigned_8) return Boolean is
   begin
      return (Frame_Count mod Unsigned_16 (Frames)) = 0;
   end Every_X_Frames;

   --------------
   -- CPU_Load --
   --------------

   function CPU_Load return Natural is
   begin
      return Natural ((Unsigned_16 (Last_Frame_Duration) * 100) /
                        Unsigned_16 (Each_Frame_Millis));
   end CPU_Load;

end Arduboy.Frames;
