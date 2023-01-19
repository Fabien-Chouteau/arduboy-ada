package body Arduboy.LEDs is

   --------
   -- On --
   --------

   procedure On (Id : LED_Id) is
   begin
      case Id is
         when Red =>
            RED_LED_PORT := RED_LED_PORT and not RED_LED_Mask;
         when Green =>
            GREEN_LED_PORT := GREEN_LED_PORT and not GREEN_LED_Mask;
         when Blue =>
            BLUE_LED_PORT := BLUE_LED_PORT and not GREEN_LED_Mask;
         when TX =>
            TX_LED_PORT := TX_LED_PORT and not TX_LED_Mask;
         when RX =>
            RX_LED_PORT := RX_LED_PORT and not RX_LED_Mask;
      end case;
   end On;

   ---------
   -- Off --
   ---------

   procedure Off (Id : LED_Id) is
   begin
      case Id is
         when Red =>
            RED_LED_PORT := RED_LED_PORT or RED_LED_Mask;
         when Green =>
            GREEN_LED_PORT := GREEN_LED_PORT or GREEN_LED_Mask;
         when Blue =>
            BLUE_LED_PORT := BLUE_LED_PORT or GREEN_LED_Mask;
         when TX =>
            TX_LED_PORT := TX_LED_PORT or TX_LED_Mask;
         when RX =>
            RX_LED_PORT := RX_LED_PORT or RX_LED_Mask;
      end case;
   end Off;

end Arduboy.LEDs;
