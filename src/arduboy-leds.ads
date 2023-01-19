package Arduboy.LEDs
with Elaborate_Body
is

   type LED_Id is (Red, Green, Blue, TX, RX);

   procedure On (Id : LED_Id);
   procedure Off (Id : LED_Id);

end Arduboy.LEDs;
