with Interfaces; use Interfaces;

private with AVR.MCU;
private with System;

package Arduboy
with Elaborate_Body
is
   Screen_Width : constant := 128;
   Screen_Height : constant := 64;

   function Clock_Ms return Unsigned_32;
   procedure Delay_Ms (Count : Unsigned_16);

   procedure Idle;

   procedure Put (C : Character);
   procedure Put (S : String);

private

   type Framebuffer is array
     (Unsigned_16 range 0 .. (Screen_Width * Screen_Height / 8) -1)
       of Interfaces.Unsigned_8;

   procedure Paint_Screen (FB : in out Framebuffer; Clear : Boolean := False);

   procedure OLED_Send_Command (Cmd : Unsigned_8);

   procedure LCH (Msg : System.Address; Line : Integer);
   pragma Export (C, LCH, "__gnat_last_chance_handler");

   use AVR.MCU;

   B_Button_Mask     : constant Unsigned_8 := PORTB4_Mask;
   B_Button_Portin   : Unsigned_8 renames PINB;

   A_Button_Mask     : constant Unsigned_8 := PORTE6_Mask;
   A_Button_Portin   : Unsigned_8 renames PINE;

   Up_Button_Mask    : constant Unsigned_8 := PORTF7_Mask;
   Down_Button_Mask  : constant Unsigned_8 := PORTF4_Mask;
   Left_Button_Mask  : constant Unsigned_8 := PORTF5_Mask;
   Right_Button_Mask : constant Unsigned_8 := PORTF6_Mask;

   SPI_MOSI_Mask     : constant Unsigned_8 := PORTB2_Mask;
   SPI_MISO_Mask     : constant Unsigned_8 := PORTB3_Mask;
   SPI_SCK_Mask      : constant Unsigned_8 := PORTB1_Mask;
   SPI_SS_Mask       : constant Unsigned_8 := PORTB0_Mask;

   OLED_CS_Mask      : constant Unsigned_8 := PORTD6_Mask;
   OLED_CS_PORT      : Unsigned_8 renames PORTD;

   OLED_Rst_PORT     : Unsigned_8 renames PORTD;
   OLED_Rst_Mask     : constant Unsigned_8 := PORTD7_Mask;

   OLED_DC_PORT      : Unsigned_8 renames PORTD;
   OLED_DC_Mask      : constant Unsigned_8 := PORTD4_Mask;

   Rand_Seed_In_Mask : constant Unsigned_8 := PORTF1_Mask;

   RED_LED_PORT : Unsigned_8 renames PORTB;
   RED_LED_Mask : constant Unsigned_8 := PORTB6_Mask;

   GREEN_LED_PORT : Unsigned_8 renames PORTB;
   GREEN_LED_Mask : constant Unsigned_8 := PORTB7_Mask;

   BLUE_LED_PORT : Unsigned_8 renames PORTB;
   BLUE_LED_Mask : constant Unsigned_8 := PORTB5_Mask;

   TX_LED_PORT : Unsigned_8 renames PORTD;
   TX_LED_Mask : constant Unsigned_8 := PORTD5_Mask;

   RX_LED_PORT : Unsigned_8 renames PORTB;
   RX_LED_Mask : constant Unsigned_8 := PORTB0_Mask;
end Arduboy;
