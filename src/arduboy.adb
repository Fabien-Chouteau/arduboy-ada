with Interfaces; use Interfaces;
with Avrada_Rts_Config;
with AVR.MCU; use AVR.MCU;
with AVR.Interrupts;
with AVR.Wait;
with AVR.Sleep;
with AVR.Timer0;
with System.Machine_Code;
with System.Storage_Elements;

package body Arduboy is

   --  Commands sent to the OLED display to initialize it
   OLED_Boot_Program : constant array (Natural range <>) of Unsigned_8 :=
     (
      --  boot defaults are commented out but left here in case they
      --  might prove useful for reference
      --
      --  Further reading: https://www.adafruit.com/datasheets/SSD1306.pdf
      --
      --  Display Off
      --  0xAE,

      --  Set Display Clock Divisor v = 0xF0
      --  default is 0x80
      16#D5#, 16#F0#,

      --  Set Multiplex Ratio v = 16#3F#
      --  16#A8#, 16#3F#,

      --  Set Display Offset v = 0
      --  16#D3#, 16#00#,

      --  Set Start Line (0)
      --  16#40#,

      --  Charge Pump Setting v = enable (16#14#)
      --  default is disabled
      16#8D#, 16#14#,

      --  Set Segment Re-map (A0) | (b0001)
      --  default is (b0000)
      16#A1#,

      --  Set COM Output Scan Direction
      16#C8#,

      --  Set COM Pins v
      --  16#DA#, 16#12#,

      --  Set Contrast v = 16#CF#
      16#81#, 16#CF#,

      --  Set Precharge = 16#F1#
      16#D9#, 16#F1#,

      --  Set VCom Detect
      --  16#DB#, 16#40#,

      --  Entire Display ON
      --  16#A4#,

      --  Set normal/inverse display
      --  16#A6#,

      --  Display On
      16#AF#,

      --  set display mode = horizontal addressing mode (16#00#)
      16#20#, 16#00#

      --  set col address range
      --  16#21#, 16#00#, COLUMN_ADDRESS_END,

      --  set page address range
      --  16#22#, 16#00#, PAGE_ADDRESS_END
     );

   Tick_Ms_Count : Unsigned_32 := 0 with Volatile;

   procedure Busy_Wait_Ms
   is new AVR.Wait.Generic_Busy_Wait_Milliseconds
     (Avrada_Rts_Config.Clock_Frequency);

   --------------
   -- Clock_Ms --
   --------------

   function Clock_Ms return Unsigned_32 is
      Result : Unsigned_32;
   begin
      AVR.Interrupts.Disable_Interrupts;
      Result := Tick_Ms_Count;
      AVR.Interrupts.Enable_Interrupts;
      return Result;
   end Clock_Ms;

   --------------
   -- Delay_Ms --
   --------------

   procedure Delay_Ms (Count : Unsigned_16) is
   begin
        Busy_Wait_Ms (Count);
   end Delay_Ms;

   --------------
   -- Set_Mask --
   --------------

   procedure Set_Mask (Reg : in out Unsigned_8; Mask : Unsigned_8) is
   begin
      Reg := Reg or Mask;
   end Set_Mask;

   ----------------
   -- Clear_Mask --
   ----------------

   procedure Clear_Mask (Reg : in out Unsigned_8; Mask : Unsigned_8) is
   begin
      Reg := Reg and not Mask;
   end Clear_Mask;

   ---------------
   -- Boot_Pins --
   ---------------

   procedure Boot_Pins is
   begin
      --  Port B INPUT_PULLUP or HIGH
      PORTB := PORTB or Red_LED_Mask or Green_LED_Mask or Blue_LED_Mask or
        B_Button_Mask;

      --  Port B INPUT or LOW (none)

      --  Port B inputs
      DDRB := DDRB and not (B_Button_Mask or SPI_MISO_Mask);

      --  Port B outputs
      DDRB := DDRB or (Red_LED_Mask or Green_LED_Mask or Blue_LED_Mask or
                         SPI_MOSI_Mask or SPI_SCK_Mask or SPI_SS_Mask);

      --  Port D INPUT_PULLUP or HIGH
      PORTD := PORTD or OLED_CS_Mask;

      -- Port D INPUT or LOW
      PORTD := PORTD and not OLED_Rst_Mask;

      --  Port D inputs (none)

      --  Port D outputs
      DDRD := DDRD or OLED_Rst_Mask or OLED_CS_Mask or OLED_DC_Mask;

      --  Port E INPUT_PULLUP or HIGH
      PORTE := PORTE or A_Button_Mask;

      --  Port E INPUT or LOW (none)

      --  Port E inputs
      DDRE :=  DDRE and not A_Button_Mask;

      --  Port E outputs (none)

      --  Port F INPUT_PULLUP or HIGH
      PORTF := PORTF or Left_Button_Mask or Right_Button_Mask or
        Up_Button_Mask or Down_Button_Mask;

      --  Port F INPUT or LOW
      PORTF := PORTF and not Rand_Seed_In_Mask;

      --  Port F inputs
      DDRF := DDRF and not (Left_Button_Mask or Right_Button_Mask or
                              Up_Button_Mask or Down_Button_Mask or
                                Rand_Seed_In_Mask);

      --  Port F outputs (none)

   end Boot_Pins;

   --------------
   -- Boot_SPI --
   --------------

   procedure Boot_SPI is
   begin
      SPCR := SPE_Mask or MSTR_Mask;
      SPSR := SPI2X_Mask;
   end Boot_SPI;

   --------------------
   -- OLED_Data_Mode --
   --------------------

   procedure OLED_Data_Mode is
   begin
      Set_Mask (OLED_DC_PORT, OLED_DC_Mask);
   end OLED_Data_Mode;

   -------------------
   -- OLED_Cmd_Mode --
   -------------------

   procedure OLED_Cmd_Mode is
   begin
      Clear_Mask (OLED_DC_PORT, OLED_DC_Mask);
   end OLED_Cmd_Mode;

   ------------------
   -- SPI_Transfer --
   ------------------

   procedure SPI_Transfer (Data : Unsigned_8) is
   begin
      SPDR := Data;

      --  The following NOP introduces a small delay that can prevent the wait
      --  loop from iterating when running at the maximum speed. This gives
      --  about 10% more speed, even if it seems counter-intuitive. At lower
      --  speeds it is unnoticed.

      System.Machine_Code.Asm ("nop",
                               Volatile => True);

      --  wait
      while (SPSR and SPIF_Mask) = 0 loop
         null;
      end loop;
   end SPI_Transfer;

   ---------------
   -- Boot_OLED --
   ---------------

   procedure Boot_OLED is
   begin
      --  reset the display
      Delay_Ms (5); -- reset pin should be low here. let it stay low a while
      Set_Mask (OLED_Rst_Port, OLED_Rst_Mask); -- set high to come out of reset
      Delay_Ms (5); -- wait a while

      --  select the display (permanently, since nothing else is using SPI)
      Clear_Mask (OLED_CS_PORT, OLED_CS_Mask);

      --  run our customized boot-up command sequence against the
      --  OLED to initialize it properly for Arduboy
      OLED_Cmd_Mode;
      for Cmd of OLED_Boot_Program loop
         SPI_Transfer (Cmd);
      end loop;
      OLED_Data_Mode;
   end Boot_OLED;

   -----------------------
   -- OLED_Send_Command --
   -----------------------

   procedure OLED_Send_Command (Cmd : Unsigned_8) is
   begin
      OLED_Cmd_Mode;
      SPI_Transfer (Cmd);
      OLED_Data_Mode;
   end OLED_Send_Command;

   -----------------------
   -- Boot_Power_Saving --
   -----------------------

   procedure Boot_Power_Saving is
   begin
      --  disable Two Wire Interface (I2C) and the ADC
      --  All other bits will be written with 0 so will be enabled
      PRR0 := PRTWI_Mask or PRADC_Mask;

      --  disable USART1
      PRR1 := PRR1 or PRUSART1_Mask;
   end Boot_Power_Saving;

   ------------------
   -- Paint_Screen --
   ------------------

   procedure Paint_Screen (FB : in out Framebuffer; Clear : Boolean := False) is
   begin
      for Elt of FB loop
         SPI_Transfer (Elt);
         if Clear then
            Elt := 0;
         end if;
      end loop;

      --  System.Machine_Code.Asm
      --    (
      --     "   ldi   %A[count], %[len_lsb]               " & ASCII.LF & ASCII.HT & -- for (len = WIDTH * HEIGHT / 8)
      --     "   ldi   %B[count], %[len_msb]               " & ASCII.LF & ASCII.HT &
      --     "1: ld    __tmp_reg__, %a[ptr]      ;2        " & ASCII.LF & ASCII.HT & -- tmp = *(image)
      --     "   out   %[spdr], __tmp_reg__      ;1        " & ASCII.LF & ASCII.HT & -- SPDR = tmp
      --     "   cpse  %[clear], __zero_reg__    ;1/2      " & ASCII.LF & ASCII.HT & -- if (clear) tmp = 0;
      --     "   mov   __tmp_reg__, __zero_reg__ ;1        " & ASCII.LF & ASCII.HT &
      --     "2: sbiw  %A[count], 1              ;2        " & ASCII.LF & ASCII.HT & -- len --
      --     "   sbrc  %A[count], 0              ;1/2      " & ASCII.LF & ASCII.HT & -- loop twice for cheap delay
      --     "   rjmp  2b                        ;2        " & ASCII.LF & ASCII.HT &
      --     "   st    %a[ptr]+, __tmp_reg__     ;2        " & ASCII.LF & ASCII.HT & -- *(image++) = tmp
      --     "   brne  1b                        ;1/2 :18  " & ASCII.LF & ASCII.HT & -- len > 0
      --     "   in    __tmp_reg__, %[spsr]                " & ASCII.LF & ASCII.HT, -- read SPSR to clear SPIF);
      --
      --    )
   end Paint_Screen;

   procedure Tick;
   pragma Machine_Attribute (Entity         => Tick,
                             Attribute_Name => "signal");
   pragma Export (C, Tick, AVR.Timer0.Signal_Compare);

   ----------
   -- Tick --
   ----------

   procedure Tick is
   begin
      Tick_Ms_Count := Tick_Ms_Count + 1;
   end Tick;

   ----------------
   -- Boot_Clock --
   ----------------

   procedure Boot_Clock is
   begin
      pragma Compile_Time_Error
        (Avrada_Rts_Config.Clock_Frequency /= 16_000_000,
         "Invalid clock for timer");

      AVR.Timer0.Init_CTC (AVR.Timer0.Scale_By_64, Overflow => 249);
      AVR.Interrupts.Enable;

   end Boot_Clock;

   ----------
   -- Idle --
   ----------

   procedure Idle is
   begin
      AVR.Sleep.Go_Sleeping;
   end Idle;

   ---------
   -- Put --
   ---------

   procedure Put (C : Character) is
   begin
      while (UCSR1A and UDRE1_Mask) = 0 loop
         null;
      end loop;
      UDR1 := C'Enum_Rep;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (S : String) is
   begin
      for C of S loop
         Put (C);
      end loop;
   end Put;

   ---------
   -- LCH --
   ---------

   procedure LCH (Msg : System.Address; Line : Integer) is
      use System.Storage_Elements;

      Ptr : Integer_Address := To_Integer (Msg);

      Line_Str : String (1 .. 10) := (others => '0');
      L : Integer := Line;
      Index : Integer := Line_Str'Last;
   begin
      Put ("Exception in ");
      loop
         declare
            C : constant Character
              with Import, Address => To_Address (Ptr);
         begin
            exit when C = ASCII.NUL;
            Put (C);
            Ptr := Ptr + 1;
         end;
      end loop;
      Put (":");

      while L > 0 loop
         Line_Str (Index) :=
           Character'Enum_Val (Character'Enum_Rep ('0') + L mod 10);
         L := L / 10;
         Index := Index - 1;
      end loop;
      Put (Line_Str);
      Put (ASCII.LF);

      loop
         null;
      end loop;
   end LCH;

begin
   Boot_Pins;
   Boot_SPI;
   Boot_OLED;
   Boot_Clock;
   Boot_Power_Saving;
end Arduboy;
