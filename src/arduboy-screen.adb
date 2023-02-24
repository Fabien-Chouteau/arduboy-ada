with AVR.Programspace;

package body Arduboy.Screen is

   Arduboy_Logo : constant Bitmap :=
     (16#F0#, 16#F8#, 16#9C#, 16#8E#, 16#87#, 16#83#, 16#87#, 16#8E#, 16#9C#,
      16#F8#, 16#F0#, 16#00#, 16#00#, 16#FE#, 16#FF#, 16#03#, 16#03#, 16#03#,
      16#03#, 16#03#, 16#07#, 16#0E#, 16#FC#, 16#F8#, 16#00#, 16#00#, 16#FE#,
      16#FF#, 16#03#, 16#03#, 16#03#, 16#03#, 16#03#, 16#07#, 16#0E#, 16#FC#,
      16#F8#, 16#00#, 16#00#, 16#FF#, 16#FF#, 16#00#, 16#00#, 16#00#, 16#00#,
      16#00#, 16#00#, 16#00#, 16#FF#, 16#FF#, 16#00#, 16#00#, 16#FE#, 16#FF#,
      16#83#, 16#83#, 16#83#, 16#83#, 16#83#, 16#C7#, 16#EE#, 16#7C#, 16#38#,
      16#00#, 16#00#, 16#F8#, 16#FC#, 16#0E#, 16#07#, 16#03#, 16#03#, 16#03#,
      16#07#, 16#0E#, 16#FC#, 16#F8#, 16#00#, 16#00#, 16#3F#, 16#7F#, 16#E0#,
      16#C0#, 16#80#, 16#80#, 16#C0#, 16#E0#, 16#7F#, 16#3F#, 16#FF#, 16#FF#,
      16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#01#, 16#FF#, 16#FF#,
      16#00#, 16#00#, 16#FF#, 16#FF#, 16#0C#, 16#0C#, 16#0C#, 16#0C#, 16#1C#,
      16#3E#, 16#77#, 16#E3#, 16#C1#, 16#00#, 16#00#, 16#7F#, 16#FF#, 16#C0#,
      16#C0#, 16#C0#, 16#C0#, 16#C0#, 16#E0#, 16#70#, 16#3F#, 16#1F#, 16#00#,
      16#00#, 16#1F#, 16#3F#, 16#70#, 16#E0#, 16#C0#, 16#C0#, 16#C0#, 16#E0#,
      16#70#, 16#3F#, 16#1F#, 16#00#, 16#00#, 16#7F#, 16#FF#, 16#C1#, 16#C1#,
      16#C1#, 16#C1#, 16#C1#, 16#E3#, 16#77#, 16#3E#, 16#1C#, 16#00#, 16#00#,
      16#1F#, 16#3F#, 16#70#, 16#E0#, 16#C0#, 16#C0#, 16#C0#, 16#E0#, 16#70#,
      16#3F#, 16#1F#, 16#00#, 16#00#, 16#00#, 16#00#, 16#00#, 16#01#, 16#FF#,
      16#FF#, 16#01#, 16#00#, 16#00#, 16#00#);
   pragma Linker_Section (Arduboy_Logo, ".progmem.data");

   FB : Framebuffer := (others => 0);

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Fill_Screen (On => False);
   end Clear;

   -----------------
   -- Fill_Screen --
   -----------------

   procedure Fill_Screen (On : Boolean := True) is
   begin
      if On then
         FB := (others => 16#FF#);
      else
         FB := (others => 16#00#);
      end if;
   end Fill_Screen;

   -------------
   -- Display --
   -------------

   procedure Display (Clear : Boolean := False) is
   begin
      Paint_Screen (FB, Clear);
   end Display;

   ------------
   -- Invert --
   ------------

   procedure Invert (Inverse : Boolean := True) is
      OLED_PIXELS_INVERTED : constant := 16#A7#; -- All pixels inverted
      OLED_PIXELS_NORMAL : constant := 16#A6#; -- All pixels normal
   begin
      OLED_Send_Command (if Inverse
                         then OLED_PIXELS_INVERTED
                         else OLED_PIXELS_NORMAL);
   end Invert;

   ----------------
   -- All_Pixels --
   ----------------

   procedure All_Pixels (On : Boolean := True) is
      OLED_ALL_PIXELS_ON : constant := 16#A5#;
      OLED_PIXELS_FROM_RAM : constant := 16#A4#;
   begin
      OLED_Send_Command (if On
                         then OLED_ALL_PIXELS_ON
                         else OLED_PIXELS_FROM_RAM);
   end All_Pixels;

   -------------------
   -- Flip_Vertical --
   -------------------

   procedure Flip_Vertical (Flip : Boolean := True) is
      OLED_VERTICAL_FLIPPED : constant := 16#C0#;
      OLED_VERTICAL_NORMAL : constant := 16#C0#;
   begin
      OLED_Send_Command (if Flip
                         then OLED_VERTICAL_FLIPPED
                         else OLED_VERTICAL_NORMAL);
   end Flip_Vertical;

   ---------------------
   -- Flip_Horizontal --
   ---------------------

   procedure Flip_Horizontal (Flip : Boolean := True) is
      OLED_HORIZ_FLIPPED : constant := 16#A0#;
      OLED_HORIZ_NORMAL : constant := 16#A1#;
   begin
      OLED_Send_Command (if Flip
                         then OLED_HORIZ_FLIPPED
                         else OLED_HORIZ_NORMAL);
   end Flip_Horizontal;

   ----------------
   -- Draw_Pixel --
   ----------------

   procedure Draw_Pixel (Pt : Point; On : Boolean := True) is
      Row_Offset : Unsigned_16;
      Bit : Unsigned_8;
      Data : Unsigned_8;
   begin
      if Pt.X < 0 or else Pt.X > Screen_Width - 1
        or else Pt.Y < 0 or else Pt.Y > Screen_Height - 1
      then
         return;
      end if;

      Bit := Shift_Left (1, Natural (Unsigned_8 (Pt.Y) and 7));
      Row_Offset := (Unsigned_16 (Pt.Y) and 16#F8#) * (Screen_Width / 8) +
        Unsigned_16 (Pt.X);

      Data := FB (Row_Offset);

      if On then
         Data := Data or Bit;
      else
         Data := Data and not Bit;
      end if;

      FB (Row_Offset) := Data;
   end Draw_Pixel;

   ---------------
   -- Get_Pixel --
   ---------------

   function Get_Pixel (Pt : Point) return Boolean is
      Row : constant Unsigned_16 := Unsigned_16 (Pt.Y) / 8;
      Bit_Position : constant Unsigned_8 := Unsigned_8 (Pt.X mod 8);
   begin
      return (FB ((Row * Screen_Width) + Unsigned_16 (Pt.X))
              and Bit_Position) /= 0;
   end Get_Pixel;

   -----------------
   -- Draw_Circle --
   -----------------

   procedure Draw_Circle (Center : Point;
                          Radius : Natural_16;
                          On     : Boolean := True)
   is
      F     : Integer_16 := 1 - Radius;
      ddF_X : Integer_16 := 1;
      ddF_Y : Integer_16 := (-2) * Radius;
      X     : Integer_16 := 0;
      Y     : Integer_16 := Radius;
   begin
      Draw_Pixel ((Center.X, Center.Y + Radius), On);
      Draw_Pixel ((Center.X, Center.Y - Radius), On);
      Draw_Pixel ((Center.X + Radius, Center.Y), On);
      Draw_Pixel ((Center.X - Radius, Center.Y), On);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         Draw_Pixel ((Center.X + X, Center.Y + Y), On);
         Draw_Pixel ((Center.X - X, Center.Y + Y), On);
         Draw_Pixel ((Center.X + X, Center.Y - Y), On);
         Draw_Pixel ((Center.X - X, Center.Y - Y), On);
         Draw_Pixel ((Center.X + Y, Center.Y + X), On);
         Draw_Pixel ((Center.X - Y, Center.Y + X), On);
         Draw_Pixel ((Center.X + Y, Center.Y - X), On);
         Draw_Pixel ((Center.X - Y, Center.Y - X), On);
      end loop;
   end Draw_Circle;

   -----------------
   -- Fill_Circle --
   -----------------

   procedure Fill_Circle (Center : Point;
                          Radius : Natural_16;
                          On     : Boolean := True)
   is
      F     : Integer_16 := 1 - Integer_16 (Radius);
      ddF_X : Integer_16 := 1;
      ddF_Y : Integer_16 := (-2) * Integer_16 (Radius);
      X     : Integer_16 := 0;
      Y     : Integer_16 := Integer_16 (Radius);
   begin
      Draw_Vertical_Line ((Center.X, Center.Y - Radius), 2 * Radius, On);
      Draw_Horizontal_Line ((Center.X - Radius, Center.Y), 2 * Radius, On);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;

         Draw_Horizontal_Line ((Center.X - X, Center.Y + Y), 2 * X, On);
         Draw_Horizontal_Line ((Center.X - X, Center.Y - Y), 2 * X, On);
         Draw_Horizontal_Line ((Center.X - Y, Center.Y + X), 2 * Y, On);
         Draw_Horizontal_Line ((Center.X - Y, Center.Y - X), 2 * Y, On);
      end loop;
   end Fill_Circle;

   ---------------
   -- Draw_Line --
   ---------------

   procedure Draw_Line (From, To : Point; On : Boolean := True)
   is

      Steep : constant Boolean := abs (To.Y - From.Y) > abs (To.X - From.X);

      F : Point := From;
      T : Point := To;

      DX, DY, Err, Y_Step : Integer_16;

      procedure Swap (A, B : in out Integer_16) is
         Tmp : constant Integer_16 := A;
      begin
         A := B;
         B := Tmp;
      end Swap;
   begin
      if Steep then
         Swap (F.X, F.Y);
         Swap (T.X, T.Y);
      end if;

      if F.X > T.X then
         Swap (F.X, T.X);
         Swap (F.Y, T.Y);
      end if;

      DX := T.X - F.X;
      DY := abs (T.Y - F.Y);
      Err := DX / 2;

      if F.Y < T.Y then
         Y_Step := 1;
      else
         Y_Step := -1;
      end if;

      while F.X <= T.X loop
         if Steep then
            Draw_Pixel ((F.Y, F.X), On);
         else
            Draw_Pixel ((F.X, F.Y), On);
         end if;

         Err := Err - DY;
         if Err < 0 then
            F.Y := F.Y + Y_Step;
            Err := Err + DX;
         end if;

         F.X := F.X + 1;
      end loop;
   end Draw_Line;

   ------------------------
   -- Draw_Vertical_Line --
   ------------------------

   procedure Draw_Vertical_Line (Top    : Point;
                                 Height : Natural_16;
                                 On     : Boolean := True)
   is
      Yend : constant Integer_16 := Top.Y + Height;
   begin
      for Y in
        Integer_16'Max (0, Top.Y) .. Integer_16'Min (Yend, Screen_Height)
      loop
         Draw_Pixel ((Top.X, Y), On);
      end loop;
   end Draw_Vertical_Line;

   --------------------------
   -- Draw_Horizontal_Line --
   --------------------------

   procedure Draw_Horizontal_Line (Left  : Point;
                                   Width : Natural_16;
                                   On    : Boolean := True)
   is
      Xend : Integer_16;
      Xstart : Integer_16;
      W : Integer_16;

      Ptr : Unsigned_16;
      Mask : Unsigned_8;
   begin
      --  Do y bounds checks
      if Left.Y < 0 or else Left.Y >= Screen_Height then
         return;
      end if;

      Xend := Left.X + Width;

      --  Check if the entire line is not on the display
      if Xend <= 0 or else Left.X >= Screen_Width then
         return;
      end if;

      --  Don't start before the left edge
      if Left.X < 0 then
         Xstart := 0;
      else
         Xstart := Left.X;
      end if;

      --  Don't end past the right edge
      if Xend > Screen_Width then
         Xend := Screen_Width;
      end if;

      --  calculate actual width (even if unchanged)
      W := Xend - Xstart;

      --  Buffer pointer plus row offset + x offset
      Ptr := Unsigned_16 (((Left.Y / 8) * Screen_Width) + Xstart);

      --  pixel mask
      Mask := Shift_Left (1, Natural (Left.Y) mod 8);

      case On is
         when True =>
            while W /= 0 loop
               FB (Ptr) := FB (Ptr) or Mask;
               Ptr := Ptr + 1;
               W := W - 1;
            end loop;
         when False =>
            while W /= 0 loop
               FB (Ptr) := FB (Ptr) and (not Mask);
               Ptr := Ptr + 1;
               W := W - 1;
            end loop;
      end case;
   end Draw_Horizontal_Line;

   ---------------
   -- Draw_Rect --
   ---------------

   procedure Draw_Rect (R : Rect; On : Boolean := True)
   is
   begin
      Draw_Horizontal_Line ((R.X, R.Y), R.Width, On);
      Draw_Horizontal_Line ((R.X, R.Y + R.Height - 1), R.Width, On);
      Draw_Vertical_Line ((R.X, R.Y), R.Width, On);
      Draw_Vertical_Line ((R.X + R.Width - 1, R.Y), R.Width, On);
   end Draw_Rect;

   ---------------
   -- Fill_Rect --
   ---------------

   procedure Fill_Rect (R : Rect; On : Boolean := True)
   is
   begin
      for Y in R.Y .. R.Y + R.Height - 1 loop
         Draw_Horizontal_Line ((R.X, Y), R.Width, On);
      end loop;
   end Fill_Rect;

   ---------------------
   -- Draw_Round_Rect --
   ---------------------

   procedure Draw_Round_Rect (R      : Rect;
                              Radius : Natural_16;
                              On     : Boolean := True)
   is
      F     : Integer_16 := 1 - Radius;
      ddF_X : Integer_16 := 1;
      ddF_Y : Integer_16 := (-2) * Radius;
      X     : Integer_16 := 0;
      Y     : Integer_16 := Radius;
      Top : constant Integer_16 := R.Y + Radius;
      Bot : constant Integer_16 := R.Y + R.Height - 1 - Radius;
      Left : constant Integer_16 := R.X + Radius;
      Right : constant Integer_16 := R.X + R.Width - 1 - Radius;
   begin
      if Radius = 0 then
         Draw_Rect (R, On);
      end if;

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;
         Draw_Pixel ((Right + X, Bot + Y), On);
         Draw_Pixel ((Left - X, Bot + Y), On);
         Draw_Pixel ((Right + X, Top - Y), On);
         Draw_Pixel ((Left - X, Top - Y), On);
         Draw_Pixel ((Right + Y, Bot + X), On);
         Draw_Pixel ((Left - Y, Bot + X), On);
         Draw_Pixel ((Right + Y, Top - X), On);
         Draw_Pixel ((Left - Y, Top - X), On);
      end loop;

      Draw_Horizontal_Line ((R.X + Radius, R.Y),
                            R.Width - 2 * Radius, On);

      Draw_Horizontal_Line ((R.X + Radius, R.Y + R.Height - 1),
                            R.Width - 2 * Radius, On);

      Draw_Vertical_Line ((R.X, R.Y + Radius),
                          R.Width - 2 * Radius, On);

      Draw_Vertical_Line ((R.X + R.Width - 1, R.Y + Radius),
                          R.Width - 2 * Radius, On);
   end Draw_Round_Rect;

   ---------------------
   -- Fill_Round_Rect --
   ---------------------

   procedure Fill_Round_Rect (R      : Rect;
                              Radius : Natural_16;
                              On     : Boolean := True)
     is
      F     : Integer_16 := 1 - Radius;
      ddF_X : Integer_16 := 1;
      ddF_Y : Integer_16 := (-2) * Radius;
      X     : Integer_16 := 0;
      Y     : Integer_16 := Radius;
      Top : constant Integer_16 := R.Y + Radius;
      Bot : constant Integer_16 := R.Y + R.Height - 1 - Radius;
      Left : constant Integer_16 := R.X + Radius;
   begin
      if Radius = 0 then
         Fill_Rect (R, On);
      end if;

      Fill_Rect ((R.X, Top, R.Width, R.Height - 2 * Radius), On);

      while X < Y loop
         if F >= 0 then
            Y := Y - 1;
            ddF_Y := ddF_Y + 2;
            F := F + ddF_Y;
         end if;
         X := X + 1;
         ddF_X := ddF_X + 2;
         F := F + ddF_X + 1;

         Draw_Horizontal_Line ((Left - X, Bot + Y),
                               R.Width - 2 * Radius + 2 * X);
         Draw_Horizontal_Line ((Left - X, Top - Y),
                               R.Width - 2 * Radius + 2 * X);
         Draw_Horizontal_Line ((Left - Y, Bot + X),
                               R.Width - 2 * Radius + 2 * Y);
         Draw_Horizontal_Line ((Left - Y, Top - X),
                               R.Width - 2 * Radius + 2 * Y);
      end loop;

   end Fill_Round_Rect;

   -------------------
   -- Draw_Triangle --
   -------------------

   procedure Draw_Triangle (P1, P2, P3 : Point; On : Boolean := True)
   is
   begin
      Draw_Line (P1, P2, On);
      Draw_Line (P2, P3, On);
      Draw_Line (P3, P1, On);
   end Draw_Triangle;

   -----------------
   -- Draw_Bitmap --
   -----------------

   procedure Draw_Bitmap (Root          : Point;
                          Bmp           : Bitmap;
                          Width, Height : Natural_16;
                          On            : Boolean := True)
   is
      use AVR.Programspace;

      Yoffset : Integer_16;
      Screen_Row, Buffer_Row : Integer_16;
      Rows : Integer_16;

      procedure Put (V : Unsigned_16) is
         Line_Str : String (1 .. 10) := (others => '0');
         L : Unsigned_16 := V;
         Index : Integer := Line_Str'Last;
      begin
         while L > 0 loop
            Line_Str (Index) :=
              Character'Enum_Val (Character'Enum_Rep ('0') + L mod 10);
            L := L / 10;
            Index := Index - 1;
         end loop;
         Put (Line_Str);
      end Put;
   begin
      if Root.X + Width <= 0
        or else
         Root.X > Screen_Width - 1
        or else
         Root.Y + Height <= 0
        or else
         Root.Y > Screen_Height - 1
      then
         return;
      end if;

      Yoffset := (abs Root.Y) mod 8;
      Screen_Row := Root.Y / 8;

      if Root.Y < 0 then
         Screen_Row := Screen_Row - 1;
         Yoffset := 8 - Yoffset;
      end if;

      Rows := Height / 8;
      if Height mod 8 /= 0 then
         Rows := Rows + 1;
      end if;

      for A in Integer_16 range 0 .. Rows - 1 loop
         Buffer_Row := Screen_Row + A;

         exit when Buffer_Row > (Screen_Height / 8) - 1;

         if Buffer_Row > -2 then

            for Icol in Integer_16 range 0 .. Width - 1 loop

               exit when Icol + Root.X > Screen_Width - 1;

               if Icol + Root.X >= 0 then
                  if Buffer_Row >= 0 then
                     declare
                        FB_Index : constant Unsigned_16 :=
                          Unsigned_16 (Buffer_Row * Screen_Width + Root.X + Icol);

                        Bmp_Index : constant Unsigned_16 :=
                          Unsigned_16 (A * Width + Icol);

                        Data : constant Unsigned_8 :=
                          Shift_Left
                            (Get_Byte (Bmp (Bmp'First + Bmp_Index)'Address),
                             Natural (Yoffset));
                     begin
                        if On then
                           FB (FB_Index) := FB (FB_Index) or Data;
                        else
                           FB (FB_Index) := FB (FB_Index) and not Data;
                        end if;

                     end;
                  end if;

                  if Yoffset /= 0
                    and then
                      Buffer_Row < ((Screen_Height / 8) - 1)
                    and then
                      Buffer_Row > -2
                  then
                     declare
                        FB_Index : constant Unsigned_16 :=
                          Unsigned_16 ((Buffer_Row + 1) * Screen_Width + Root.X + Icol);

                        Bmp_Index : constant Unsigned_16 :=
                          Unsigned_16 (A * Width + Icol);

                        Data : constant Unsigned_8 :=
                          Shift_Right
                            (Get_Byte (Bmp (Bmp_Index)'Address),
                             Natural (8 - Yoffset));
                     begin
                        if On then
                           FB (FB_Index) := FB (FB_Index) or Data;
                        else
                           FB (FB_Index) := FB (FB_Index) and not Data;
                        end if;
                     end;
                  end if;
               end if;
            end loop;
         end if;
      end loop;
   end Draw_Bitmap;

   ---------------
   -- Boot_Logo --
   ---------------

   procedure Boot_Logo is
   begin
      for Y in Integer_16 range -15 .. 24 loop
         Draw_Bitmap ((20, Y), Arduboy_Logo, 88, 16);
         Display (Clear => True);
         Delay_Ms (30);
      end loop;
   end Boot_Logo;

end Arduboy.Screen;
