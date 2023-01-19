package Arduboy.Screen
with Elaborate_Body
is
   subtype Natural_16 is Integer_16 range 0 .. Integer_16'Last;
   type Rect is record
      X, Y : Integer_16;
      Width, Height : Natural_16;
   end record;

   type Point is record
      X, Y : Integer_16;
   end record;

   procedure Clear;
   --  Clear the display buffer.
   --
   --  The entire pixels of the screen buffer are turned off.

   procedure Fill_Screen (On : Boolean := True);
   --  Fill the screen buffer.
   --
   --  The entire pixels of the screen buffer are turned either on or off.

   procedure Display (Clear : Boolean := False);
   --  Copy the contents of the display buffer to the display.
   --
   --  The contents of the display buffer in RAM are copied to the display and
   --  will appear on the screen.
   --
   --  If Clear is  `True` the display buffer will be cleared to zero.

   procedure Invert (Inverse : Boolean := True);
   procedure All_Pixels (On : Boolean := True);
   procedure Flip_Vertical (Flip : Boolean := True);
   procedure Flip_Horizontal (Flip : Boolean := True);

   procedure Draw_Pixel (Pt : Point; On : Boolean := True);
   --  Set a single pixel in the display buffer to the specified color.
   --
   --  \param X The X coordinate of the pixel.
   --  \param Y The Y coordinate of the pixel.
   --  \param On Pixel on or Off (defaults to on).
   --
   --  The single pixel specified location in the display buffer is set to the
   --  specified color.

   function Get_Pixel (Pt : Point) return Boolean;
   --  Returns the state of the given pixel in the screen buffer.
   --
   --  \param X The X coordinate of the pixel.
   --  \param Y The Y coordinate of the pixel.
   --
   --  \return True if the pixel is on or False if the pixel is off.

   procedure Draw_Circle (Center : Point;
                          Radius : Natural_16;
                          On     : Boolean := True);
   --  Draw a circle of a given center and radius

   procedure Fill_Circle (Center : Point;
                          Radius : Natural_16;
                          On     : Boolean := True);
   --  Draw a filled-in circle of a given center and radius

   procedure Draw_Line (From, To : Point; On : Boolean := True);
   --  Draw a line between two specified points

   procedure Draw_Vertical_Line (Top    : Point;
                                 Height : Natural_16;
                                 On     : Boolean := True);
   --  Draw a vertical line

   procedure Draw_Horizontal_Line (Left  : Point;
                                   Width : Natural_16;
                                   On    : Boolean := True);
   --  Draw an horizontal line

   procedure Draw_Rect (R : Rect; On : Boolean := True);
   --  Draw a rectangle of a specified width and height

   procedure Fill_Rect (R : Rect; On : Boolean := True);
   --  Draw a filled-in rectangle of a specified width and height

   procedure Draw_Round_Rect (R      : Rect;
                              Radius : Natural_16;
                              On     : Boolean := True);
   --  Draw a rectangle with rounded corners

   procedure Fill_Round_Rect (R      : Rect;
                              Radius : Natural_16;
                              On     : Boolean := True);
   --  Draw a filled-in rectangle with rounded corners

   procedure Draw_Triangle (P1, P2, P3 : Point; On : Boolean := True);
   --  Draw a triangle given the coordinates of each corner

   type Bitmap is array (Unsigned_16 range <>) of Unsigned_8;

   procedure Draw_Bitmap (Root          : Point;
                          Bmp           : Bitmap;
                          Width, Height : Natural_16;
                          On            : Boolean := True);

   procedure Boot_Logo;
   --   Display the boot logo sequence

end Arduboy.Screen;
