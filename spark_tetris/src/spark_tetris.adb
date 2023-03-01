with Interfaces;        use Interfaces;
with Tetris_Functional; use Tetris_Functional;

with Arduboy.Screen;  use Arduboy.Screen;
with Arduboy.Buttons; use Arduboy.Buttons;
with Arduboy.Frames;  use Arduboy.Frames;
with Arduboy.Text;

procedure Spark_Tetris is

   Zoom : constant := 3;
   --  Number of pixel to represent one block in the game

   function Fall_Period (Speed_Up : Boolean) return Unsigned_32;
   --  Time to wait between each game step

   Level_Nbr    : Unsigned_32 := 0;
   Score        : Unsigned_32 := 0;

   Line_Counter : Unsigned_32 := 0;
   --  Used to compute the level

   Nbr_Of_Complete_Lines : Unsigned_32;

   procedure Draw_Board (With_Piece : Boolean);
   --  Draw on the OLED screen the board and the falling piece

   --  Simple random generator.
   Rnd : Unsigned_32 := 137;

   procedure Random_Piece (Nbr : in out Unsigned_32; P : out Piece)
     with Post => Piece_Within_Board (P);
   --  Generate a new random piece

   Next_Piece : Piece;
   --  Next piece to be inserted in the game

   -----------------
   -- Fall_Period --
   -----------------

   function Fall_Period (Speed_Up : Boolean) return Unsigned_32 is
      Interval : constant Unsigned_32 :=
        ((11 - Level_Nbr mod 11) * 30);
   begin
      return (if Speed_Up then Interval / 3 else Interval);
   end Fall_Period;

   ----------------
   -- Draw_Board --
   ----------------

   procedure Draw_Board (With_Piece : Boolean) is
      X_Start : constant := 5;
      Y_Start : constant := 3;

      procedure Draw_Block (X : Integer_16; Y : Integer_16);
      procedure Draw_Piece (P        : Piece;
                            Offset_X : Integer_16 := 0;
                            Offset_Y : Integer_16 := 0);

      ----------------
      -- Draw_Block --
      ----------------

      procedure Draw_Block (X : Integer_16; Y : Integer_16) is
         X_Pos : constant Integer_16 := Zoom * (X - X_Coord'First);
         Y_Pos : constant Integer_16 := Zoom * (Y - Y_Coord'First);
      begin
         for I in Integer_16 range 1 .. Zoom loop
            for J in Integer_16 range 1 .. Zoom loop
               Arduboy.Screen.Draw_Pixel ((X_Pos + I - 1 + X_Start,
                                           Y_Pos + J - 1 + Y_Start));
            end loop;
         end loop;
      end Draw_Block;

      ----------------
      -- Draw_Piece --
      ----------------

      procedure Draw_Piece (P        : Piece;
                            Offset_X : Integer_16 := 0;
                            Offset_Y : Integer_16 := 0)
      is
      begin
         case P.S is
            when O =>
               Draw_Block (P.X + Offset_X, P.Y + Offset_Y);
               Draw_Block (P.X + Offset_X, P.Y + 1 + Offset_Y);
               Draw_Block (P.X + 1 + Offset_X, P.Y + Offset_Y);
               Draw_Block (P.X + 1 + Offset_X, P.Y + 1 + Offset_Y);

            when I =>
               for Y in I_Delta loop
                  for X in I_Delta loop
                     if Possible_I_Shapes (P.D) (Y, X) then
                        Draw_Block (P.X + X + Offset_X, P.Y + Y + Offset_Y);
                     end if;
                  end loop;
               end loop;

            when Three_Shape =>
               for Y in Three_Delta loop
                  for X in Three_Delta loop
                     if Possible_Three_Shapes
                       (P.S, P.D) (Y, X)
                     then
                        Draw_Block (P.X + X + Offset_X, P.Y + Y + Offset_Y);
                     end if;
                  end loop;
               end loop;
         end case;
      end Draw_Piece;
   begin

      Arduboy.Text.Set_Cursor (63, 35);
      Arduboy.Text.Put ("SPARK");
      Arduboy.Text.Set_Cursor (60, 45);
      Arduboy.Text.Put ("TETRIS");
      Arduboy.Text.Set_Cursor (66, 55);
      Arduboy.Text.Put ("Demo");

      Arduboy.Text.Set_Cursor (53, 0);
      Arduboy.Text.Put ("Score:");
      Arduboy.Text.Set_Cursor (53, 8);

      Arduboy.Text.Put (Integer_32 (Score));
      Arduboy.Text.Set_Cursor (53, 16);
      Arduboy.Text.Put ("Level:");
      Arduboy.Text.Set_Cursor (53, 24);
      Arduboy.Text.Put (Integer_32 (Level_Nbr + 1));

      --  Vertical boarders
      Arduboy.Screen.Draw_Vertical_Line ((X_Start - 1,
                                          Y_Start),
                                         Y_Size * Zoom);
      Arduboy.Screen.Draw_Vertical_Line ((X_Start + X_Size * Zoom,
                                          Y_Start),
                                         Y_Size * Zoom);

      --  Bottom boarder
      Arduboy.Screen.Draw_Horizontal_Line ((X_Start, Y_Start + Y_Size * Zoom),
                                            X_Size * Zoom);

      --  The board
      for Y in Y_Coord loop
         for X in X_Coord loop
            if Cur_Board (Y)(X) /= Empty then
               Draw_Block (X, Y);
            end if;
         end loop;
      end loop;

      --  The current piece
      if With_Piece then
         Draw_Piece (Cur_Piece);
      end if;

      --  Preview of the next piece displayed to the right of the board
      Draw_Piece (Next_Piece, Offset_X => 7, Offset_Y => 1);
   end Draw_Board;

   ------------------
   -- Random_Piece --
   ------------------

   procedure Random_Piece (Nbr : in out Unsigned_32; P : out Piece) is
   begin
      Nbr := Nbr * 1103515245 + 12345;
      P := (S => Cell'Val (1 + ((Nbr / 65536) mod 7)),
            D => North,
            X => X_Size / 2,
            Y => Y_Coord'First);
   end Random_Piece;

   ----------------
   -- Reset_Game --
   ----------------

   procedure Reset_Game is
   begin
      Cur_Board := [others => [others => Empty]];
      Score := 0;
      Level_Nbr := 0;
      Line_Counter := 0;
      Rnd := Arduboy.Clock_Ms;
   end Reset_Game;

   type Game_State is (Pre_Game, New_Piece, Piece_Fall, Game_Over);

   State : Game_State := Game_State'First;
   Success : Boolean;

   Rotation_Count : Natural;
   Next_Fall : Unsigned_32;
begin

   Set_Frame_Rate (60);

   Boot_Logo;

   Reset_Game;
   Random_Piece (Rnd, Next_Piece);

   --  Game loop
   loop

      if Next_Frame then
         Poll_Buttons;

         case State is
            when Pre_Game =>
               Draw_Board (False);
               Arduboy.Text.Set_Cursor (4, 6);
               Arduboy.Text.Put ("PRESS");
               Arduboy.Text.Set_Cursor (17, 21);
               Arduboy.Text.Put ("A");
               Arduboy.Text.Set_Cursor (14, 36);
               Arduboy.Text.Put ("TO");
               Arduboy.Text.Set_Cursor (4, 51);
               Arduboy.Text.Put ("START");

               if Just_Pressed (A) then
                  State := Game_State'Succ (State);
                  Reset_Game;
               end if;

            when New_Piece =>
               --  Add a new piece
               Cur_Piece  := Next_Piece;
               Random_Piece (Rnd, Next_Piece);

               Cur_State := Piece_Falling;
               Rotation_Count := 0;
               Next_Fall := Arduboy.Clock_Ms;

               --  Stop the game when the piece cannot appear
               if not Valid_Configuration then
                  State := Game_Over;
               else
                  State := Piece_Fall;
               end if;

               Draw_Board (True);

            when Piece_Fall =>

               if Rotation_Count < 2 then
                  if Just_Pressed (Arduboy.Buttons.A) then
                     Do_Action (Turn_Counter_Clockwise, Success);
                     Rotation_Count := Rotation_Count + 1;
                  elsif Just_Pressed (Arduboy.Buttons.B) then
                     Do_Action (Turn_Clockwise, Success);
                     Rotation_Count := Rotation_Count + 1;
                  end if;
               end if;

               if Just_Pressed (Arduboy.Buttons.Left) then
                  Do_Action (Move_Left, Success);
               elsif Just_Pressed (Arduboy.Buttons.Right) then
                  Do_Action (Move_Right, Success);
               end if;

               if Arduboy.Clock_Ms >= Next_Fall then

                  Next_Fall := Next_Fall + Fall_Period
                    (Speed_Up => Pressed (Down));

                  Rotation_Count := 0;

                  --  Fall and continue unless the piece hits the ground
                  Do_Action (Move_Down, Success);

                  if not Success then
                     --  Done with that piece
                     Cur_State := Piece_Blocked;

                     Include_Piece_In_Board;
                     Delete_Complete_Lines (Nbr_Of_Complete_Lines);

                     case Nbr_Of_Complete_Lines is
                     when 1 => Score := Score + 4 * (Level_Nbr + 1);
                     when 2 => Score := Score + 10 * (Level_Nbr + 1);
                     when 3 => Score := Score + 30 * (Level_Nbr + 1);
                     when 4 => Score := Score + 120 * (Level_Nbr + 1);
                     when others => null;
                     end case;

                     Line_Counter := Line_Counter + Nbr_Of_Complete_Lines;

                     if Line_Counter >= 10 and then Level_Nbr < 10 then
                        Level_Nbr    := Level_Nbr + 1;
                        Line_Counter := Line_Counter - 10;
                     end if;

                     State := New_Piece;
                  end if;
               end if;

               Draw_Board (True);

            when Game_Over =>
               --  Lost!
               Draw_Board (False);
               Arduboy.Text.Set_Cursor (7, 20);
               Arduboy.Text.Put ("GAME");
               Arduboy.Text.Set_Cursor (7, 35);
               Arduboy.Text.Put ("OVER");

               if Just_Pressed (B) then
                  State := Pre_Game;
               end if;

         end case;
         Display (Clear => True);
      end if;
   end loop;
end Spark_Tetris;
