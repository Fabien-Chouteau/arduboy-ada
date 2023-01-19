with Interfaces; use Interfaces;

with Arduboy;
with Arduboy.Screen;  use Arduboy.Screen;
with Arduboy.Frames;  use Arduboy.Frames;
with Arduboy.Buttons; use Arduboy.Buttons;
with Arduboy.LEDs;    use Arduboy.LEDs;

procedure Example is
   Pt : Point := (Arduboy.Screen_Width / 2, Arduboy.Screen_Height / 2);
begin

   Set_Frame_Rate (60);

   Boot_Logo;

   loop
      if Next_Frame then
         Poll_Buttons;

         if Every_X_Frames (5) then
            if Pressed (Up) then
               Pt.Y := Pt.Y - 1;
            end if;
            if Pressed (Down) then
               Pt.Y := Pt.Y + 1;
            end if;

            if Pressed (Left) then
               Pt.X := Pt.X - 1;
            end if;
            if Pressed (Right) then
               Pt.X := Pt.X + 1;
            end if;
            Draw_Pixel (Pt);
         end if;

         if Just_Pressed (A) then
            for Id in LED_Id loop
               On (Id);
            end loop;
            Invert;
         end if;

         if Just_Pressed (B) then
            for Id in LED_Id loop
               Off (Id);
            end loop;
            Invert (False);
         end if;

         Display (Clear => False);
      end if;
   end loop;
end Example;
