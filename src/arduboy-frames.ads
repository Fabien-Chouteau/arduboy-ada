package Arduboy.Frames
with Elaborate_Body
is

   procedure Set_Frame_Rate (Rate : Unsigned_8);
   --  Set the frame rate used by the frame control functions.
   --
   --  Rate: The desired frame rate in frames per second.
   --
   --  Set the frame rate, in frames per second, used by `Next_Frame ()` to
   --  update frames at a given rate. If this function or `Set_Frame_Duration
   --  ()` isn't used, the default rate will be 60 (actually 62.5; see note
   --  below).
   --
   --  Normally, the frame rate would be set to the desired value once, at the
   --  start of the game, but it can be changed at any time to alter the frame
   --  update rate.
   --
   --  The given rate is internally converted to a frame duration in
   --  milliseconds, rounded down to the nearest integer. Therefore, the actual
   --  rate will be equal to or higher than the rate given. For example, 60 FPS
   --  would be 16.67ms per frame. This will be rounded down to 16ms, giving an
   --  actual frame rate of 62.5 FPS.

   procedure Set_Frame_Duration (Duration : Unsigned_8);
   --  Set the frame rate, used by the frame control functions, by giving the
   --  duration of each frame.
   --
   --  Duration: The desired duration of each frame in milliseconds.
   --
   --  Set the frame rate by specifying the duration of each frame in
   --  milliseconds. This is used by `Next_Frame ()` to update frames at a
   --  given rate. If this function or `Set_Frame_Rate ()` isn't used, the
   --  default will be 16ms per frame.
   --
   --  Normally, the frame rate would be set to the desired value once, at the
   --  start of the game, but it can be changed at any time to alter the frame
   --  update rate.

   function Next_Frame (Show_Overload : Boolean := False) return Boolean;
   --  Indicate that it's time to render the next frame.
   --
   --  Return `True` if it's time for the next frame.
   --
   --  When Show_Overload is true, the yellow TX LED (at the bottom, to the
   --  left of the USB connector) will be turned on if a frame takes longer to
   --  generate than the time allowed. This can be useful during developement.
   --
   --  When this function returns `True`, the amount of time has elapsed
   --  to display the next frame, as specified by `Set_Frame_Rate ()` or
   --  `Set_Frame_Duration ()`.
   --
   --  This function will normally be called at the start of the rendering
   --  loop which would wait for `True` to be returned before rendering and
   --  displaying the next frame.
   --
   --   example:
   --    Set_Frame_Rate (60);
   --    loop
   --       if Next_Frame then
   --          --  Render and display the next frame
   --       end if;
   --    end loop;

   function Every_X_Frames (Frames : Unsigned_8) return Boolean;
   --  Indicate if the specified number of frames has elapsed.
   --
   --  Frames: The desired number of elapsed frames.
   --
   --  Return `True` if the specified number of frames has elapsed.
   --
   --  This function should be called with the same value each time for a given
   --  event. It will return `True` if the given number of frames has elapsed
   --  since the previous frame in which it returned `True`.
   --
   --  For example, if you wanted to fire a shot every 5 frames while the A
   --  button is being held down:
   --   if Every_X_Frames(5) and then Pressed (A) then
   --       Fire_Shot ();
   --   end if;

   function CPU_Load return Natural;
   --  Return the load on the CPU as a percentage.
   --
   --  Return The load on the CPU as a percentage of the total frame time.
   --
   --  The returned value gives the time spent processing a frame as a
   --  percentage the total time allotted for a frame, as determined by
   --  the frame rate.
   --
   --  This function normally wouldn't be used in the final program. It is
   --  intended for use during program development as an aid in helping with
   --  frame timing.
   --
   --  The percentage returned can be higher than 100 if more time is spent
   --  processing a frame than the time allotted per frame. This would indicate
   --  that the frame rate should be made slower or the frame processing code
   --  should be optimized to run faster.

end Arduboy.Frames;
