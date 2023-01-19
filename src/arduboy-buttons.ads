with Interfaces; use Interfaces;

package Arduboy.Buttons
with Elaborate_Body
is
   type Button_Kind is (B, A, Down, Left, Right, Up);

   function Pressed (Button : Button_Kind) return Boolean;
   --  Test if specified button is pressed

   function Pressed (Button_Mask : Unsigned_8) return Boolean;
   --  Test if specified buttons are pressed.
   --
   --  Use Button_Kind'Enum_Rep to create a button mask, e.g.:
   --    if Pressed (A'Enum_Rep or Left'Enum_Rep) then

   function Any_Pressed (Button_Mask : Unsigned_8) return Boolean;
   --  Test if any of the specified buttons is pressed

   function None_Pressed (Button_Mask : Unsigned_8) return Boolean;
   --  Test if none of thd specified buttons are presed

   procedure Poll_Buttons;
   --  Poll the buttons and track their state over time.

   --  Read and save the current state of the buttons and also keep track of
   --  the button state when this function was previously called. These states
   --  are used by the `Just_Pressed ()` and `Just_Released()` functions to
   --  determine if a button has changed state between now and the previous
   --  call to `Poll_Button s()`.
   --
   --  This function should be called once at the start of each new frame.
   --
   --  As long as the elapsed time between calls to this function is long
   --  enough, buttons will be naturally debounced. Calling it once per frame
   --  at a frame rate of 60 or lower (or possibly somewhat higher), should be
   --  sufficient.

   function Just_Pressed (Button : Button_Kind) return Boolean;
   --  Check if a button has just been pressed.
   --
   --  Return `True` if the given button was pressed between the latest call to
   --  `Poll_Buttons ()` and previous call to `Poll_Buttons()`. If the button
   --  has been held down over multiple polls, this function will return
   --  `False`.

   function Just_Released (Button : Button_Kind) return Boolean;
   --  Check if a button has just been released.
   --
   --  Return `True` if the given button, having previously been pressed, was
   --  released between the latest call to `Poll_Buttons ()` and previous call
   --  to `Poll_Buttons ()`. If the button has remained released over multiple
   --  polls, this function will return `False`.
   --
   --  There is no need to check for the button having been pressed since it
   --  must have been previously pressed for this function to return `True`
   --  upon release.
   --
   --  There aren't many cases where this function would be needed. Wanting to
   --  know if a button has been released, without knowing when it was pressed,
   --  is uncommon.

   procedure Wait_No_Buttons;
   --  Wait until all buttons have been released.
   --
   --  It won't return unless no buttons are being pressed. A short delay is
   --  performed each time before testing the state of the buttons to do a
   --  simple button debounce.
   --
   --  This function is called at the end of `begin()` to make sure no buttons
   --  used to perform system start up actions are still being pressed, to
   --  prevent them from erroneously being detected by the sketch code itself.

private

   for Button_Kind use (
                        B     => 2#00000100#,
                        A     => 2#00001000#,
                        Down  => 2#00010000#,
                        Left  => 2#00100000#,
                        Right => 2#01000000#,
                        Up    => 2#10000000#
                       );

end Arduboy.Buttons;
