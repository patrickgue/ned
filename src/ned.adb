with VT100; use VT100;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Editor; use Editor;

procedure Ned is
   Current_Buffer :  Buffer;
   I : Natural;
   Win_X : constant Natural := 2;
   Win_Y : constant Natural := 2;
begin
   Add_Line (Current_Buffer, "Hello World");
   Add_Line (Current_Buffer, "Lorem Ipsum");

   Clear_Screen;
   for I in Current_Buffer.Lines.First_Index .. Current_Buffer.Lines.Last_Index loop
      Move_Cursor (Win_Y + Natural (I), Win_X);
      declare
         Content_Line : Line_Type := Current_Buffer.Lines (Natural (I));
      begin
         Put_Line (To_String (Content_Line.Content) (1 .. 3));
      end;
   end loop;
   Put_Line ("  " & Natural'Image (I) & "  " & Natural'Image(Current_Buffer.Lines.First_Index) & "  " & Natural'Image(Current_Buffer.Lines.Last_Index));
end Ned;
