with VT100; use VT100;
with VT100.Utils;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers.Vectors;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Editor; use Editor;

procedure Ned is
   package Char_Vectors is new
     Ada.Containers.Vectors
     (Index_Type   => Natural,
     Element_Type => Wide_Wide_Character);

   V : Char_Vectors.Vector;

   Curr_Buff :  Buffer;
   I : Integer;
   Win_X : constant Natural := 0;
   Win_Y : constant Natural := 1;
   In_Ch : Wide_Wide_Character := To_Wide_Wide_Character (ASCII.NUL);
   Pos : Integer := 0;

   function WWS (Inp : String) return Wide_Wide_String renames To_Wide_Wide_String;
begin
   Save_Screen;

   Add_Line (Curr_Buff, "Hello World");
   Add_Line (Curr_Buff, "Lorem Ipsum");
   Add_Line (Curr_Buff, "Hellö in UTF-8  漢字");
   Add_Line (Curr_Buff, "This Line is longer than 100 characters. This Line is longer than 100 characters. This Line is longer than 100 characters.");

   while Pos /= 27 loop
      Clear_Screen;

      for I in Curr_Buff.Lines.First_Index .. Curr_Buff.Lines.Last_Index loop
         Move_Cursor (Win_Y + Natural (I), Win_X);
         declare
            Content_Line : constant Line_Type := Curr_Buff.Lines (Natural (I));
            Limit : Natural := VT100.Utils.Columns;
         begin
            if Length (Content_Line.Content) < Limit then
               Limit := Length (Content_Line.Content);
            end if;
            Put_Line (To_Wide_Wide_String (Content_Line.Content) (1 .. Limit));
         end;
      end loop;

      Set_Background_Color (Blue);
      Set_Foreground_Color (White);

      Move_Cursor (VT100.Utils.Lines, 1);

      Put (WWS (
        Natural'Image (I) & " " &
        Natural'Image (Curr_Buff.Lines.First_Index) & " " &
        Natural'Image (Curr_Buff.Lines.Last_Index) & " " &
        Natural'Image (VT100.Utils.Columns) & " '") &
        In_Ch & "'");

      for E of V loop
         Put (WWS (" ") & WWS (Integer'Image (Wide_Wide_Character'Pos (E))));
      end loop;
      Set_Attribute (Reset);

      Get_Immediate (In_Ch);
      Pos := Wide_Wide_Character'Pos (In_Ch);
      V.Append (In_Ch);
   end loop;
   Restore_Screen;
end Ned;
