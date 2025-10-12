----------------------------------------------------------------------------
--  NED.ADB                                                               --
----------------------------------------------------------------------------
--  main function of the program                                          --
----------------------------------------------------------------------------
--  Copyright (C) 2025 Patrick Günthard                                   --
--                                                                        --
--  This program is free software: you can redistribute it and/or modify  --
--  it under the terms of the GNU General Public License as published by  --
--  the Free Software Foundation, either version 3 of the License, or     --
--  (at your option) any later version.                                   --
--                                                                        --
--  This program is distributed in the hope that it will be useful,       --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--  GNU General Public License for more details.                          --
--                                                                        --
--  You should have received a copy of the GNU General Public License     --
--  along with this program.  If not, see <https://www.gnu.org/licenses/>.--
----------------------------------------------------------------------------

with VT100; use VT100;
with VT100.Utils;
with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;
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
   In_Ch : Wide_Wide_Character := To_Wide_Wide_Character (ASCII.NUL);
   Pos : Integer := 0;
   Esc : Boolean := False;
   Main_Loop : Boolean := True;

   function WWS (Inp : String)
                return Wide_Wide_String
     renames To_Wide_Wide_String;
begin
   Save_Screen;
   Curr_Buff.Pos_Line_Nr := 0;
   Curr_Buff.Pos_On_Line := 0;
   Add_Line (Curr_Buff, "Hello World");
   Add_Line (Curr_Buff, "Lorem Ipsum");
   Add_Line (Curr_Buff, "Hellö in UTF-8  漢字");
   Add_Line (Curr_Buff, "This Line is longer than 100 characters. This Line is longer than 100 characters. This Line is longer than 100 characters.");

   for I in 1 .. 100 loop
      Add_Line (Curr_Buff, "Hello");
   end loop;

   while Main_Loop loop
      Clear_Screen;

      Render_Buffer (Curr_Buff, VT100.Utils.Lines - 2);
      Move_Cursor (VT100.Utils.Lines, 1);

      Set_Background_Color (Blue);
      Set_Foreground_Color (White);

      Put (WWS (
        "L:" & Natural'Image (Curr_Buff.Pos_Line_Nr) &
        " C:" & Natural'Image (Curr_Buff.Pos_On_Line)));

      for E of V loop
         Put (WWS (" ") & WWS (Integer'Image (Wide_Wide_Character'Pos (E))));
      end loop;
      Set_Background_Color (Default);
      Set_Foreground_Color (Default);

      Get_Immediate (In_Ch);
      Pos := Wide_Wide_Character'Pos (In_Ch);
      V.Append (In_Ch);
      if Natural (V.Length) > 10 then
         V.Delete (V.First_Index);
      end if;
      if Pos >= 32 and then Pos /= 127 then
         if Esc then
            case In_Ch is
               when 'x' => Main_Loop := False;
               when 'A' | 'k' => Move_Cursor (Curr_Buff, Up);
               when 'B' | 'j' => Move_Cursor (Curr_Buff, Down);
               when 'C' | 'l' => Move_Cursor (Curr_Buff, Right);
               when 'D' | 'h' => Move_Cursor (Curr_Buff, Left);
               when others => null; -- other keys can be ignored
            end case;

            --  arrow keys are ESC[A|B|C|D -> keep ESC := True
            if Pos /= 91 then
               Esc := False;
            end if;
         else
            Insert_Char_At_Pos (Curr_Buff, In_Ch);
         end if;
      else
         case Pos is
            when 1 => Move_Cursor (Curr_Buff, Start);
            when 2 => Move_Cursor (Curr_Buff, Left);
            when 5 => Move_Cursor (Curr_Buff, End_Line);
            when 6 => Move_Cursor (Curr_Buff, Right);
            when 14 => Move_Cursor (Curr_Buff, Down);
            when 16 => Move_Cursor (Curr_Buff, Up);
            when 27 => Esc := True;
            when 127 => Delete_Char_At_Pos (Curr_Buff, Backward);
            when others => null;
         end case;
      end if;
   end loop;
   Restore_Screen;
end Ned;
