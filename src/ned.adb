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
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;
with Ada.Containers.Vectors;
with Ada.Characters.Conversions; use Ada.Characters.Conversions;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.IO_Exceptions;
with Ada.Strings.Fixed;

with Editor; use Editor;
with File_Utils; use File_Utils;

procedure Ned is
   package Char_Vectors is new
     Ada.Containers.Vectors
     (Index_Type   => Natural,
     Element_Type => Integer);

   V : Char_Vectors.Vector;

   Curr_Buff :  Buffer;
   In_Ch : Wide_Wide_Character := To_Wide_Wide_Character (ASCII.NUL);
   Pos : Integer := 0;
   Esc : Boolean := False;
   Main_Loop : Boolean := True;

   function WWS (Inp : String)
                return Wide_Wide_String
     renames To_Wide_Wide_String;

   function Natural_As_String (N : Natural) return String is
      use Ada.Strings.Fixed;
   begin
      return Trim (Natural'Image (N), Ada.Strings.Left);
   end Natural_As_String;

   procedure Exit_Query is
      Ch : Wide_Wide_Character := '0';
   begin
      Move_Cursor (VT100.Utils.Lines, 1);
      Erase_Line;

      if Curr_Buff.Modified then
         Put (WWS ("Buffer was modified. Discard changes? (y/n) "));
         while not (Ch = 'y' or else Ch = 'n') loop
            Get_Immediate (Ch);
         end loop;
         Main_Loop := Ch = 'n';
      else
         Main_Loop := False;
      end if;
   end Exit_Query;
begin
   --  load file specified in arguments
   if Argument_Count >= 1 then
      Read_File_To_Buffer (Curr_Buff, Argument (1));
   else
      Curr_Buff.File_Name := To_Unbounded_Wide_Wide_String ("UNTITLED");
      Add_Line (Curr_Buff, "");
   end if;

   --  tell terminal to save the previous content
   Save_Screen;

   Hide_Cursor;

   --  Buffer initialization
   Curr_Buff.Modified := False;
   Curr_Buff.Pos_Line_Nr := 0;
   Curr_Buff.Pos_On_Line := 0;

   while Main_Loop loop
      Clear_Screen;

      Render_Buffer (Curr_Buff, VT100.Utils.Lines - 2);
      Move_Cursor (VT100.Utils.Lines, 1);

      Set_Background_Color (Blue);
      Set_Foreground_Color (White);

      if Curr_Buff.Modified then
         Put ("* ");
      else
         Put ("  ");
      end if;
      Put (To_Wide_Wide_String (Curr_Buff.File_Name));
      Put (WWS (
        " L:" & Natural_As_String (Curr_Buff.Pos_Line_Nr + 1) &
        "/" & Natural_As_String (Curr_Buff.Lines.Last_Index + 1) &
        " C:" & Natural_As_String (Curr_Buff.Pos_On_Line)));

      --  DEBUG: print entered char codes. Will be removed at some point
      Set_Background_Color (Red);
      for E of V loop
         Put (WWS (" ") & WWS (Natural_As_String (E)));
      end loop;
      Set_Background_Color (Default);
      Set_Foreground_Color (Default);

      --  Read character from stdin
      begin
         Get_Immediate (In_Ch);
         Pos := Wide_Wide_Character'Pos (In_Ch);
      exception
         when Ada.IO_Exceptions.End_Error => Pos := 4;
      end;

      V.Append (Pos);
      if Natural (V.Length) > 10 then
         V.Delete (V.First_Index);
      end if;
      if Pos >= 32 and then Pos /= 127 then
         if Esc then
            case In_Ch is
               when 'x' => Exit_Query;
               when 'w' => Write_File_From_Buffer (Curr_Buff);
               when 'A' | 'k' => Move_Cursor (Curr_Buff, Up);
               when 'B' | 'j' => Move_Cursor (Curr_Buff, Down);
               when 'C' | 'l' => Move_Cursor (Curr_Buff, Right);
               when 'D' | 'h' => Move_Cursor (Curr_Buff, Left);
               when ':' => Process_Command (Curr_Buff);
               when others => null; --  other keys can be ignored
            end case;

            if Pos = 126 then
               Delete_Char_At_Pos (Curr_Buff, Forward);
            end if;

            --  arrow keys are ESC[A|B|C|D -> keep ESC := True
            if Pos /= 91 and then Pos /= 51 then
               Esc := False;
            end if;
         else
            Insert_Char_At_Pos (Curr_Buff, In_Ch);
         end if;
      else
         case Pos is
            when 1 => Move_Cursor (Curr_Buff, Start);              --  C-a
            when 2 => Move_Cursor (Curr_Buff, Left);               --  C-b
            when 4 => Delete_Char_At_Pos (Curr_Buff, Forward);     --  C-d
            when 5 => Move_Cursor (Curr_Buff, End_Line);           --  C-e
            when 6 => Move_Cursor (Curr_Buff, Right);              --  C-f
            when 9 => Insert_Tab_At_Pos (Curr_Buff);
            when 10 => Newline_At_Pos (Curr_Buff);                 --  Enter
            when 14 => Move_Cursor (Curr_Buff, Down);              --  C-n
            when 16 => Move_Cursor (Curr_Buff, Up);                --  C-p
            when 27 => Esc := True;                                --  Esc
            when 127 => Delete_Char_At_Pos (Curr_Buff, Backward);  --  Delete
            when others => null;
         end case;
      end if;
   end loop;
   Show_Cursor;
   Restore_Screen;
end Ned;
