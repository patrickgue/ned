----------------------------------------------------------------------------
--  EDITOR.ADB                                                            --
----------------------------------------------------------------------------
--  editor logic                                                          --
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

package body Editor is
   procedure Add_Line (Buff : in out Buffer; Content : Wide_Wide_String)
   is
      Tmp : Line_Type :=
        (Content => To_Unbounded_Wide_Wide_String (Content));
   begin
      Buff.Lines.Append (Tmp);
   end Add_Line;

   procedure Update_Buffer_Pos_Abs (Buff : in out Buffer)
   is
      Count : Integer := 0;
   begin
      --  Loop to line before current cursor position
      for I in Buff.Lines.First_Index .. Buff.Pos_Line_Nr - 1 loop
         Count := Count + Length (Buff.Lines (I).Content);
      end loop;
      Buff.Pos_Abs := Count + Buff.Pos_On_Line;
   end Update_Buffer_Pos_Abs;

   procedure Insert_Char_At_Pos (Buff : in out Buffer;
                                 Char : Wide_Wide_Character)
   is
      Line_Str : constant Wide_Wide_String
        := To_Wide_Wide_String (Buff.Lines (Buff.Pos_Line_Nr).Content);

      First_Half : constant Wide_Wide_String
        := Line_Str (1 .. Buff.Pos_On_Line);
      Second_Half : constant Wide_Wide_String
        := Line_Str (Buff.Pos_On_Line + 1 .. Line_Str'Length);
   begin
      Buff.Lines (Buff.Pos_Line_Nr).Content
        := To_Unbounded_Wide_Wide_String (First_Half & Char & Second_Half);
      Buff.Pos_On_Line := Buff.Pos_On_Line + 1;
   end Insert_Char_At_Pos;

   procedure Delete_Char_At_Pos (Buff : in out Buffer; Dir : Delete_Direction)
   is
   begin
      if Dir = Backward then
         if Buff.Pos_On_Line > 0 then
            declare
               Line_Str : constant Wide_Wide_String
                 := To_Wide_Wide_String (Buff.Lines (Buff.Pos_Line_Nr).Content);

               First_Half : constant Wide_Wide_String
                 := Line_Str (1 .. Buff.Pos_On_Line - 1);
               Second_Half : constant Wide_Wide_String
                 := Line_Str (Buff.Pos_On_Line + 1 .. Line_Str'Length);
            begin
               Buff.Lines (Buff.Pos_Line_Nr).Content
                 := To_Unbounded_Wide_Wide_String (First_Half & Second_Half);
               Buff.Pos_On_Line := Buff.Pos_On_Line - 1;
            end;
         end if;
      end if;
   end Delete_Char_At_Pos;

   procedure Move_Cursor (Buff : in out Buffer; Mov : Virt_Cursor_Movement)
   is
      Line_Length : constant Integer
        := Length (Buff.Lines (Buff.Pos_Line_Nr).Content);
      New_Length : Integer;
   begin
      if Mov = Down then
         if Buff.Pos_Line_Nr < Buff.Lines.Last_Index then
            Buff.Pos_Line_Nr := Buff.Pos_Line_Nr + 1;
         end if;

         New_Length := Length (Buff.Lines (Buff.Pos_Line_Nr).Content);
         if Buff.Pos_On_Line > New_Length then
            Buff.Pos_On_Line := New_Length;
         end if;
      elsif Mov = Up then
         if Buff.Pos_Line_Nr > Buff.Lines.First_Index then
            Buff.Pos_Line_Nr := Buff.Pos_Line_Nr - 1;
            New_Length := Length (Buff.Lines (Buff.Pos_Line_Nr).Content);
            if Buff.Pos_On_Line > New_Length then
               Buff.Pos_On_Line := New_Length;
            end if;
         else
            Buff.Pos_On_Line := 0;
         end if;

      elsif Mov = Right then
         if Buff.Pos_On_Line < Line_Length then
            Buff.Pos_On_Line := Buff.Pos_On_Line + 1;
         else
            Move_Cursor (Buff, Down);
            Buff.Pos_On_Line := 0;
         end if;
      elsif Mov = Left then
         if Buff.Pos_On_Line > 0 then
            Buff.Pos_On_Line := Buff.Pos_On_Line - 1;
         else
            Move_Cursor (Buff, Up);
            Buff.Pos_On_Line := Line_Length;
         end if;
      elsif Mov = Start then
         Buff.Pos_On_Line := 0;
      else
         Buff.Pos_On_Line := Line_Length;
      end if;
   end Move_Cursor;

   --------------------------------
   --  R E N D E R _ B U F F E R --
   --------------------------------

   procedure Render_Buffer (Buff : in out Buffer;
                            Height : Integer)
   is
      First_Line_Index : Integer;
      Last_Line_Index : Integer;
   begin
      First_Line_Index := Buff.Pos_Line_Nr - (Height / 2);
      if First_Line_Index < Buff.Lines.First_Index then
         First_Line_Index := Buff.Lines.First_Index;
      end if;

      Last_Line_Index := First_Line_Index + Height;
      if Last_Line_Index > Buff.Lines.Last_Index then
         Last_Line_Index := Buff.Lines.Last_Index;
      end if;

      for I in First_Line_Index .. Last_Line_Index loop
         Move_Cursor (1 + Natural (I) - First_Line_Index, 0);
         Set_Attribute (Dim);
         Put (To_Wide_Wide_String (Tail (
           To_Unbounded_Wide_Wide_String (Integer'Wide_Wide_Image (I)), 4, ' ')
                                  ) & " ");
         Reset_Attribute (Dim);
         declare
            Content_Line : constant Line_Type := Buff.Lines (Natural (I));
            Limit : Natural := VT100.Utils.Columns - 5;
         begin
            if Length (Content_Line.Content) < Limit then
               Limit := Length (Content_Line.Content);
            end if;

            if I = Buff.Pos_Line_Nr then
               declare
                  Line_Str : constant Wide_Wide_String
                    := To_Wide_Wide_String (Content_Line.Content);
                  F_Idx : constant Integer := Line_Str'First;
                  L_Idx : constant Integer := Line_Str'Last;
                  C_Idx : constant Integer := F_Idx + Buff.Pos_On_Line;
                  First_Half : constant Wide_Wide_String
                    := Line_Str (F_Idx .. C_Idx - 1);
                  Second_Half : constant Wide_Wide_String
                    := Line_Str (C_Idx + 1 .. L_Idx);

                  Curs_Char : Wide_Wide_Character;
               begin
                  if C_Idx > L_Idx then
                     Curs_Char := ' ';
                  else
                     Curs_Char := Line_Str (C_Idx);
                  end if;

                  Put (First_Half);
                  Set_Background_Color (White);
                  Set_Foreground_Color (Black);
                  Put (Curs_Char);
                  Set_Background_Color (Default);
                  Set_Foreground_Color (Default);
                  Put (Second_Half);
               end;
            else
               Put_Line (
                 To_Wide_Wide_String (Content_Line.Content) (1 .. Limit));
            end if;
         end;
      end loop;

   end Render_Buffer;
end Editor;
