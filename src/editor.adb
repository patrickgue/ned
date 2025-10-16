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

with Ada.Wide_Wide_Text_IO; use Ada.Wide_Wide_Text_IO;

with VT100; use VT100;
with VT100.Utils;

package body Editor is
   -----------------------
   --  A D D _ L I N E  --
   -----------------------

   procedure Add_Line (Buff : in out Buffer; Content : Wide_Wide_String)
   is
      Tmp : Line_Type :=
        (Content => To_Unbounded_Wide_Wide_String (Content));
   begin
      Buff.Lines.Append (Tmp);
   end Add_Line;

   -------------------------------------------------
   --  U P D A T E _ B U F F E R _ P O S _ A B S  --
   -------------------------------------------------

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

   -------------------------------------------
   --  I N S E R T _ C H A R _ A T _ P O S  --
   -------------------------------------------

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
      Buff.Modified := True;
   end Insert_Char_At_Pos;

   -------------------------------------------
   --  D E L E T E _ C H A R _ A T _ P O S  --
   -------------------------------------------

   procedure Delete_Char_At_Pos (Buff : in out Buffer; Dir : Delete_Direction)
   is
   begin
      if Dir = Backward then
         --  Somewhere inside the line
         if Buff.Pos_On_Line > 0 then
            declare
               Line_Str : constant Wide_Wide_String
                 := To_Wide_Wide_String (
                 Buff.Lines (Buff.Pos_Line_Nr).Content);

               First_Half : constant Wide_Wide_String
                 := Line_Str (1 .. Buff.Pos_On_Line - 1);
               Second_Half : constant Wide_Wide_String
                 := Line_Str (Buff.Pos_On_Line + 1 .. Line_Str'Length);
            begin
               Buff.Lines (Buff.Pos_Line_Nr).Content
                 := To_Unbounded_Wide_Wide_String (First_Half & Second_Half);
               Buff.Pos_On_Line := Buff.Pos_On_Line - 1;
            end;

         else --  At the beginning of the line; combine with the previous line
            --  Only run if cursor is not on first line, else do nothing
            if Buff.Pos_Line_Nr > 0 then
               declare
                  Old_Len : constant Natural
                    := Length (Buff.Lines (Buff.Pos_Line_Nr - 1).Content);
               begin
                  Buff.Lines (Buff.Pos_Line_Nr - 1).Content :=
                    Buff.Lines (Buff.Pos_Line_Nr - 1).Content &
                    Buff.Lines (Buff.Pos_Line_Nr).Content;
                  Buff.Lines.Delete (Buff.Pos_Line_Nr);
                  Buff.Pos_Line_Nr := Buff.Pos_Line_Nr - 1;
                  Buff.Pos_On_Line := Old_Len;
               end;
            end if;
         end if;
      --  forward
      else
         if Buff.Pos_On_Line < Length (Buff.Lines (Buff.Pos_Line_Nr).Content)
         then
            declare
               Line_Str : constant Wide_Wide_String
                 := To_Wide_Wide_String (
                 Buff.Lines (Buff.Pos_Line_Nr).Content);

               First_Half : constant Wide_Wide_String
                 := Line_Str (1 .. Buff.Pos_On_Line);
               Second_Half : constant Wide_Wide_String
                 := Line_Str (Buff.Pos_On_Line + 2 .. Line_Str'Length);
            begin
               Buff.Lines (Buff.Pos_Line_Nr).Content
                 := To_Unbounded_Wide_Wide_String (First_Half & Second_Half);
            end;
         else --  At the end of the line; combine with the next line
            --  Only run if cursor is not on last line. Else do nothing
            if Buff.Pos_Line_Nr < Buff.Lines.Last_Index then
               Buff.Lines (Buff.Pos_Line_Nr).Content :=
                 Buff.Lines (Buff.Pos_Line_Nr).Content &
                 Buff.Lines (Buff.Pos_Line_Nr + 1).Content;
               Buff.Lines.Delete (Buff.Pos_Line_Nr + 1);
            end if;
         end if;
      end if;
      Buff.Modified := True;
   end Delete_Char_At_Pos;

   -----------------------------------
   --  N E W L I N E _ A T _ P O S  --
   -----------------------------------

   procedure Newline_At_Pos (Buff : in out Buffer)
   is
      Line_Str : constant Wide_Wide_String
        := To_Wide_Wide_String (Buff.Lines (Buff.Pos_Line_Nr).Content);
      First_Index : constant Integer := Line_Str'First;
      Last_Index : constant Integer := Line_Str'Last;

      First_Half : constant Wide_Wide_String
        := Line_Str (First_Index .. First_Index + Buff.Pos_On_Line - 1);
      Second_Half : constant Wide_Wide_String
        := Line_Str (First_Index + Buff.Pos_On_Line .. Last_Index);
   begin
      Buff.Lines.Insert (Natural (Buff.Pos_Line_Nr + 1),
        (Content => To_Unbounded_Wide_Wide_String (Second_Half)));
      Buff.Lines (Buff.Pos_Line_Nr).Content
        := To_Unbounded_Wide_Wide_String (First_Half);
      Buff.Pos_Line_Nr := Buff.Pos_Line_Nr + 1;
      Buff.Pos_On_Line := 0;
      Buff.Modified := True;
   end Newline_At_Pos;

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
           To_Unbounded_Wide_Wide_String (
           Integer'Wide_Wide_Image (I + 1)), 4, ' ')
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

   -------------------------------------
   --  P R O C E S S _ C O M M A N D  --
   -------------------------------------

   procedure Process_Command (Buff : in out Buffer) is
      Command : Unbounded_Wide_Wide_String;
   begin
      Put (":");
      Command := To_Unbounded_Wide_Wide_String (Get_Line (Standard_Input));
      declare
         C : constant Wide_Wide_String := To_Wide_Wide_String (Command);
      begin
         case C (C'First) is
            when 'o' => Buff.File_Name
               := To_Unbounded_Wide_Wide_String (C (C'First + 1 .. C'Last));
            when others => null;
         end case;
      end;
   end Process_Command;
end Editor;
