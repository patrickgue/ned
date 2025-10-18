----------------------------------------------------------------------------
--  EDITOR.ADS                                                            --
----------------------------------------------------------------------------
--  editor logic definitions                                              --
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

with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package Editor is
   type Line_Type is record
      Content : Unbounded_Wide_Wide_String;
   end record;

   package Line_Vector is new
     Ada.Containers.Vectors
     (Index_Type  => Natural,
     Element_Type => Line_Type);

   type Buffer is record
      File_Name    : Unbounded_Wide_Wide_String;
      Pos_Abs      : Integer;

      Pos_Line_Nr  : Integer;
      Pos_On_Line  : Integer;
      Lines        : Line_Vector.Vector;
      Modified     : Boolean;
      Tab_Len      : Natural;
   end record;

   procedure Add_Line (Buff : in out Buffer; Content : Wide_Wide_String);
   procedure Update_Buffer_Pos_Abs (Buff : in out Buffer);
   procedure Insert_Tab_At_Pos (Buff : in out Buffer);
   procedure Insert_Char_At_Pos
     (Buff : in out Buffer; Char : Wide_Wide_Character);
   procedure Newline_At_Pos (Buff : in out Buffer);

   type Delete_Direction is (Forward, Backward);
   procedure Delete_Char_At_Pos (Buff : in out Buffer; Dir : Delete_Direction);

   type Virt_Cursor_Movement is (Left, Right, Up, Down, Start, End_Line);
   procedure Move_Cursor (Buff : in out Buffer; Mov : Virt_Cursor_Movement);

   procedure Render_Buffer (Buff : in out Buffer;
                            Height : Integer);
   procedure Process_Command (Buff : in out Buffer);
end Editor;
