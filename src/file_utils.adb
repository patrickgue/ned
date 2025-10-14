----------------------------------------------------------------------------
--  FILE-UTILS.ADS                                                        --
----------------------------------------------------------------------------
--  file utilities                                                        --
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
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

package body File_Utils is
   procedure Read_File_To_Buffer (Buff : in out Buffer; File_Name : String)
   is
         F : File_Type;
   begin
      Open (F, In_File, File_Name);
      while not End_Of_File (F) loop
         Add_Line (Buff, Get_Line (F));
      end loop;
      Close (F);
   end Read_File_To_Buffer;

   procedure Write_File_From_Buffer (Buff : in out Buffer; File_Name : String)
   is
      F : File_Type;
   begin
      Open (F, Out_File, File_Name);
      for I in Buff.Lines.First_Index .. Buff.Lines.Last_Index loop
         Put_Line (F, To_Wide_Wide_String (Buff.Lines (I).Content));
      end loop;
      Close (F);
      Buff.Modified := False;
   end Write_File_From_Buffer;
end File_Utils;
