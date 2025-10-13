----------------------------------------------------------------------------
--  FILE-UTILS.ADS                                                        --
----------------------------------------------------------------------------
--  file utilities definitions                                            --
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

with Editor; use Editor;

package File_Utils is
   procedure Read_File_To_Buffer (Buff : in out Buffer; File_Name : String);
   procedure Write_File_From_Buffer (Buff : Buffer; File_Name : String);
end File_Utils;
