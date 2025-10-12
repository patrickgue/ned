-------------------------------------------------------------------------------
-- Copyright © 2011 - 2015 darkestkhan licensed under ISC                   --
-- Modifications Copyright © 2025 patrickgue licensed under GNU GPL v3      --
-------------------------------------------------------------------------------
--  Permission to use, copy, modify, and/or distribute this software for any --
--  purpose with or without fee is hereby granted, provided that the above   --
--  copyright notice and this permission notice appear in all copies.        --
--                                                                           --
--  The software is provided "as is" and the author disclaims all warranties --
--  with regard to this software including all implied warranties of         --
--  merchantability and fitness. In no event shall the author be liable for  --
--  any special, direct, indirect, or consequential damages or any damages   --
--  whatsoever resulting from loss of use, data or profits, whether in an    --
--  action of contract, negligence or other tortious action, arising out of  --
--  or in connection with the use or performance of this software.           --
-------------------------------------------------------------------------------
--  Changelog                                                                --
--  patrickgue: Terminal size is now accessed via ioctl because usually the  --
--              variables COLUMNS/LINES are only _shell variables_ and not   --
--              env variables and therefore not accessible inside the        --
--              program                                                      --
-------------------------------------------------------------------------------
--  Utility subprograms for ANSI/VT100 API wrapper
with Interfaces.C;

package VT100.Utils is
   function Lines return Natural;
   --  return number of lines displayed on screen

   function Columns return Natural;
   --  return number of column displayed on screen
private
   function Term_Cols return Interfaces.C.int
     with
     Import => True,
     Convention => C;

   function Term_Lines return Interfaces.C.int
     with
     Import => True,
     Convention => C;
end VT100.Utils;
