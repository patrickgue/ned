with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Editor is

   type Line_Type is record
      Length : Natural;
      Content : Unbounded_String;
   end record;

   package Line_Vector is new
     Ada.Containers.Vectors
     (Index_Type   => Natural,
     Element_Type => Line_Type);

   type Buffer is record
      Pos_Abs      : Integer;
      Pos_Line_Nr  : Integer;
      Pos_On_Line  : Integer;
      Lines : Line_Vector.Vector;
   end record;

   procedure Add_Line (Buff : in out Buffer; Content : String);
end Editor;
