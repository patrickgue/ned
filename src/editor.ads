with Ada.Containers.Vectors;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;


package Editor is

   type Line_Type is record
      Length : Natural;
      Content : Unbounded_Wide_Wide_String;
   end record;

   package Line_Vector is new
     Ada.Containers.Vectors
     (Index_Type  => Natural,
     Element_Type => Line_Type);

   type Buffer is record
      Pos_Abs      : Integer;

      Pos_Line_Nr  : Integer;
      Pos_On_Line  : Integer;
      Lines : Line_Vector.Vector;
   end record;

   procedure Add_Line (Buff : in out Buffer; Content : Wide_Wide_String);
   procedure Update_Buffer_Pos_Abs (Buff : in out Buffer);
end Editor;
