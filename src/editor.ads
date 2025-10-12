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
      Pos_Abs      : Integer;

      Pos_Line_Nr  : Integer;
      Pos_On_Line  : Integer;
      Lines : Line_Vector.Vector;
   end record;

   procedure Add_Line (Buff : in out Buffer; Content : Wide_Wide_String);
   procedure Update_Buffer_Pos_Abs (Buff : in out Buffer);
   procedure Insert_Char_At_Pos
     (Buff : in out Buffer; Char : Wide_Wide_Character);

   type Delete_Direction is (Forward, Backward);
   procedure Delete_Char_At_Pos (Buff : in out Buffer; Dir : Delete_Direction);

   type Virt_Cursor_Movement is (Left, Right, Up, Down, Start, End_Line);
   procedure Move_Cursor (Buff : in out Buffer; Mov : Virt_Cursor_Movement);

   procedure Render_Buffer (Buff : in out Buffer;
                            Height : Integer);
end Editor;
