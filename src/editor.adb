package body Editor is

   procedure Add_Line (Buff : in out Buffer; Content : String) is
      Tmp : Line_Type :=
        (Content => To_Unbounded_String (Content),
        Length   => Content'Length);
   begin
      Buff.Lines.Append (Tmp);
   end Add_Line;
end Editor;
