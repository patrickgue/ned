package body Editor is
   procedure Add_Line (Buff : in out Buffer; Content : Wide_Wide_String) is
      Tmp : Line_Type :=
        (Content => To_Unbounded_Wide_Wide_String (Content),
        Length   => Content'Length);
   begin
      Buff.Lines.Append (Tmp);
   end Add_Line;

   procedure Update_Buffer_Pos_Abs (Buff : in out Buffer) is
      Count : Integer := 0;
   begin
      --  Loop to line before current cursor position
      for I in Buff.Lines.First_Index .. Buff.Pos_Line_Nr - 1 loop
         Count := Count + Length (Buff.Lines (I).Content);
      end loop;
      Buff.Pos_Abs := Count + Buff.Pos_On_Line;
   end Update_Buffer_Pos_Abs;
end Editor;
