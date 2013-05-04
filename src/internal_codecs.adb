with Ada.Text_IO; use Ada.Text_IO;
package body Internal_Codecs is

   function Str_To_Sea (Str : String) return Ada.Streams.Stream_Element_Array is
      use Ada.Streams;
      Result : Stream_Element_Array (Stream_Element_Offset (Str'First) ..
                                     Stream_Element_Offset (Str'Last));
      for Result'Address use Str'Address;
   begin
      return Result;
   end Str_To_Sea;

   function Sea_To_Str (Sea : Ada.Streams.Stream_Element_Array) return String is
      use Ada.Streams;
      Result : String (1 .. Sea'Length);
      for Result'Address use Sea'Address;
   begin
      return Result;
   end Sea_To_Str;

end Internal_Codecs;
