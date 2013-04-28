with League.Stream_Element_Vectors; use League.Stream_Element_Vectors;
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

   function US_To_String(Item : Universal_String) return String is
         Str : Ada.Streams.Stream_Element_Array := To_Stream_Element_Array(League.Text_Codecs.Codec_For_Application_Locale.Encode(Item));
    begin
        return Sea_To_Str(Str);
    end US_To_String;

   function String_To_US(Item : String) return Universal_String is
    US : Universal_String;
    S  : Ada.Streams.Stream_Element_Array := Str_to_Sea(Item);
    begin
         US := League.Text_Codecs.Codec_For_Application_Locale.Decode(S);
    return US;
    end String_To_US;
end Internal_Codecs;
