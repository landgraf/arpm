with Ada.Streams.Stream_IO;

with League.Strings; use League.Strings;
with League.Text_Codecs;
package Internal_Codecs is 
   function Sea_To_Str (Sea : Ada.Streams.Stream_Element_Array) return String;
   function Str_To_Sea (Str : String) return Ada.Streams.Stream_Element_Array;
   function US_To_String(Item : Universal_String) return String;
   function String_To_US(Item : String) return Universal_String;
end Internal_Codecs;
