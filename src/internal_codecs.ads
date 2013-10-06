with Ada.Streams;
package Internal_Codecs is
   function Sea_To_Str (Sea : Ada.Streams.Stream_Element_Array) return String;
   function Str_To_Sea (Str : String) return Ada.Streams.Stream_Element_Array;
end Internal_Codecs;
