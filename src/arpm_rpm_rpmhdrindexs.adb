with Ada.Text_IO; use Ada.Text_IO;
with  Ada.Unchecked_Conversion;
package body arpm_rpm_rpmhdrIndexs is

   function htonl (Index : in Four_Byte_Number) return Four_Byte_Number;
   pragma Import (C, htonl, "htonl");

   function Tag (Index : in rpmhdrIndex) return Tags_Type is
      function Extract_Tag is
         new Ada.Unchecked_Conversion (Four_Byte_Number, Tags_Type);
   begin
      return Extract_Tag (htonl (Index.Tag));
   exception
      when Constraint_Error =>
         Put_Line ("Exception Tag: " & htonl (Index.Tag)'Img);
         raise;
   end Tag;

   function Format (Index : in rpmhdrIndex) return Format_Type  is
      function Extract_Format is
         new Ada.Unchecked_Conversion (Four_Byte_Number, Format_Type);
   begin
      return Extract_Format (htonl (Index.Format));
   exception
      when Constraint_Error =>
         Put_Line ("Exception Format: " & htonl (Index.Format)'Img);
         raise;
   end Format;

   function Data_Items (Index : in rpmhdrIndex) return Integer is
   begin
      return  Integer (htonl (Index.number_of_Data_Items));
   end Data_Items;

   function Data_Offset (Index : in rpmhdrIndex) return Integer is
   begin
      return  Integer (htonl (Index.Data_Offset));
   end Data_Offset;

end arpm_rpm_rpmhdrIndexs;

