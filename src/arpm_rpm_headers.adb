package body arpm_Rpm_Headers is

   function Version  (This : in Rpm_Header) return Integer is
      function htons (Index : in dummy_byte) return dummy_byte;
      pragma Import  (C, htons, "htons");
   begin
      return Integer (htons (This.Version));
   end Version;
   function Indexes  (This : in Rpm_Header) return Integer is
      function htonl (Index : in Four_Byte_Number) return Four_Byte_Number;
      pragma Import  (C, htonl, "htonl");
   begin
      return Integer (htonl (This.Indexes));
   end Indexes;

   function Data_Bytes  (This : in Rpm_Header) return Integer is
      function htonl (Index : in Four_Byte_Number) return Four_Byte_Number;
      pragma Import  (C, htonl, "htonl");
   begin
      return Integer (htonl (This.data_bytes));
   end Data_Bytes;
end arpm_Rpm_Headers;

