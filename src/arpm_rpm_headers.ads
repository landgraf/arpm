package arpm_Rpm_Headers is
   type Rpm_Header is private;
   type Rpm_Header_access is access all Rpm_Header;

   function Version  (This : in Rpm_Header) return Integer;
   function Indexes  (This : in Rpm_Header) return Integer;
   function Data_Bytes  (This : in Rpm_Header) return Integer;
   type dummy_byte is range 0 .. 2** (1*8)-1;
   for dummy_byte'Size use 8;
   LABELONE : constant array  (1 .. 3)
      of dummy_byte :=  (16#8e#, 16#ad#, 16#e8#);
private
   type Four_Byte_Number is range 0 .. 2** (4*8)-1;
   for Four_Byte_Number'Size use 32;
   type Rpm_Header is record
      Version : dummy_byte;
      reserved :  Four_Byte_Number;
      Indexes : Four_Byte_Number := 0;
      data_bytes : Four_Byte_Number := 0;
   end record;
   for Rpm_Header use record
      Version at 0 range 0 .. 7;
      reserved at 1 range 0 .. 31;
      Indexes at 5 range 0 .. 31;
      data_bytes at 9 range 0 .. 31;
   end record;
   for Rpm_Header'Size use 13*8;

end arpm_Rpm_Headers;

