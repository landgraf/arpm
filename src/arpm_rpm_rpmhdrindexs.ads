with ARPM_RPM_Types; use ARPM_RPM_Types; 
package arpm_rpm_rpmhdrIndexs is
   type rpmhdrIndex is private;
   type rpmhdrIndex_access is access all rpmhdrIndex;

   function Format (Index : in rpmhdrIndex) return Format_Type;
   function Tag (Index : in rpmhdrIndex) return Tags_Type;
   function Data_Items (Index : in rpmhdrIndex) return Integer;
   function Data_Offset (Index : in rpmhdrIndex) return Integer;
   type Four_Byte_Number is range 0 .. 2**(4*8)-1;
   for Four_Byte_Number'Size use 32;
   type Two_Byte_Number is range 0 .. 2**(2*8)-1;
   for Two_Byte_Number'Size use 16;
private
   type rpmhdrIndex is record
     Tag : Four_Byte_Number := 0;
     Format : Four_Byte_Number := 0;
     Data_Offset :  Four_Byte_Number := 0;
     number_of_Data_Items  : Four_Byte_Number := 0;
   end record;
   for rpmhdrIndex'Size use 16*8;


end arpm_rpm_rpmhdrIndexs;

