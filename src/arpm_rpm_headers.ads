with Interfaces.C; use Interfaces.C; 
with System; use System; 
package arpm_rpm_headers is 
    type rpm_header is private; 
    type rpm_header_access is access all rpm_header; 

    function Version (This : in RPM_Header) return Integer; 
    function Indexes (This : in RPM_Header) return Integer; 
    function Data_Bytes (This : in RPM_Header) return Integer; 
    type dummy_byte is range 0..2**(1*8)-1; 
    for dummy_byte'Size use 8; 
    LABELONE : constant array (1..3) of dummy_byte := (16#8e#, 16#ad#, 16#e8#); 
    private
    type four_byte_number is range 0..2**(4*8)-1;
    for four_byte_number'Size use 32; 
    type rpm_header is record 
        version : dummy_byte; 
        reserved :  four_byte_number;
        indexes : four_byte_number := 0;
        data_bytes : four_byte_number := 0; 
    end record; 
    for rpm_header use record
        version at 0 range 0..7; 
        reserved at 1 range 0..31;
        indexes at 5 range 0..31; 
        data_bytes at 9 range 0..31; 
    end record; 
    for rpm_header'Bit_Order use High_Order_First;

end arpm_rpm_headers; 

