with Interfaces.C; use Interfaces.C; 
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
    type rpm_header is record 
        version : dummy_byte; 
        reserved :  four_byte_number;
        indexes : four_byte_number;
        data_bytes : four_byte_number; 
    end record; 


end arpm_rpm_headers; 

