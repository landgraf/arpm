with Interfaces.C; use Interfaces.C; 
package arpm_rpm_headers is 
    type rpm_header is private; 
    type rpm_header_access is access all rpm_header; 

    type dummy_byte is range 0..2**8-1; 
    for dummy_byte'Size use 8; 
    LABELONE : constant array (1..3) of dummy_byte := (142, 173, 232); 
    private 
    type rpm_header is null record; 


end arpm_rpm_headers; 

