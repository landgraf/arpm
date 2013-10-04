with Ada.Text_IO; use Ada.Text_IO; 
package body arpm_rpm_leaders is 

    -- procedure htonl(Number : in short; Result : out short); 
    function htonl_short(Number : in short) return short; 
        pragma Import(C, htonl_short, "htons"); 
    function htonl_2bn(Number : in two_bytes_number) return two_bytes_number; 
        pragma Import(C, htonl_2bn, "htonl"); 
    function htonl_uchar(Number : in unsigned_char) return unsigned_char; 
        pragma Import(C, htonl_uchar, "htonl"); 
    function htonl_magic(Number : in magic_type) return magic_type; 
        pragma Import(C, htonl_magic, "htonl"); 

    function Name(This : rpm_leader) return String is 
    begin
        return To_Ada(This.Name); 
    end Name;
    function RPMType ( This : rpm_leader) return rpmtypes is 
    begin
        Put_line("Type " & This.rpmtype'Img );
        return rpmtypes'Val(Integer(htonl_short(This.rpmtype)));
    end RPMType; 
    function Magic ( This : rpm_leader ) return String is 
        NON_RPM_EXCEPTION : exception;
    begin
        if htonl_magic(This.Magic) /= leader_magic then 
            Put_Line("Magic is not equals");
            raise NON_RPM_EXCEPTION; 
        end if; 
        return htonl_magic(This.Magic)'Img; 
    end Magic; 
end arpm_rpm_leaders; 

