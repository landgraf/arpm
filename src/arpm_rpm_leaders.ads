with System; use System; 
with Interfaces.C; use Interfaces.C; 
package arpm_rpm_leaders is 
    type rpm_leader is private; 
    type rpm_leader_access is access all rpm_leader; 

    type rpmtypes is (Binary, Source); 
    for rpmtypes use (Binary => 0, Source => 1);

    function Name(This : rpm_leader) return String; 

    -- getter for RPMType 
    function RPMType ( This : rpm_leader) return rpmtypes; 

    -- Check if magic is RPM magic or raise exception
    function Magic ( This : rpm_leader ) return String; 
    private 

    type magic_type is range 0..2**(4*8)-1; 
    for magic_type'size use 4*8;
    leader_magic : constant magic_type := 16#edabeedb#;
    type two_bytes_number is range 0..2**(2*8)-1; 
    for two_bytes_number'Size use 2*8; 

    type rpm_leader is record
        magic : magic_type; 
        major : unsigned_char; 
        minor : unsigned_char; 
        -- rpmtype : two_bytes_number; 
        rpmtype : short; 
        archnum : short; 
        name : char_array(1..66); 
        osnum : short; 
        signature_type : short; 
        reserver : char_array(1..16);
    end record;
    -- for rpm_leader'Bit_Order use High_Order_First;

end arpm_rpm_leaders; 

