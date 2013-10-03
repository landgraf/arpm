with System; use System; 
with Interfaces.C; use Interfaces.C; 
package arpm_rpm_leaders is 
    type rpm_leader is private; 
    type rpm_leader_access is access all rpm_leader; 

    type rpmtypes is (Binary, Source); 
    for rpmtypes use (Binary => 0, Source => 1);

    function Name(This : rpm_leader) return String; 
    function RPMType ( This : rpm_leader) return rpmtypes; 
    -- TODO  We have to check magic number here
    -- NOT IMPLEMENTED
    function Magic ( This : rpm_leader ) return String; 
    private 

    type magic_type is new unsigned_char; 
    for magic_type'size use 4*8;
    type rpm_leader is record
        magic : magic_type; 
        major : unsigned_char; 
        minor : unsigned_char; 
        rpmtype : short; 
        archnum : short; 
        name : char_array(1..66); 
        osnum : short; 
        signature_type : short; 
        reserver : char_array(1..16);
    end record;
     pragma Warnings (Off);
    for rpm_leader'Bit_Order use High_Order_First;
     pragma Warnings (On);

end arpm_rpm_leaders; 

