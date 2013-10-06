with Interfaces.C; use Interfaces.C;
package arpm_rpm_leaders is
    type rpm_leader is private;
    type rpm_leader_access is access all rpm_leader;

    type rpmtypes is (Binary, Source);
    for rpmtypes use (Binary => 0, Source => 1);

    function Name(This : rpm_leader) return String;

    -- getter for RPMType
    function RPMType ( This : rpm_leader) return rpmtypes;

    -- Check if Magic is RPM Magic or raise exception
    function Magic ( This : rpm_leader ) return String;
    private

    type Magic_type is range 0..2**(4*8)-1;
    for Magic_type'size use 4*8;
    LEADER_Magic : constant Magic_type := 16#edabeedb#;
    type two_bytes_number is range 0..2**(2*8)-1;
    for two_bytes_number'Size use 2*8;

    type rpm_leader is record
        Magic : Magic_type;
        major : unsigned_char;
        minor : unsigned_char;
        -- rpmtype : two_bytes_number;
        rpmtype : two_bytes_number;
        archnum : two_bytes_number;
        Name : char_array(1..66);
        osnum : two_bytes_number;
        signature_type : two_bytes_number;
        reserved : char_array(1..16);
    end record;
    for rpm_leader use record
         Magic at 0 range 0..31;
         major at 4 range 0..7;
         minor at 4 range 8..15;
         rpmtype at 6 range 0..15;
         archnum at 8 range 0..15;
         Name at 10 range 0..66*8-1;
         osnum at 76  range 0..15;
         signature_type at 78 range 0..15;
         reserved at 80 range 0..127;
    end record;
    for rpm_leader'Size use 96*8;

end arpm_rpm_leaders;

