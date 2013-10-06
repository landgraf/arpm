with Ada.Text_IO; use Ada.Text_IO;
package body arpm_Rpm_Leaders is

    -- procedure htonl(Number : in short; Result : out short);
    function htonl_short(Number : in short) return short;
        pragma Import(C, htonl_short, "htons");
    function htonl_2bn(Number : in two_bytes_number) return two_bytes_number;
        pragma Import(C, htonl_2bn, "htons");
    function htonl_uchar(Number : in unsigned_char) return unsigned_char;
        pragma Import(C, htonl_uchar, "htonl");
    function htonl_Magic(Number : in Magic_type) return Magic_type;
        pragma Import(C, htonl_Magic, "htonl");

    function Name(This : Rpm_Leader) return String is
    begin
        return To_Ada(This.Name);
    end Name;
    function Rpm_Type ( This : Rpm_Leader) return Rpm_Types is
    begin
        return Rpm_Types'Val(Integer(htonl_2bn(This.Rpm_Type)));
    end Rpm_Type;
    function Magic ( This : Rpm_Leader ) return String is
        NON_RPM_EXCEPTION : exception;
    begin
        if htonl_Magic(This.Magic) /= Leader_Magic then
            Put_Line("Magic is not equals");
            raise NON_RPM_EXCEPTION;
        end if;
        return htonl_Magic(This.Magic)'Img;
    end Magic;
end arpm_Rpm_Leaders;

