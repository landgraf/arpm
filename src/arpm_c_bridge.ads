with Interfaces.C.Strings;  use Interfaces.C.Strings;
package ARPM_C_Bridge is 
    type ARPM_C is record
        errno : Integer := 0; 
        pkg_name : chars_ptr := Null_Ptr;
        pkg_version : chars_ptr := Null_Ptr;
        pkg_release : chars_ptr := Null_Ptr;
    end record;
    procedure Parse_RPM(filename : in Chars_ptr; MyRPM : out ARPM_C);
         pragma Import (C, Parse_RPM, "parse_rpm");
    function Print(FileName : String) return String ;
end ARPM_C_Bridge;
