with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Interfaces.C.Pointers;
with Interfaces.C;  use Interfaces.C;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
package ARPM_C_Bridge is 

    procedure Parse_RPM(Filename : in Chars_Ptr; MyRPM : out My_RPM_Struct);
    pragma Import(C, Parse_RPM, "parse_rpm");
    function Read_Config return Interfaces.C.Int; 
    pragma Import(C, Read_Config, "read_config");
    function Test(Filename : String) return Integer;

    procedure Free(MyRPM : in out My_RPM_Struct); 

end ARPM_C_Bridge;
