with Interfaces.C.Strings;  use Interfaces.C.Strings;
with Interfaces.C;  use Interfaces.C;
with ARPM_RPM_internals; use ARPM_RPM_internals;
package ARPM_C_Bridge is
    package Constructors is
        function Create(FileName : String)
            return My_RPM_Struct_Access;
    end Constructors;

    procedure Parse_RPM(FileName : in chars_ptr; MyRPM : out My_RPM_Struct);
        pragma Import(C, Parse_RPM, "parse_rpm");

    function Read_Config return Interfaces.C.int;
        pragma Import(C, Read_Config, "read_config");

    procedure Free_Config;
        pragma Import(C, Free_Config, "free_config");


    procedure Free(MyRPM : in out My_RPM_Struct_Access);

    function Convert(MyRPM : My_RPM_Struct_Access) return ARPM_RPM_Access;

end ARPM_C_Bridge;
