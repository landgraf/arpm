with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
package ARPM_RPM_Internals is
    --typedef struct{
    type My_RPM_Struct is record
            --int error;
            Error : Interfaces.C.int := 0;
            --    char* name;
            Name : Chars_Ptr;
            --    char* version;
            Version : Chars_Ptr;
            --    char* release;
            Release : Chars_Ptr;
            --    int depends_count;
            Depends_Count : Interfaces.C.Int := 0;
            --    char** depends_on;
            -- FIXME
            --    int provides_cound;
            Provides_Count : Interfaces.C.Int := 0;
            --    char** provides;
            --    FIXME
    end record;
    type My_RPM_Struct_Access is access all My_RPM_Struct;

end ARPM_RPM_Internals;
