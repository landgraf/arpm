with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
package ARPM_RPM_Internals is
    package chars_ptr_Pointers is
        new Interfaces.C.Pointers
            (Interfaces.C.size_t,
            Interfaces.C.Strings.chars_ptr,
            Interfaces.C.Strings.chars_ptr_array,
            Interfaces.C.Strings.Null_Ptr);
    --typedef struct{
    subtype Char_Star is chars_ptr_Pointers.Pointer;
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
            Depend_On : char_star;
            --    int provides_cound;
            Provides_Count : Interfaces.C.Int := 0;
            --    char** provides;
            --    FIXME
            Provides : char_star;
    end record;
    type My_RPM_Struct_Access is access all My_RPM_Struct;

end ARPM_RPM_Internals;
