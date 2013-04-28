with Interfaces.C; use Interfaces.C;
with Interfaces.C.Pointers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with League.Strings; use League.Strings;
with League.String_Vectors; use League.String_Vectors;
package ARPM_RPM_Internals is
    package chars_ptr_Pointers is
        new Interfaces.C.Pointers
            (Interfaces.C.size_t,
            Interfaces.C.Strings.chars_ptr,
            Interfaces.C.Strings.chars_ptr_array,
            Interfaces.C.Strings.Null_Ptr);
    --typedef struct{
    subtype Char_Star is chars_ptr_Pointers.Pointer;
    type Char_Star_Access is access all Char_Star;
    type My_RPM_Struct is limited record
            Error : Interfaces.C.int := 0;
            Name : Chars_Ptr;
            Version : Chars_Ptr;
            Release : Chars_Ptr;
            Depends_Count : Interfaces.C.Int := 0;
            Depend_On : char_star;
            Provides_Count : Interfaces.C.Int := 0;
            Provides : char_star;
    end record;
    type My_RPM_Struct_Access is access all My_RPM_Struct;

    type ARPM_RPM is record
        Id  : Integer   := Integer'Last;
        Name : Universal_String := Empty_Universal_String;
        Version : Universal_String := Empty_Universal_String;
        Release : Universal_String := Empty_Universal_String;
        Depend_On : Universal_String_Vector := Empty_Universal_String_Vector;
        Provides : Universal_String_Vector := Empty_Universal_String_Vector;
    end record;
    type ARPM_RPM_Access is access all ARPM_RPM;

end ARPM_RPM_Internals;
