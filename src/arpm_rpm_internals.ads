with Interfaces.C; use Interfaces.C;
with Ada.Strings. Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Pointers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
package ARPM_RPM_Internals is
    package ARPM_Vector_Container_Package is new Ada.Containers.Vectors(
        Element_Type => Unbounded_String, Index_Type => Positive
        );
    subtype ARPM_Vector_Container is ARPM_Vector_Container_Package.Vector;

    type SHA256S is array (1..64) of Character;
    package ARPM_Map_Container_Package is new Ada.Containers.Ordered_Sets(
        Element_Type => SHA256S);
    subtype ARPM_Osets_Container is ARPM_Map_Container_Package.Set;

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
        --   time_file INTEGER,  time_build INTEGER,  rpm_group TEXT,  rpm_buildhost TEXT,  rpm_sourcerpm TEXT,  
        --   rpm_header_start INTEGER,  rpm_header_end INTEGER,  
        --   rpm_packager TEXT,  size_package INTEGER,  size_installed INTEGER,  size_archive INTEGER,  location_href TEXT,  location_base TEXT,  
        --   checksum_type TEXT);" );
            Error : Interfaces.C.int := 0;
            Name : Chars_Ptr;
            Version : Chars_Ptr;
            Epoch : Chars_Ptr;
            Release : Chars_Ptr;
            Arch : Chars_Ptr;
            Summary: Chars_Ptr;
            Description : Chars_Ptr;
            Url : Chars_Ptr;
            license : Chars_Ptr;
            Vendor : Chars_Ptr;
            requires_Count : Interfaces.C.Int := 0;
            requires : char_star;
            requires_version : char_star;
            Provides_Count : Interfaces.C.Int := 0;
            Provides : char_star;
            Provides_version : char_star;
    end record;
    type My_RPM_Struct_Access is access all My_RPM_Struct;

    type ARPM_RPM is record
        Id  : Integer   := Integer'Last;
        Name : Unbounded_String := Null_Unbounded_String;
        Version : Unbounded_String := Null_Unbounded_String;
        epoch : Unbounded_String := Null_Unbounded_String;
        Release : Unbounded_String := Null_Unbounded_String;
        Arch   : Unbounded_String := Null_Unbounded_String;
        Summary : Unbounded_String := Null_Unbounded_String;
        Description   : Unbounded_String := Null_Unbounded_String;
        Url   : Unbounded_String := Null_Unbounded_String;
        Vendor : Unbounded_String := Null_Unbounded_String;
        License : Unbounded_String := Null_Unbounded_String;
        requires : ARPM_Vector_Container;
        requires_version : ARPM_Vector_Container; 
        Provides : ARPM_Vector_Container; 
        Provides_version : ARPM_Vector_Container;
    end record;
    type ARPM_RPM_Access is access all ARPM_RPM;

    procedure Free is new Ada.Unchecked_Deallocation
        (Object => ARPM_RPM,
         Name   => ARPM_RPM_Access);


end ARPM_RPM_Internals;
