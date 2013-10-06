with Interfaces.C; use Interfaces.C;
with Ada.Strings. Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Interfaces.C.Pointers;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Containers.Vectors;
with Ada.Containers.Ordered_Sets;
package ARPM_RPM_internals is
    package ARPM_Vector_Container_Package is new Ada.Containers.Vectors(
        Element_Type => Unbounded_String, Index_Type => Positive
        );
    subtype ARPM_Vector_Container is ARPM_Vector_Container_Package.Vector;

    type SHA256S is array (1..64) of Character;
    package ARPM_Map_Container_Package is new Ada.Containers.Ordered_Sets(
        Element_Type => SHA256S);
    subtype Arpm_Osets_Container is ARPM_Map_Container_Package.Set;

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
        --   time_file Integer,  time_build Integer,  rpm_group TEXT,  rpm_buildhost TEXT,  rpm_sourcerpm TEXT,
        --   Rpm_Header_start Integer,  Rpm_Header_end Integer,
        --   rpm_packager TEXT,  size_package Integer,  size_installed Integer,  size_archive Integer,  location_href TEXT,  location_base TEXT,
        --   checksum_type TEXT);" );
            Error : Interfaces.C.int := 0;
            Name : chars_ptr;
            Version : chars_ptr;
            Epoch : chars_ptr;
            Release : chars_ptr;
            Arch : chars_ptr;
            Summary: chars_ptr;
            Description : chars_ptr;
            URL : chars_ptr;
            License : chars_ptr;
            Vendor : chars_ptr;
            requires_Count : Interfaces.C.int := 0;
            requires : Char_Star;
            requires_Version : Char_Star;
            Provides_Count : Interfaces.C.int := 0;
            Provides : Char_Star;
            Provides_Version : Char_Star;
    end record;
    type My_RPM_Struct_Access is access all My_RPM_Struct;

    type ARPM_RPM is record
        Id  : Integer   := Integer'Last;
        Name : Unbounded_String := Null_Unbounded_String;
        Version : Unbounded_String := Null_Unbounded_String;
        Epoch : Unbounded_String := Null_Unbounded_String;
        Release : Unbounded_String := Null_Unbounded_String;
        Arch   : Unbounded_String := Null_Unbounded_String;
        Summary : Unbounded_String := Null_Unbounded_String;
        Description   : Unbounded_String := Null_Unbounded_String;
        URL   : Unbounded_String := Null_Unbounded_String;
        Vendor : Unbounded_String := Null_Unbounded_String;
        License : Unbounded_String := Null_Unbounded_String;
        requires : ARPM_Vector_Container;
        requires_Version : ARPM_Vector_Container;
        Provides : ARPM_Vector_Container;
        Provides_Version : ARPM_Vector_Container;
    end record;
    type ARPM_RPM_Access is access all ARPM_RPM;

    procedure Free is new Ada.Unchecked_Deallocation
        (Object => ARPM_RPM,
         Name   => ARPM_RPM_Access);


end ARPM_RPM_internals;
