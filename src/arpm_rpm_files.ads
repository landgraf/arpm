with Ada.Streams;
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO;
with arpm_rpm_rpmhdrIndexs; use arpm_rpm_rpmhdrIndexs;
with arpm_rpm_depends;  use arpm_rpm_depends;
with Ada.Unchecked_Deallocation;

package Arpm_Rpm_Files is
    type Rpm_File is Tagged limited private;
    type Rpm_File_Access is access all Rpm_File;

    procedure Parse(This : in out Rpm_File);
    package Constructors is
        -- function  Create(FileName : Unbounded_String) return Rpm_File_Access;
        function  Create(FileName : String) return Rpm_File_Access;
        -- function  Create(FileName : Unbounded_String) return Rpm_File_Access is null ;
    end Constructors;
    procedure Free(This : in out Rpm_File_Access);
    private

    INVALID_Format_EXCEPTION : exception;
    type Index_array is array (Positive range <>) of rpmhdrIndex;
    type Index_array_access is access all Index_array;
    procedure Read_Leader(This : in out Rpm_File);
    procedure Read_Header (This : in out Rpm_File; Signature : Boolean := False);
    function Read_Indexes(This : in out Rpm_File; count : in Integer) return Index_array_access;
    procedure Read_Payload(This: in out Rpm_File; Indexes : in Index_array_access);
    -- procedure Read_HdrIndex(This : in out Rpm_File);
    type Rpm_File is Tagged limited record
        File : Ada.Streams.Stream_IO.File_Type;
        Stream : Ada.Streams.Stream_IO.Stream_Access;
        Offset: Integer := 0 ;
        Indexes : Index_array_access;

        -- FILE
        Name : Unbounded_String;
        Version : Unbounded_String;
        Release: Unbounded_String;
        Build_Host : Unbounded_String;
        Summary : Unbounded_String;
        Description : Unbounded_String;
        Build_Time : Long_Long_Integer := 0;
        Size : Long_Long_Integer := 0;
        License : Unbounded_String;
        Distribution : Unbounded_String;
        Vendor : Unbounded_String;
        Group : Unbounded_String;
        PAYLOADFlags : Unbounded_String;
        PAYLOADCOMPRESSOR : Unbounded_String;
        PAYLOADFormat : Unbounded_String;
        packager : Unbounded_String;
        cookie : Unbounded_String;
        optFlags : Unbounded_String;
        postunprog : Unbounded_String;
        postinprog : Unbounded_String;
        postin : Unbounded_String;
        postun : Unbounded_String;
        Arch : Unbounded_String;
        OS : Unbounded_String;
        URL : Unbounded_String;
        SRPM : Unbounded_String;
        RPM_Version : Unbounded_String;
        Platform : Unbounded_String;
        Requires : arpm_rpm_depends.rpm_depends_access;
        Provides : arpm_rpm_depends.rpm_depends_access;
    end record;


    procedure Free_Indexes is new Ada.Unchecked_Deallocation
        (Object => Index_array,
        Name   => Index_array_access);
    procedure Free_RPM is new Ada.Unchecked_Deallocation
        (Object => Rpm_File,
        Name   => Rpm_File_Access);
end Arpm_Rpm_Files;

