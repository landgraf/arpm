with Ada.Streams; 
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; 
with arpm_rpm_rpmhdrindexs; use arpm_rpm_rpmhdrindexs; 
with arpm_rpm_depends;  use arpm_rpm_depends;
with Ada.Unchecked_Deallocation;

package arpm_rpm_files is 
    type rpm_file is tagged limited private; 
    type rpm_file_access is access all rpm_file; 

    procedure Parse(This : in out RPM_File);
    package Constructors is 
        -- function  Create(Filename : Unbounded_String) return rpm_file_access; 
        function  Create(Filename : String) return rpm_file_access; 
        -- function  Create(Filename : Unbounded_String) return rpm_file_access is null ; 
    end Constructors; 
    procedure Free(This : in out RPM_File_Access); 
    private

    INVALID_FORMAT_EXCEPTION : exception; 
    type index_array is array (Positive range <>) of rpmhdrindex; 
    type index_array_access is access all index_array; 
    procedure Read_Leader(This : in out RPM_File); 
    procedure Read_Header (This : in out RPM_File; Signature : Boolean := False); 
    function Read_Indexes(This : in out RPM_File; count : in Integer) return index_array_access;
    procedure Read_Payload(This: in out RPM_File; indexes : in index_array_access); 
    -- procedure Read_Hdrindex(This : in out RPM_File); 
    type rpm_file is tagged limited record
        File : Ada.Streams.Stream_IO.File_Type;
        Stream : Ada.Streams.Stream_IO.Stream_Access;
        offset: Integer := 0 ; 
        indexes : index_array_access;

        -- FILE
        Name : Unbounded_String; 
        Version : Unbounded_String; 
        Release: Unbounded_String; 
        Build_Host : Unbounded_String;
        Summary : Unbounded_String;
        Description : Unbounded_String;
        Build_Time : LOng_Long_Integer := 0;
        Size : LOng_Long_Integer := 0;
        License : Unbounded_String; 
        Distribution : Unbounded_String;
        Vendor : Unbounded_String;
        Group : Unbounded_String;
        PAYLOADFLAGS : Unbounded_String;
        PAYLOADCOMPRESSOR : Unbounded_String;
        PAYLOADFORMAT : Unbounded_String;
        packager : Unbounded_String;
        cookie : Unbounded_String;
        optflags : Unbounded_String;
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
        provides : arpm_rpm_depends.rpm_depends_access; 
    end record; 


    procedure Free_Indexes is new Ada.Unchecked_Deallocation
        (Object => index_array,
        Name   => index_array_access);
    procedure Free_RPM is new Ada.Unchecked_Deallocation
        (Object => rpm_file,
        Name   => rpm_file_access);
end arpm_rpm_files; 

