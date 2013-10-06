with League.String_Vectors; use League.String_Vectors;
with Ada.Streams; 
with League.Strings; use League.Strings; 
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; 
with arpm_rpm_rpmhdrindexs; use arpm_rpm_rpmhdrindexs; 
with arpm_rpm_depends;  use arpm_rpm_depends;
package arpm_rpm_files is 
    type rpm_file is tagged limited private; 
    type rpm_file_access is access all rpm_file; 

    procedure Parse(This : in out RPM_File);
    package Constructors is 
        function  Create(Filename : Universal_String) return rpm_file_access; 
        -- function  Create(Filename : String) return rpm_file_access is null; 
        -- function  Create(Filename : Unbounded_String) return rpm_file_access is null ; 
    end Constructors; 
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
        Name : Universal_String; 
        Version : Universal_String; 
        Release: Universal_String; 
        Build_Host : Universal_String;
        Summary : Universal_String;
        Description : Universal_String;
        Build_Time : LOng_Long_Integer := 0;
        Size : LOng_Long_Integer := 0;
        License : Universal_String; 
        Distribution : Universal_String;
        Vendor : Universal_String;
        Group : Universal_String;
        PAYLOADFLAGS : Universal_String;
        PAYLOADCOMPRESSOR : Universal_String;
        PAYLOADFORMAT : Universal_String;
        packager : Universal_String;
        cookie : Universal_String;
        optflags : Universal_String;
        postunprog : Universal_String;
        postinprog : Universal_String;
        postin : Universal_String;
        postun : Universal_String; 
        Arch : Universal_String;
        OS : Universal_String;
        URL : Universal_String;
        SRPM : Universal_String;
        RPM_Version : Universal_String;
        Platform : Universal_String;
        Requires : arpm_rpm_depends.rpm_depends_access; 
        provides : arpm_rpm_depends.rpm_depends_access; 
    end record; 

end arpm_rpm_files; 

