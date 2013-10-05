with League.String_Vectors; use League.String_Vectors;
with Ada.Streams; 
with League.Strings; use League.Strings; 
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; 
with arpm_rpm_rpmhdrindexs; use arpm_rpm_rpmhdrindexs; 
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
        -- File_name : Universal_String; 
        File : Ada.Streams.Stream_IO.File_Type;
        Stream : Ada.Streams.Stream_IO.Stream_Access;
        offset: Long_Long_Integer := 0 ; 
        Name : Universal_String; 
        Version : Universal_String; 
        Release: Universal_String; 
        Build_Host : Universal_String;
        Summary : Universal_String;
        Description : Universal_String;
        Build_Time : LOng_Long_Integer := 0;
        Requires : Universal_String_Vector; 
        Provides : Universal_String_Vector; 
        License : Universal_String; 
        indexes : index_array_access;
    end record; 

end arpm_rpm_files; 

