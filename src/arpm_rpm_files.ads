with League.String_Vectors; use League.String_Vectors;
with Ada.Streams; 
with League.Strings; use League.Strings; 
with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Streams.Stream_IO; 
package arpm_rpm_files is 
    type rpm_file is tagged limited private; 
    type rpm_file_access is access all rpm_file; 

    procedure Read_Leader(This : in out RPM_File); 
    procedure Read_Header ( This : in out RPM_File); 
    package Constructors is 
        function  Create(Filename : Universal_String) return rpm_file_access; 
        -- function  Create(Filename : String) return rpm_file_access is null; 
        -- function  Create(Filename : Unbounded_String) return rpm_file_access is null ; 
    end Constructors; 
    private
    type rpm_file is tagged limited record
        File_name : Universal_String; 
        File : Ada.Streams.Stream_IO.File_Type;
        Stream : Ada.Streams.Stream_IO.Stream_Access;
        Version : Universal_String; 
        Release: Universal_String; 
        Requires : Universal_String_Vector; 
        Provides : Universal_String_Vector; 
        License : Universal_String; 
    end record; 

end arpm_rpm_files; 

