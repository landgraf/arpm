with arpm_rpm_files; 
use arpm_rpm_files; 
with Ada.Text_IO; use Ada.Text_IO; 
with Internal_Codecs; use Internal_Codecs; 
procedure main is 
    file : rpm_file_Access := Constructors.Create(String_To_Us("/tmp/test.rpm"));
begin
    file.parse; 
end main; 

