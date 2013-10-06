with arpm_rpm_files; 
use arpm_rpm_files; 
with Ada.Text_IO; use Ada.Text_IO; 
with Internal_Codecs; use Internal_Codecs; 
with Ada.Command_Line; use Ada.Command_Line; 
procedure main is 
    file : rpm_file_Access; --  := Constructors.Create(String_To_Us("/tmp/test.rpm"));
begin
    for I in 1..Ada.Command_Line.Argument_Count loop
        file :=  Constructors.Create(String_To_Us(Ada.Command_Line.Argument(I)));
        file.parse; 
    end loop;
end main; 

