with Arpm_Rpm_Files;
use Arpm_Rpm_Files;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
procedure main is
   file : Rpm_File_Access; --  := Constructors.Create(String_To_Us("/tmp/test.rpm"));
begin
   for I in 1..Ada.Command_Line.Argument_Count loop
     file :=  Constructors.Create(Ada.Command_Line.Argument(I));
     file.Parse;
     Close(file);
   end loop;
end main;

