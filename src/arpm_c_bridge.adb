with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with ARPM_Files_Handlers;

package body ARPM_C_Bridge is 

    procedure Free(MyRPM : in out My_RPM_Struct) is 
    begin
        Free(MyRPM.Name);
        Free(MyRPM.Version);
        Free(MyRPM.Release);
    exception
        when STORAGE_ERROR =>
            Put_Line("Unable to free memory");
    end Free;

    function Test(Filename : String) return String is 
        File_Name : Chars_Ptr := New_String(Filename);
        MyRPM : My_RPM_Struct;
    begin
        Parse_RPM(File_name, MyRPM);
        if MyRPM.Error /= 0 then
            return "False"; 
        end if;
        Put_Line("Name: " & Value(MyRPM.Name) & "  ; Version:" & Value(MyRPM.Version) & "  ; Release:" & Value(MyRPM.Release));
        Put_Line("Dependency count:" & MyRPM.Depends_Count'Img);
        Free(File_Name);
        Free(MyRPM); 
        return "Good";
    end Test;
end ARPM_C_Bridge;
