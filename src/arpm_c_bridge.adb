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

    function Test(Filename : String) return Integer is 
        File_Name : Chars_Ptr := New_String(Filename);
        MyRPM : My_RPM_Struct;
        Element : Chars_Ptr;
    begin
        Parse_RPM(File_name, MyRPM);
        if MyRPM.Error /= 0 then
            return 1; 
        end if;
        Put_Line("Name: " & Value(MyRPM.Name) & "  ; Version:" & Value(MyRPM.Version) & "  ; Release:" & Value(MyRPM.Release));
        Put_Line("Dependency count:" & MyRPM.Depends_Count'Img);
        for I in 1..MyRPM.Depends_Count loop
            Put_Line("Depends on:" & Value(MyRPM.Depend_On.all));
            chars_ptr_Pointers.Increment(MyRPM.Depend_On);
        end loop;
        for I in 1..MyRPM.Provides_Count loop
            Put_Line("Provides:" & Value(MyRPM.Provides.all));
            chars_ptr_Pointers.Increment(MyRPM.Provides);
        end loop;
        Free(File_Name);
        Free(MyRPM); 
        return 0;
    end Test;
end ARPM_C_Bridge;
