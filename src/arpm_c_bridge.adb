with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with ARPM_Files_Handlers;

package body ARPM_C_Bridge is 

    package body Constructors is 
        function Create(Filename : String) 
            return My_RPM_Struct_Access is 
            
            File_Name : Chars_Ptr := New_String(Filename);
            MyRPM : My_RPM_Struct_Access := new My_RPM_Struct;
        begin
            Parse_RPM(File_name, MyRPM.all);
            Free(File_Name);
            return MyRPM; 
        exception 
            when others => 
                Put_Line("Fuck");
                MyRPM.Error := 77;
                return MyRPM;
        end  Create;
    end Constructors;
    procedure Free(MyRPM : in out My_RPM_Struct_Access) is 
    begin
        Free(MyRPM.Name);
        Free(MyRPM.Version);
        Free(MyRPM.Release);
    exception
        when STORAGE_ERROR =>
            Put_Line("Unable to free memory");
    end Free;

    procedure Test(MyRPM : in out My_RPM_Struct_Access; Error : out Integer) is 
        Element : Chars_Ptr;
    begin
        if MyRPM.Error /= 0 then
            Error :=  1; 
            return;
        end if;
        pragma Debug(Put_Line("Name: " & Value(MyRPM.Name) & "  ; Version:" & Value(MyRPM.Version) & "  ; Release:" & Value(MyRPM.Release)));
        pragma Debug(Put_Line("Dependency count:" & MyRPM.Depends_Count'Img));
        for I in 1..MyRPM.Depends_Count loop
            pragma Debug(Put_Line("Depends on:" & Value(MyRPM.Depend_On.all)));
            chars_ptr_Pointers.Increment(MyRPM.Depend_On);
        end loop;
        for I in 1..MyRPM.Provides_Count loop
            pragma Debug(Put_Line("Provides:" & Value(MyRPM.Provides.all)));
            chars_ptr_Pointers.Increment(MyRPM.Provides);
        end loop;
        Error := 0;
    exception 
        when others => 
            Put_Line("Fuck");
            Error := 2;
            return;
    end Test;
end ARPM_C_Bridge;
