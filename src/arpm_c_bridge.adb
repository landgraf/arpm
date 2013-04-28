with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with ARPM_Files_Handlers;
with League.Strings; 
with League.String_Vectors;
with Internal_Codecs;

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
        procedure Free_Ptr is new Ada.Unchecked_Deallocation
            (Object => My_RPM_Struct,
             Name   => My_RPM_Struct_Access);

        procedure C_Free(MyRPM : in Char_Star);
            pragma Import(C, C_Free, "free");
        FREE_ERROR : exception;
    begin
        for I in 1..MyRPM.Depends_Count loop
            chars_ptr_Pointers.Decrement(MyRPM.Depend_On);
            Free(MyRPM.Depend_On.all);
            if MyRPM.Depend_On.all /= Null_Ptr then 
                raise FREE_ERROR;
            end if;
        end loop;
        C_Free(MyRPM.Depend_On);
        for I in 1..MyRPM.Provides_Count loop
            chars_ptr_Pointers.Decrement(MyRPM.Provides);
            Free(MyRPM.Provides.all);
            if MyRPM.Provides.all /= Null_Ptr then 
                raise FREE_ERROR;
            end if;
        end loop;
        C_Free(MyRPM.Provides);
        Free(MyRPM.Name);
        Free(MyRPM.Version);
        Free(MyRPM.Release);
        Free_Ptr(MyRPM);
    exception
        when STORAGE_ERROR =>
            Put_Line("Unable to free memory");
        when FREE_ERROR => 
            Put_Line("Unable to free C pointers");
    end Free;

    function Convert(MyRPM : in out My_RPM_Struct_Access) return ARPM_RPM_Access is 
        RPM : ARPM_RPM_Access := new ARPM_RPM;
        use Internal_Codecs;
    begin
        RPM.Name := String_To_US(Value(MyRPM.Name));
        RPM.Version := String_To_US(Value(MyRPM.Version));
        RPM.Release := String_To_US(Value(MyRPM.Release));
        for I in 1..MyRPM.Depends_Count loop
            pragma Debug(Put_Line("Depends on:" & Value(MyRPM.Depend_On.all)));
            RPM.Depend_On.Append(String_To_US(Value(MyRPM.Depend_On.all)));
            chars_ptr_Pointers.Increment(MyRPM.Depend_On);
        end loop;
        for I in 1..MyRPM.Provides_Count loop
            pragma Debug(Put_Line("Provides:" & Value(MyRPM.Provides.all)));
            RPM.Provides.Append(String_To_US(Value(MyRPM.Provides.all)));
            chars_ptr_Pointers.Increment(MyRPM.Provides);
        end loop;
        return RPM;
    end Convert;

    procedure Convert(RPM : out ARPM_RPM_Access; MyRPM : in out My_RPM_Struct_Access) is
        use Internal_Codecs;
    begin
        RPM.Name := String_To_US(Value(MyRPM.Name));
        RPM.Version := String_To_US(Value(MyRPM.Version));
        RPM.Release := String_To_US(Value(MyRPM.Release));
        for I in 1..MyRPM.Depends_Count loop
            pragma Debug(Put_Line("Depends on:" & Value(MyRPM.Depend_On.all)));
            RPM.Depend_On.Append(String_To_US(Value(MyRPM.Depend_On.all)));
            chars_ptr_Pointers.Increment(MyRPM.Depend_On);
        end loop;
        for I in 1..MyRPM.Provides_Count loop
            pragma Debug(Put_Line("Provides:" & Value(MyRPM.Provides.all)));
            RPM.Provides.Append(String_To_US(Value(MyRPM.Provides.all)));
            chars_ptr_Pointers.Increment(MyRPM.Provides);
        end loop;
    end Convert;

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
            Put_Line("Exception in test");
            Error := 2;
            return;
    end Test;

end ARPM_C_Bridge;
