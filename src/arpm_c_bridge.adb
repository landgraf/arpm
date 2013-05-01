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
        for I in 1..MyRPM.requires_Count loop
            chars_ptr_Pointers.Decrement(MyRPM.requires);
            Free(MyRPM.requires.all);
            if MyRPM.requires.all /= Null_Ptr then 
                raise FREE_ERROR;
            end if;
        end loop;
        C_Free(MyRPM.requires);
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

    procedure Convert(RPM : out ARPM_RPM_Access; MyRPM : in out My_RPM_Struct_Access) is
        use Internal_Codecs;
    begin
        RPM.Name := String_To_US(Value(MyRPM.Name));
        RPM.Version := String_To_US(Value(MyRPM.Version));
        RPM.Release := String_To_US(Value(MyRPM.Release));
        for I in 1..MyRPM.requires_Count loop
            RPM.requires.Append(String_To_US(Value(MyRPM.requires.all)));
            chars_ptr_Pointers.Increment(MyRPM.requires);
        end loop;
        for I in 1..MyRPM.Provides_Count loop
            RPM.Provides.Append(String_To_US(Value(MyRPM.Provides.all)));
            chars_ptr_Pointers.Increment(MyRPM.Provides);
        end loop;
    end Convert;

end ARPM_C_Bridge;
