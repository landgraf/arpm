with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with ARPM_Files_Handlers;
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
                pragma Debug(Put_Line("Unknown error: arpm_c_bridge"));
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
            chars_ptr_Pointers.Decrement(MyRPM.requires_version);
            Free(MyRPM.requires.all);
            Free(MyRPM.requires_version.all);
            if MyRPM.requires.all /= Null_Ptr or MyRPM.requires_version.all /= Null_Ptr then 
                raise FREE_ERROR;
            end if;
        end loop;
        C_Free(MyRPM.requires);
        C_Free(MyRPM.requires_version);
        for I in 1..MyRPM.Provides_Count loop
            chars_ptr_Pointers.Decrement(MyRPM.Provides);
            chars_ptr_Pointers.Decrement(MyRPM.Provides_version);
            Free(MyRPM.Provides.all);
            Free(MyRPM.Provides_version.all);
            if MyRPM.Provides.all /= Null_Ptr or MyRPM.Provides_Version.all /= Null_Ptr then 
                raise FREE_ERROR;
            end if;
        end loop;
        C_Free(MyRPM.Provides);
        C_Free(MyRPM.Provides_version);
        Free(MyRPM.Name);
        Free(MyRPM.Version);
        Free(MyRPM.Release);
        Free(MyRPM.Arch);
        Free_Ptr(MyRPM);
    exception
        when STORAGE_ERROR =>
            pragma Debug(Put_Line("Unable to free memory"));
        when FREE_ERROR => 
            pragma Debug(Put_Line("Unable to free C pointers"));
    end Free;

    function Convert(MyRPM : My_RPM_Struct_Access) return ARPM_RPM_Access is
        RPM : ARPM_RPM_Access := new ARPM_RPM;
        CONVERT_ERROR : exception;
    begin
        RPM.Name := To_Unbounded_String(Value(MyRPM.Name));
        RPM.Version := To_Unbounded_String(Value(MyRPM.Version));
        RPM.Release := To_Unbounded_String(Value(MyRPM.Release));
        RPM.Arch   := To_Unbounded_String(Value(MyRPM.Arch));
        for I in 1..MyRPM.requires_Count loop
            RPM.requires.Append(To_Unbounded_String(Value(MyRPM.requires.all)));
            RPM.requires_version.Append (To_Unbounded_String(Value(MyRPM.requires_version.all)));
            chars_ptr_Pointers.Increment(MyRPM.requires);
            chars_ptr_Pointers.Increment(MyRPM.requires_version);
        end loop;
        for I in 1..MyRPM.Provides_Count loop
            RPM.Provides.Append(To_Unbounded_String(Value(MyRPM.Provides.all)));
            RPM.Provides_version.Append(To_Unbounded_String(Value(MyRPM.Provides_Version.all)));
            chars_ptr_Pointers.Increment(MyRPM.Provides);
            chars_ptr_Pointers.Increment(MyRPM.Provides_version);
        end loop;
        return RPM;
    exception
        when others =>
            raise CONVERT_ERROR;
    end Convert;

end ARPM_C_Bridge;
