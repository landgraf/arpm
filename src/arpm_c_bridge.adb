with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

package body ARPM_C_Bridge is

    package body Constructors is
        function Create(FileName : String)
            return My_RPM_Struct_Access is
            File_Name : chars_ptr := New_String(FileName);
            MyRPM : constant My_RPM_Struct_Access := new My_RPM_Struct;
        begin
            Parse_RPM(File_Name, MyRPM.all);
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

        procedure C_Free(R : in Char_Star);
            pragma Import(C, C_Free, "free");
        FREE_ERROR : exception;
    begin
        for I in 1..MyRPM.requires_Count loop
            chars_ptr_Pointers.Decrement(MyRPM.requires);
            chars_ptr_Pointers.Decrement(MyRPM.requires_Version);
            Free(MyRPM.requires.all);
            Free(MyRPM.requires_Version.all);
            if MyRPM.requires.all /= Null_Ptr or MyRPM.requires_Version.all /= Null_Ptr then
                raise FREE_ERROR;
            end if;
        end loop;
        C_Free(MyRPM.requires);
        C_Free(MyRPM.requires_Version);
        for I in 1..MyRPM.Provides_Count loop
            chars_ptr_Pointers.Decrement(MyRPM.Provides);
            chars_ptr_Pointers.Decrement(MyRPM.Provides_Version);
            Free(MyRPM.Provides.all);
            Free(MyRPM.Provides_Version.all);
            if MyRPM.Provides.all /= Null_Ptr or MyRPM.Provides_Version.all /= Null_Ptr then
                raise FREE_ERROR;
            end if;
        end loop;
        C_Free(MyRPM.Provides);
        C_Free(MyRPM.Provides_Version);
        Free(MyRPM.Name);
        Free(MyRPM.Version);
        Free(MyRPM.Epoch);
        Free(MyRPM.Release);
        Free(MyRPM.Arch);
        Free(MyRPM.Summary);
        Free(MyRPM.License);
        Free(MyRPM.Description);
        Free(MyRPM.URL);
        Free(MyRPM.Vendor);
        Free_Ptr(MyRPM);
    exception
        when STORAGE_ERROR =>
            pragma Debug(Put_Line("Unable to free memory"));
        when FREE_ERROR =>
            pragma Debug(Put_Line("Unable to free C pointers"));
    end Free;

    function Convert(MyRPM : My_RPM_Struct_Access) return ARPM_RPM_Access is
        RPM : constant ARPM_RPM_Access := new ARPM_RPM;
        CONVERT_ERROR : exception;
    begin
        RPM.Name := To_Unbounded_String(Value(MyRPM.Name));
        RPM.Version := To_Unbounded_String(Value(MyRPM.Version));
        RPM.Epoch := To_Unbounded_String(Value(MyRPM.Epoch));
        RPM.Release := To_Unbounded_String(Value(MyRPM.Release));
        RPM.Arch   := To_Unbounded_String(Value(MyRPM.Arch));
        RPM.Summary := To_Unbounded_String(Value(MyRPM.Summary));
        RPM.Description   := To_Unbounded_String(Value(MyRPM.Description));
        RPM.URL   := To_Unbounded_String(Value(MyRPM.URL));
        RPM.Vendor  := To_Unbounded_String(Value(MyRPM.Vendor));
        RPM.License   := To_Unbounded_String(Value(MyRPM.License));
        for I in 1..MyRPM.requires_Count loop
            RPM.requires.Append(To_Unbounded_String(Value(MyRPM.requires.all)));
            RPM.requires_Version.Append (To_Unbounded_String(Value(MyRPM.requires_Version.all)));
            chars_ptr_Pointers.Increment(MyRPM.requires);
            chars_ptr_Pointers.Increment(MyRPM.requires_Version);
        end loop;
        for I in 1..MyRPM.Provides_Count loop
            RPM.Provides.Append(To_Unbounded_String(Value(MyRPM.Provides.all)));
            RPM.Provides_Version.Append(To_Unbounded_String(Value(MyRPM.Provides_Version.all)));
            chars_ptr_Pointers.Increment(MyRPM.Provides);
            chars_ptr_Pointers.Increment(MyRPM.Provides_Version);
        end loop;
        return RPM;
    exception
        when others =>
            raise CONVERT_ERROR;
    end Convert;

end ARPM_C_Bridge;
