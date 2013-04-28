with Interfaces.C.Strings; use Interfaces.C.Strings;
with Internal_Codecs; use Internal_Codecs;
with Ada.Exceptions;
with Ada.Text_Io; use Ada.Text_IO;
with League.Strings; use League.Strings;
with League.String_Vectors; use League.String_Vectors;

package body ARPM_DB_Containers is 
    procedure Save(MyRPM : in out ARPM_RPM_Access; DB : in Session_Type) is 
        RPM_Record : Detached_RPM'Class := New_RPM;
    begin
        RPM_Record.Set_Release(US_To_String(MyRPM.Release));
        RPM_Record.Set_Name(US_To_String(MyRPM.Name));
        RPM_Record.Set_Version(US_To_String(MyRPM.Version));
        -- Put_Line("New generated for package " & US_To_String(MyRPM.Name) & "  is " & RPM_Record.Id'Img);
        DB.Persist(RPM_Record);
        DB.Commit;
        MyRPM.Id := RPM_Record.Id;
    exception
        when The_Event: others =>
            --Put_Line("Failed to save  " & US_To_String(MyRPM.Name));
            Put_Line("Reason:" & Ada.Exceptions.Exception_Message(The_Event));
    end Save;
    procedure Save_Depends(MYRPM : in out ARPM_RPM_Access; DB : in Session_Type) is
    begin
        for I in 1..Length(MyRPM.Depend_On) loop
            declare
                Depend : Detached_Requires'Class := New_Requires; 
            begin
                Depend.Set_PkgKey(MyRPM.Id);
                Depend.Set_Name(US_To_String(MyRPM.Depend_On.Element(I)));
                DB.Persist(Depend);
            end;
        end loop;
        DB.Commit;
    end Save_Depends;
    procedure Save_Provides(MYRPM : in out ARPM_RPM_Access; DB : in Session_Type) is
    begin
        for I in 1..Length(MyRPM.Provides) loop
            declare
                Provides : Detached_Provides'Class := New_Provides; 
            begin
                Provides.Set_PkgKey(MyRPM.Id);
                Provides.Set_Name(US_To_String(MyRPM.Provides.Element(I)));
                DB.Persist(Provides);
            end;
        end loop;
        DB.Commit;
    end Save_Provides;
end ARPM_DB_Containers; 
