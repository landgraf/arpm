with Interfaces.C.Strings; use Interfaces.C.Strings;
with Internal_Codecs; use Internal_Codecs;
with Ada.Exceptions;
with Ada.Text_Io; use Ada.Text_IO;
with League.Strings; use League.Strings;

package body ARPM_DB_Containers is 
    procedure Save(MyRPM : in out ARPM_RPM_Access; DB : in Session_Type) is 
        RPM_Record : Detached_RPM'Class := New_RPM;
    begin
        RPM_Record.Set_Release(US_To_String(MyRPM.Release));
        RPM_Record.Set_Name(US_To_String(MyRPM.Name));
        RPM_Record.Set_Version(US_To_String(MyRPM.Version));
        DB.Persist(RPM_Record);
    exception
        when The_Event: others =>
            --Put_Line("Failed to save  " & US_To_String(MyRPM.Name));
            Put_Line("Reason:" & Ada.Exceptions.Exception_Message(The_Event));
    end Save;
end ARPM_DB_Containers; 
