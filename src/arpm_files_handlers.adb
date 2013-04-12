with Ada.Text_Io; use Ada.Text_IO;
package body ARPM_Files_Handlers is 
    protected body Files is 
        procedure Put(FileName : Unbounded_String) is 
        begin
            F.Append(FileName);
        end Put;
        entry Get(FileName : out Unbounded_String; myRC : out RC_Access) when not F.Is_Empty or E is 
        begin
            if E then
                FileName := Null_Unbounded_String;
            else
                myRC := R;
                FileName := F.First_Element;
                F.Delete_First;
            end if;
        end Get;
        entry Finish when F.Is_Empty is 
        begin
            E := True;
        end Finish;
        procedure Set_RC is 
            Status : Integer := 0;
            procedure RC_C(Status: out Integer; mRC : out RC_Access);
                pragma Import (C, RC_C, "read_config");

        begin
            RC_C(Status, R);
            if Status /= 0 then
                Put_Line("Failed to read config");
            end if;
        end Set_RC;
    end Files;
end ARPM_FIles_Handlers;
