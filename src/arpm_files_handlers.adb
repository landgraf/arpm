with Ada.Text_Io; use Ada.Text_IO;
-- REMOVEME 
with ORM; use ORM;
package body ARPM_Files_Handlers is 
    protected body Files is 
        procedure Put(FileName : Unbounded_String) is 
        begin
            F.Append(FileName);
        end Put;
        entry Get(FileName : out Unbounded_String) when not F.Is_Empty or E is 
        begin
            if E then
                FileName := Null_Unbounded_String;
            else
                FileName := F.First_Element;
                F.Delete_First;
            end if;
        end Get;
        entry Finish when F.Is_Empty is 
        begin
            E := True;
        end Finish;
    end Files;
    protected body Sessions is 
        function Get_Session return Session_Type is 
        begin
            return Get_New_Session;
        end Get_Session;
        procedure Create_Session is
        begin
            GNATCOLL.SQL.Sessions.Setup
               (Descr  => GNATCOLL.SQL.Sqlite.Setup ("db/arpm.db"),
                Weak_Cache => True,
                Max_Sessions => 9);
            Setup := True;
        end Create_Session;
        entry Wait_Session(DB : out Session_Type) when Setup is
        begin
            DB := Get_New_Session;
        end Wait_Session;
    end Sessions;
    protected body Workers is 
        procedure Increase is 
        begin
            Count := Count + 1;
            Started := True;
        end Increase;

        procedure Decrease is
        begin
            Count := Count - 1;
        end Decrease;
        
        entry Is_Empty when Count = 0 and Started is
        begin
            null;
        end Is_Empty;

    end Workers;
begin
    Sessions.Create_Session;
end ARPM_FIles_Handlers;
