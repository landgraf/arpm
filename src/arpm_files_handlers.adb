with Ada.Text_Io; use Ada.Text_IO;
with Ada.Directories;
with Internal_Codecs; use Internal_Codecs;
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
    
    protected body  KeyGenerator is 
        procedure Next(Key : out Integer) is 
        begin
            Key := K; 
            K := K + 1;
        end Next;
    end KeyGenerator;

    protected body DB_Keys is 
            procedure Add_Provide_Key(Key : in Unbounded_String) is 
            begin
                Provides.Append(New_Item => Key);
            end Add_Provide_Key;
            function Has_Provide_Key(Key : in Unbounded_String) return Boolean is 
            begin
                if Provides.Is_Empty or not Provides.Contains(Key) then
                    return False;
                end if;
                return True;
            end Has_Provide_Key;
            procedure Add_Require_Key(Key : in Unbounded_String) is 
            begin
                Requires.Append(Key);
            end Add_Require_Key;
            function Has_Require_Key(Key : in Unbounded_String) return Boolean is 
            begin
                if Requires.Is_Empty or not Requires.Contains(Key) then
                    return False;
                end if;
                return True;
            end Has_Require_Key;
    end DB_Keys;
    protected body DB is 

            function Create_DB(Session : Session_Type) return Integer is 
            begin
                return 1;
            exception
                when others =>
                    Put_Line("CreateDB is failed");
                    return 1;
            end Create_DB;

            entry Get_DB(rDB : out Database_Connection ) when Initialized is
            begin
                rDB :=  GNATCOLL.SQL.Exec.Get_Task_Connection
                        (Description  => DB);
            end Get_DB;

            function Prepare_Directories(Dir : String) return Integer is 
                Suf : constant String := "/repodata/";
            begin
                if Ada.Directories.Exists(Dir & Suf) then 
                    Ada.Directories.Delete_Tree(Dir & Suf);
                end if;
                Ada.Directories.Create_Directory(Dir & Suf);
                return 0;
            exception
                when others =>
                    return 1;
            end Prepare_Directories;

            procedure Init_DB(FileName : in String) is 
                Suffix : constant String := "/repodata/";
            begin
                Put_Line("InitDB");
                --if Prepare_Directories(FileName) = 0 then
                   DB := GNATCOLL.SQL.Sqlite.Setup (FileName & Suffix & "dbname.db");
                --end if;
                Initialized := True;
            exception
                when others =>
                    Put_Line("DB is failed");
            end Init_DB;

            procedure Free is 
            begin
                null;
            end Free;
    end DB;
end ARPM_FIles_Handlers;
