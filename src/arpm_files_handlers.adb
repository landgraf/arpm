with Ada.Text_Io; use Ada.Text_IO;
with Matreshka.Internals.SQL_Drivers.SQLite3.Factory;
with SQL.Databases; use SQL.Databases;
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
            procedure Add_Provide_Key(Key : in Universal_String) is 
            begin
                Provides.Append(Key);
            end Add_Provide_Key;
            function Has_Provide_Key(Key : in Universal_String) return Boolean is 
            begin
                if Provides.Is_Empty or Index(Provides, Key) = 0 then
                    return False;
                end if;
                return True;
            end Has_Provide_Key;
            procedure Add_Require_Key(Key : in Universal_String) is 
            begin
                Requires.Append(Key);
            end Add_Require_Key;
            function Has_Require_Key(Key : in Universal_String) return Boolean is 
            begin
                if Requires.Is_Empty or Requires.Index(Key) = 0 then
                    return False;
                end if;
                return True;
            end Has_Require_Key;
    end DB_Keys;
    protected body DB is 
            function Get_DB return ARPM_DB_Container_Access is 
            begin
                return DB;
            end Get_DB;
            procedure Init_DB(FileName : in Universal_String) is 
                DB_Driver : constant League.Strings.Universal_String
                    := League.Strings.To_Universal_String ("SQLITE3");
                DB_Options : constant League.Strings.Universal_String
                    := FileName;
            begin
                DB := new ARPM_DB_Container;
                DB.Handler := new SQL_Database'(SQL.Databases.Create (DB_Driver, DB_Options));
                DB.Handler.Open;
                -- Create_Tables(DB);
                DB.Error := 0;
            end Init_DB;
            procedure Free is 
            begin
                      DB.Handler.Close;
                      Free_Handler(DB.Handler);
                      Free(DB);
            end Free;
    end DB;
begin
    DB.Init_DB(To_Universal_String("/tmp/db/arpm.db"));
end ARPM_FIles_Handlers;
