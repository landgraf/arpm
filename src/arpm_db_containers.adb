with Ada.Text_IO; use Ada.Text_IO;
with SQL.Queries;
with Internal_Codecs; use Internal_Codecs;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
package body ARPM_DB_Containers is 
    function "+"
        (Item : Wide_Wide_String) return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;
    
    package body Constructors is 
        procedure Create_Tables(DB : in ARPM_DB_Container_Access) is 
               Q : SQL.Queries.SQL_Query := DB.Handler.Query;
        begin
               Q.Prepare
                   (+"CREATE TABLE packages (pkgKey INTEGER, name CHARACTER VARYING, version FLOAT)");
               Q.Execute;
        end Create_Tables;

        function Create(Name : in Universal_String) return ARPM_DB_Container_Access is 
            DB : ARPM_DB_Container_Access := new ARPM_DB_Container;
            DB_Driver : constant League.Strings.Universal_String
                := League.Strings.To_Universal_String ("SQLITE3");
            DB_Options : constant League.Strings.Universal_String
                := Name;
        begin
            DB.Handler := new SQL_Database'(SQL.Databases.Create (DB_Driver, DB_Options));
            DB.Handler.Open;
            Create_Tables(DB);
            DB.Error := 0;
            return DB;
        exception 
            when others => 
                Put_Line("Error:" & US_To_String(DB.Handler.Error_Message));
                return DB;
        end Create;
    end Constructors;
    procedure Save_Main(RPM : in ARPM_RPM_Access; DB : in ARPM_DB_Container_Access) is 
    begin
        null;
    end Save_Main;
end ARPM_DB_Containers;
