with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with Ada.Directories; use Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.SQLite;

package ARPM_DB_Handlers is 
    protected DB_Keys is 
            procedure Add_Provide_Key(Key : in Unbounded_String);
            function Has_Provide_Key(Key : in Unbounded_String) return Boolean;
            procedure Add_Require_Key(Key : in Unbounded_String);
            function Has_Require_Key(Key : in Unbounded_String) return Boolean;
        private
            Provides : ARPM_OSets_Container ;
            Requires : ARPM_Osets_Container;
    end DB_Keys;

    protected DB is 
            procedure Init_DB(FileName : in String);
            entry Get_DB(rDB : out Database_Description);
            procedure Free;
        private 
            DB : GNATCOLL.SQL.Exec.Database_Description;
            Initialized : Boolean := False;
    end DB;

end ARPM_DB_Handlers;
