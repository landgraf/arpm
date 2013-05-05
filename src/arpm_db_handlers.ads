with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;

package ARPM_DB_Handlers is 
    protected DB_Keys is 
            procedure Add_Provide_Key(Key : in String);
            function Has_Provide_Key(Key : in String) return Boolean;
            procedure Add_Require_Key(Key : in String);
            function Has_Require_Key(Key : in String) return Boolean;
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
