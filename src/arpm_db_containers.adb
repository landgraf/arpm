with Ada.Text_IO; use Ada.Text_IO;
with League.Holders;
with League.Holders.Integers;
with ARPM_Files_Handlers;
with SQL.Queries;
with Internal_Codecs; use Internal_Codecs;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
package body ARPM_DB_Containers is 
    function "+"
        (Item : Wide_Wide_String) return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;
    
    procedure Save_Provides(RPM : in ARPM_RPM_Access; DB : in ARPM_DB_Container_Access) is 
        Q : SQL.Queries.SQL_Query := DB.Handler.Query;
        Key : Integer; 
    begin
        for I in 1..Length(RPM.Provides) loop
            if ARPM_Files_Handlers.DB_Keys.Has_Provide_Key(RPM.Provides.Element(I)) then 
                -- update here 
                -- FIXME
                null;
            else
                Q.Prepare (+"INSERT INTO  provides ( name, version, release, provideKey) VALUES ( :name, :version, :release, :pkey)");
                ARPM_Files_Handlers.KeyGenerator.Next (Key);
                Q.Bind_Value (+":name", League.Holders.To_Holder(RPM.Provides.Element(I)));
                Q.Bind_Value (+":version", League.Holders.To_Holder(To_Universal_string("Fixme")));
                Q.Bind_Value (+":release", League.Holders.To_Holder(To_Universal_string("Fixme")));
                Q.Bind_Value (+":pkey", League.Holders.Integers.To_Holder (Key));
                Q.Execute;
            end if;
        end loop;

    end Save_Provides;

    procedure Save_Main(RPM : in ARPM_RPM_Access; DB : in ARPM_DB_Container_Access) is 
        Q : SQL.Queries.SQL_Query := DB.Handler.Query;
        Key : Integer; 
    begin
        Q.Prepare (+"INSERT INTO packages (pkgKey, name, version, release) VALUES (:key, :name, :version, :release)");
        ARPM_Files_Handlers.KeyGenerator.Next (Key);
        Q.Bind_Value (+":pkgKey", League.Holders.Integers.To_Holder (Key));
        Q.Bind_Value (+":name", League.Holders.To_Holder(RPM.name));
        Q.Bind_Value (+":version", League.Holders.To_Holder(RPM.version));
        Q.Bind_Value (+":release", League.Holders.To_Holder(RPM.release));
        Q.Execute;
        Save_Provides(RPM, DB);
    end Save_Main;
end ARPM_DB_Containers;
