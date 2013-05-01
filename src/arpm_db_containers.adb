with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;
with League.Holders;
with League.Holders.Integers;
with ARPM_Files_Handlers;
with SQL.Queries;
with Internal_Codecs; use Internal_Codecs;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with GNAT.SHA256; use GNAT.SHA256;
package body ARPM_DB_Containers is 
    function "+"
        (Item : Wide_Wide_String) return League.Strings.Universal_String
        renames League.Strings.To_Universal_String;
    
    function SHA256(Item : Universal_String) return Universal_String is 
    begin
        return String_To_US(GNAT.SHA256.Digest(US_To_Sea(Item)));
    end SHA256;

    procedure Save_Provides(RPM : in ARPM_RPM_Access; DB : in ARPM_DB_Container_Access) is 
        Q : SQL.Queries.SQL_Query := DB.Handler.Query;
    begin
        Q.Prepare (+"INSERT INTO  provides ( name, version, release, provideKey) VALUES ( :name, :version, :release, :pkey)");
        for I in 1..Length(RPM.Provides) loop
            if not ARPM_Files_Handlers.DB_Keys.Has_Provide_Key(RPM.Provides.Element(I)) then 
                ARPM_Files_Handlers.DB_Keys.Add_Provide_Key (RPM.Provides.Element(I));
                Q.Bind_Value (+":name", League.Holders.To_Holder(RPM.Provides.Element(I)));
                Q.Bind_Value (+":version", League.Holders.To_Holder(To_Universal_string("Fixme")));
                Q.Bind_Value (+":release", League.Holders.To_Holder(To_Universal_string("Fixme")));
                Q.Bind_Value (+":pkey", League.Holders.To_Holder (SHA256(RPM.Provides.Element(I))));
                Q.Execute;
                Q.Finish;
            end if;
        end loop;
    exception
        when The_Event: others =>
            Put_Line("Failed to save provides " & US_To_String(RPM.Name) & "  Message: " & Ada.Exceptions.Exception_Message(The_Event));
            Put_Line (Ada.Exceptions.Exception_Information(The_Event));
    end Save_Provides;

    procedure Save_requires(RPM : in ARPM_RPM_Access; DB : in ARPM_DB_Container_Access) is 
        Q : SQL.Queries.SQL_Query := DB.Handler.Query;
    begin
        Q.Prepare (+"INSERT INTO  requires ( name, version, release, requireKey) VALUES ( :name, :version, :release, :pkey)");
        for I in 1..Length(RPM.requires) loop
            if not ARPM_Files_Handlers.DB_Keys.Has_require_Key(RPM.requires.Element(I)) then 
                ARPM_Files_Handlers.DB_Keys.Add_Require_Key (RPM.requires.Element(I));
                Q.Bind_Value (+":name", League.Holders.To_Holder(RPM.requires.Element(I)));
                Q.Bind_Value (+":version", League.Holders.To_Holder(To_Universal_string("Fixme")));
                Q.Bind_Value (+":release", League.Holders.To_Holder(To_Universal_string("Fixme")));
                Q.Bind_Value (+":pkey", League.Holders.To_Holder (SHA256(RPM.requires.Element(I))));
                Q.Execute;
                Q.Finish;
            end if;
        end loop;
    exception
        when others =>
            Put_Line("Failed to save requires " & US_To_String(RPM.Name));
    end Save_requires;

    procedure Save_Main(RPM : in ARPM_RPM_Access; DB : in ARPM_DB_Container_Access) is 
        Q : SQL.Queries.SQL_Query := DB.Handler.Query;
    begin
        Q.Prepare (+"INSERT INTO packages (pkgKey, name, version, release) VALUES (:key, :name, :version, :release)");
        Q.Bind_Value (+":key", League.Holders.To_Holder (SHA256(RPM.Name)));
        Q.Bind_Value (+":name", League.Holders.To_Holder(RPM.name));
        Q.Bind_Value (+":version", League.Holders.To_Holder(RPM.version));
        Q.Bind_Value (+":release", League.Holders.To_Holder(RPM.release));
        Q.Execute;
        Save_Provides(RPM, DB);
        Save_Requires(RPM, DB);
    end Save_Main;
end ARPM_DB_Containers;
