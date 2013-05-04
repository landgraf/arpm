with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with ARPM_Files_Handlers;
with Internal_Codecs; use Internal_Codecs;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with GNAT.SHA256; use GNAT.SHA256;

with GNATCOLL.SQL_Impl;   use GNATCOLL.SQL_Impl;
with GNATCOLL.SQL.Sqlite; use GNATCOLL.SQL;
-- with Database; use Database;

package body ARPM_DB_Containers is 
    
    function SHA256(Name : String; Version :  String := ""; Release : String := ""; Arch : String := "" ) return String is 
    begin
        return GNAT.SHA256.Digest(Str_To_Sea(Name & ("-" & Version & "." & Arch )));
    end SHA256;

    procedure Save_Provides(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is 
      --   QP  : Prepared_Statement;
      --   provides_parameters :   SQL_Parameters (1 .. 4) :=
      --       (1 => (Parameter_Text, null),
      --        2 => (Parameter_Text, 0),
      --        3 => (Parameter_Text, null),
      --        4 => (Parameter_Text, 0));
      --   QPP : Prepared_Statement;
      --   provides_packages_parameters :   SQL_Parameters (1 .. 2) :=
      --       (1 => (Parameter_Text, null),
      --        2 => (Parameter_Text, null));
    begin
        -- QP.Prepare ("INSERT INTO  provides ( name, version, release, provideKey) VALUES ( ?, ?, ?, ?)");
        --QPP.Prepare ("INSERT INTO packages_provides ( pkgKey, provideKey ) VALUES ( ? , ? )");
        for I in 1..Integer(RPM.Provides.Length) loop
            if not ARPM_Files_Handlers.DB_Keys.Has_Provide_Key(RPM.Provides.Element(I)) then 
                ARPM_Files_Handlers.DB_Keys.Add_Provide_Key (RPM.Provides.Element(I));
            end if;
        end loop;
    exception
        when The_Event: others =>
            pragma Debug(Put_Line("Failed to save provides " & To_String(RPM.Name) & "  Message: " & Ada.Exceptions.Exception_Message(The_Event)));
            pragma Debug(Put_Line (Ada.Exceptions.Exception_Information(The_Event)));
    end Save_Provides;

    procedure Save_requires(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is 
        -- QP  : Prepared_Statement;
        -- requires_parameters :   SQL_Parameters (1 .. 4) :=
        --     (1 => (Parameter_Text, null),
        --      2 => (Parameter_Text, 0),
        --      3 => (Parameter_Text, null),
        --      4 => (Parameter_Text, 0));
        -- QPP : Prepared_Statement;
        -- requires_packages_parameters :   SQL_Parameters (1 .. 2) :=
        --     (1 => (Parameter_Text, null),
        --      2 => (Parameter_Text, null));
    begin
        -- Q.Prepare (+"INSERT INTO  requires ( name, version, release, requireKey) VALUES ( :name, :version, :release, :pkey)");
        -- QPP.Prepare (+"INSERT INTO packages_requires ( pkgKey, requireKey ) VALUES ( :pkgKey, :requireKey)");
        for I in 1..Integer(RPM.requires.Length) loop
            if not ARPM_Files_Handlers.DB_Keys.Has_require_Key(RPM.requires.Element(I)) then 
                null;
            end if;
            null;
        end loop;
    exception
        when The_Event: others =>
            pragma Debug(Put_Line("Failed to save requires " & To_String(RPM.Name) & "  Message: " & Ada.Exceptions.Exception_Message(The_Event)));
            pragma Debug(Put_Line (Ada.Exceptions.Exception_Information(The_Event)));
    end Save_requires;

    procedure Save_Main(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is 
        Q  : Prepared_Statement;
        param :   SQL_Parameters (1 .. 4) :=
            (1 => (Parameter_Text, null),
             2 => (Parameter_Text, null),
             3 => (Parameter_Text, null),
             4 => (Parameter_Text, null));
        Name : aliased String := To_String(RPM.Name);
        Version : aliased String := To_String(RPM.Version);
        Release : aliased String := To_String(RPM.Release);
        Arch : aliased String := To_String(RPM.Arch);
        SHA : aliased String := SHA256(Name, Version, Release, Arch);
    begin
        Put_Line(Name);
        Q := Prepare ("INSERT INTO packages (pkgKey, name, version, release) VALUES (? , ? , ? , ? )");
        -- Q := SQL_Begin;
        Param := ("+"(SHA'Access), "+"(Name'Access), "+" (Version'Access), "+" (Release'Access));
        Execute(DB, Q, Param);
        -- Commit_Or_Rollback (DB);
        -- Put_Line("Exit");
    end Save_Main;
end ARPM_DB_Containers;
