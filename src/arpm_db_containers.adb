with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with ARPM_DB_Handlers;
with Internal_Codecs; use Internal_Codecs;
with GNAT.SHA256; use GNAT.SHA256;

with GNATCOLL.SQL_Impl;   use GNATCOLL.SQL_Impl;

package body ARPM_DB_Containers is 
    
    function SHA256(Name : String; Version :  String := ""; Release : String := ""; Arch : String := "" ) return String is 
    begin
        return GNAT.SHA256.Digest(Str_To_Sea(Name & ("-" & Version & "." & Release & "." & Arch )));
    end SHA256;

    procedure Save_Provides(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is 
         QP  : Prepared_Statement;
         provides_parameters :   SQL_Parameters (1 .. 4) :=
             (1 => (Parameter_Text, null),
              2 => (Parameter_Text, null),
              3 => (Parameter_Text, null),
              4 => (Parameter_Text, null));
         QPP : Prepared_Statement;
         provides_packages_parameters :   SQL_Parameters (1 .. 2) :=
             (1 => (Parameter_Text, null),
              2 => (Parameter_Text, null));
    begin
        QP := Prepare ("INSERT INTO  provides ( name, version, release, provideKey) VALUES ( ?, ?, ?, ?)");
        QPP := Prepare ("INSERT INTO packages_provides ( pkgKey, provideKey ) VALUES ( ? , ? )");
        for I in 1..Integer(RPM.Provides.Length) loop
            declare
                Name : aliased String := To_String(RPM.Provides.Element(I));
                Version : aliased String := To_String(RPM.Provides_Version.Element(I));
                Release : aliased String := "FIXME";
                SHA : aliased String := SHA256(Name => Name, Version => Version);
                SHAPKG : aliased String := SHA256(To_String(RPM.Name), To_String(RPM.Version), To_String(RPM.Release), To_String(RPM.Arch));
            begin
                if not ARPM_DB_Handlers.DB_Keys.Has_Provide_Key(SHA) then 
                    ARPM_DB_Handlers.DB_Keys.Add_Provide_Key (SHA);
                    provides_parameters := ("+"(Name'Access), "+" (Version'Access), "+" (Release'Access), "+"(SHA'Access));
                    Execute(DB, QP, provides_parameters);
                end if;
                provides_packages_parameters := ("+"(SHAPKG'Access), "+"(SHA'Access));
                Execute(DB, QPP, provides_packages_parameters);
            end;
        end loop;
    exception
        when The_Event: others =>
            pragma Debug(Put_Line("Failed to save provides " & To_String(RPM.Name) & "  Message: " & Ada.Exceptions.Exception_Message(The_Event)));
            pragma Debug(Put_Line (Ada.Exceptions.Exception_Information(The_Event)));
    end Save_Provides;

    procedure Save_Requires(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is 
         QP  : Prepared_Statement;
         requires_parameters :   SQL_Parameters (1 .. 4) :=
             (1 => (Parameter_Text, null),
              2 => (Parameter_Text, null),
              3 => (Parameter_Text, null),
              4 => (Parameter_Text, null));
         QPP : Prepared_Statement;
         requires_packages_parameters :   SQL_Parameters (1 .. 2) :=
             (1 => (Parameter_Text, null),
              2 => (Parameter_Text, null));
    begin
        QP := Prepare ("INSERT INTO  requires ( name, version, release, requireKey) VALUES ( ?, ?, ?, ?)");
        QPP := Prepare ("INSERT INTO packages_requires ( pkgKey, requireKey ) VALUES ( ? , ? )");
        for I in 1..Integer(RPM.requires.Length) loop
            declare
                Name : aliased String := To_String(RPM.requires.Element(I));
                Version : aliased String := To_String(RPM.requires_Version.Element(I));
                Release : aliased String := "FIXME";
                SHA : aliased String := SHA256(Name => Name, Version => Version);
                SHAPKG : aliased String := SHA256(To_String(RPM.Name), To_String(RPM.Version), To_String(RPM.Release), To_String(RPM.Arch));
            begin
                if not ARPM_DB_Handlers.DB_Keys.Has_require_Key(SHA) then 
                    ARPM_DB_Handlers.DB_Keys.Add_require_Key (SHA);
                    requires_parameters := ("+"(Name'Access), "+" (Version'Access), "+" (Release'Access), "+"(SHA'Access));
                    Execute(DB, QP, requires_parameters);
                end if;
                requires_packages_parameters := ("+"(SHAPKG'Access), "+"(SHA'Access));
                Execute(DB, QPP, requires_packages_parameters);
            end;
        end loop;
    exception
        when The_Event: others =>
            pragma Debug(Put_Line("Failed to save requires " & To_String(RPM.Name) & "  Message: " & Ada.Exceptions.Exception_Message(The_Event)));
            pragma Debug(Put_Line (Ada.Exceptions.Exception_Information(The_Event)));
    end Save_requires;

    procedure Save(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is 
        Q  : Prepared_Statement;
        param :   SQL_Parameters (1 .. 8) :=
            (1 => (Parameter_Text, null),
             2 => (Parameter_Text, null),
             3 => (Parameter_Text, null),
             4 => (Parameter_Text, null),
             5 => (Parameter_Text, null),
             6 => (Parameter_Text, null),
             7 => (Parameter_Text, null),
             8 => (Parameter_Text, null));
        Name : aliased String := To_String(RPM.Name);
        Version : aliased String := To_String(RPM.Version);
        Release : aliased String := To_String(RPM.Release);
        Arch : aliased String := To_String(RPM.Arch);
        Summary : aliased String := To_String(RPM.Summary);
        Description : aliased String := To_String(RPM.Description);
        Url : aliased String := To_String(RPM.URL);
        SHA : aliased String := SHA256(Name, Version, Release, Arch);
    begin
        Reset_Connection (DB);
        Q := Prepare ("INSERT INTO packages (pkgKey, name, version, release, arch, summary, description, url ) VALUES (? , ? , ? , ?, ?, ?, ?, ? )");
        Param := ("+"(SHA'Access), "+"(Name'Access), "+" (Version'Access), "+" (Release'Access), "+"(Arch'Access), "+"(Summary'Access), "+"(Description'Access), "+"(URL'Access));
        Execute(DB, Q, Param);
    end Save;
end ARPM_DB_Containers;
