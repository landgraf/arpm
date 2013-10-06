with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with ARPM_DB_Handlers;
with Internal_Codecs; use Internal_Codecs;
with GNAT.SHA256; use GNAT.SHA256;

with GNATCOLL.SQL_Impl;   use GNATCOLL.SQL_Impl;

package body Arpm_Db_Containers is

    function SHA256(Name : String; Version :  String := ""; Release : String := ""; Arch : String := "" ) return String is
    begin
        return GNAT.SHA256.Digest(Str_To_Sea(Name & ("-" & Version & "." & Release & "." & Arch )));
    end SHA256;

    procedure Save_Provides(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is
         QP  : Prepared_Statement;
         Provides_Parameters :   SQL_Parameters (1 .. 4) :=
             (1 => (Parameter_Text, null),
              2 => (Parameter_Text, null),
              3 => (Parameter_Text, null),
              4 => (Parameter_Text, null));
         QPP : Prepared_Statement;
         Provides_packages_Parameters :   SQL_Parameters (1 .. 2) :=
             (1 => (Parameter_Text, null),
              2 => (Parameter_Text, null));
    begin
        QP := Prepare ("INSERT intO  Provides ( Name, Version, release, provideKey) VALUES ( ?, ?, ?, ?)");
        QPP := Prepare ("INSERT intO packages_Provides ( pkgKey, provideKey ) VALUES ( ? , ? )");
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
                    Provides_Parameters := ("+"(Name'Access), "+" (Version'Access), "+" (Release'Access), "+"(SHA'Access));
                    Execute(DB, QP, Provides_Parameters);
                end if;
                Provides_packages_Parameters := ("+"(SHAPKG'Access), "+"(SHA'Access));
                Execute(DB, QPP, Provides_packages_Parameters);
            end;
        end loop;
    exception
        when The_Event: others =>
            pragma Debug(Put_Line("Failed to Save Provides " & To_String(RPM.Name) & "  Message: " & Ada.Exceptions.Exception_Message(The_Event)));
            pragma Debug(Put_Line (Ada.Exceptions.Exception_Information(The_Event)));
    end Save_Provides;

    procedure Save_Requires(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is
         QP  : Prepared_Statement;
         requires_Parameters :   SQL_Parameters (1 .. 4) :=
             (1 => (Parameter_Text, null),
              2 => (Parameter_Text, null),
              3 => (Parameter_Text, null),
              4 => (Parameter_Text, null));
         QPP : Prepared_Statement;
         requires_packages_Parameters :   SQL_Parameters (1 .. 2) :=
             (1 => (Parameter_Text, null),
              2 => (Parameter_Text, null));
    begin
        QP := Prepare ("INSERT intO  requires ( Name, Version, release, requireKey) VALUES ( ?, ?, ?, ?)");
        QPP := Prepare ("INSERT intO packages_requires ( pkgKey, requireKey ) VALUES ( ? , ? )");
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
                    requires_Parameters := ("+"(Name'Access), "+" (Version'Access), "+" (Release'Access), "+"(SHA'Access));
                    Execute(DB, QP, requires_Parameters);
                end if;
                requires_packages_Parameters := ("+"(SHAPKG'Access), "+"(SHA'Access));
                Execute(DB, QPP, requires_packages_Parameters);
            end;
        end loop;
    exception
        when The_Event: others =>
            pragma Debug(Put_Line("Failed to Save requires " & To_String(RPM.Name) & "  Message: " & Ada.Exceptions.Exception_Message(The_Event)));
            pragma Debug(Put_Line (Ada.Exceptions.Exception_Information(The_Event)));
    end Save_Requires;

    procedure Save(RPM : in ARPM_RPM_Access; DB : in Database_Connection) is
        Q  : Prepared_Statement;
        Param :   SQL_Parameters (1 .. 11) :=
            (1 => (Parameter_Text, null),
             2 => (Parameter_Text, null),
             3 => (Parameter_Text, null),
             4 => (Parameter_Text, null),
             5 => (Parameter_Text, null),
             6 => (Parameter_Text, null),
             7 => (Parameter_Text, null),
             8 => (Parameter_Text, null),
             9 => (Parameter_Text, null),
             11 => (Parameter_Text, null),
             10 => (Parameter_Text, null)
             );
        Name : aliased String := To_String(RPM.Name);
        Version : aliased String := To_String(RPM.Version);
        Release : aliased String := To_String(RPM.Release);
        Arch : aliased String := To_String(RPM.Arch);
        Summary : aliased String := To_String(RPM.Summary);
        Description : aliased String := To_String(RPM.Description);
        URL : aliased String := To_String(RPM.URL);
        License : aliased String := To_String(RPM.License);
        Vendor : aliased String := To_String(RPM.Vendor);
        Epoch : aliased String := To_String(RPM.Epoch);
        SHA : aliased String := SHA256(Name, Version, Release, Arch);
    begin
        Reset_Connection (DB);
        Q := Prepare ("INSERT intO packages (pkgKey, Name, Version, release, arch, summary, description, URL, rpm_License , rpm_vendor, Epoch) VALUES (? , ? , ? , ?, ?, ?, ?, ?, ?, ?, ? )");
        Param := ("+"(SHA'Access), "+"(Name'Access), "+" (Version'Access), "+" (Release'Access), "+"(Arch'Access), "+"(Summary'Access),
                  "+"(Description'Access), "+"(URL'Access), "+"(License'Access), "+"(Vendor'Access), "+" (Epoch'Access)  );
        Execute(DB, Q, Param);
    end Save;
end Arpm_Db_Containers;
