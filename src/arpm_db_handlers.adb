package body ARPM_DB_Handlers is 

    protected body DB_Keys is
        procedure Add_Provide_Key(Key : in Unbounded_String) is
        begin
            Provides.Insert(SHA256S(To_String(Key)));
        end Add_Provide_Key;
        function Has_Provide_Key(Key : in Unbounded_String) return Boolean is
        begin
            if Provides.Is_Empty or not Provides.Contains(SHA256S(To_String(Key))) then
                return False;
            end if;
            return True;
        end Has_Provide_Key;
        procedure Add_Require_Key(Key : in Unbounded_String) is
        begin
            Requires.Insert(SHA256S(To_String(Key)));
        end Add_Require_Key;
        function Has_Require_Key(Key : in Unbounded_String) return Boolean is
        begin
            if Requires.Is_Empty or not Requires.Contains(SHA256S(To_String(Key))) then
                return False;
            end if;
            return True;
        end Has_Require_Key;
    end DB_Keys;



    protected body DB is 

    function Create_DB return Boolean is 
        nDB : Database_Connection;
    begin
        nDB := GNATCOLL.SQL.Exec.Get_Task_Connection
            (Description  => DB);
        Execute(nDB,"CREATE TABLE conflicts (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65));" );
        Execute(nDB,"CREATE TABLE db_info (dbversion INTEGER, checksum TEXT);" ); 
        Execute(nDB,"CREATE TABLE files (  name TEXT,  type TEXT,  pkgKey CHAR(65)); " ); 
        Execute(nDB,"CREATE TABLE obsoletes (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65) );" );
        Execute(nDB,"CREATE TABLE packages (  pkgKey CHAR(65) PRIMARY KEY,  pkgId TEXT,  name TEXT,  arch TEXT,  version TEXT,  epoch TEXT,  release TEXT,  summary TEXT,  description TEXT,  url TEXT,  time_file INTEGER,  time_build INTEGER,  rpm_license TEXT,  rpm_vendor TEXT,  rpm_group TEXT,  rpm_buildhost TEXT,  rpm_sourcerpm TEXT,  rpm_header_start INTEGER,  rpm_header_end INTEGER,  rpm_packager TEXT,  size_package INTEGER,  size_installed INTEGER,  size_archive INTEGER,  location_href TEXT,  location_base TEXT,  checksum_type TEXT);" );
        Execute(nDB,"CREATE TABLE provides (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65) , provideKey CHAR(65) PRIMARY KEY);" );
        Execute(nDB,"CREATE TABLE packages_provides ( pkgKey CHAR(65), provideKey CHAR(65));" );
        Execute(nDB,"CREATE TABLE requires (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgkey CHAR(65) , pre BOOLEAN DEFAULT FALSE, requirekey CHAR(65) PRIMARY KEY);" );
        Execute(nDB,"CREATE TABLE packages_requires ( pkgkey CHAR(65), requirekey CHAR(65) );" );
        Execute(nDB,"CREATE INDEX filenames ON files (name);" );
        Execute(nDB,"CREATE INDEX packageId ON packages (pkgId);" );
        Execute(nDB,"CREATE INDEX packagename ON packages (name);" );
        Execute(nDB,"CREATE INDEX pkgconflicts on conflicts (pkgKey);" );
        Execute(nDB,"CREATE INDEX pkgfiles ON files (pkgKey);" );
        Execute(nDB,"CREATE INDEX pkgobsoletes on obsoletes (pkgKey);" );
        Execute(nDB,"CREATE INDEX pkgprovides on provides (pkgKey);" );
        Execute(nDB,"CREATE INDEX pkgrequires on requires (pkgKey);" );
        Execute(nDB,"CREATE INDEX providesname ON provides (name);" );
        Execute(nDB,"CREATE INDEX requiresname ON requires (name);" );
        Execute(nDB,"CREATE INDEX packages_provides_providekey ON packages_provides(provideKey);" );
        Execute(nDB,"CREATE INDEX packages_provides_pkgkey ON packages_provides(pkgKey);" );
        Execute(nDB,"CREATE INDEX packages_requires_requirekey ON packages_requires(requireKey);" );
        Execute(nDB,"CREATE INDEX packages_requires_pkgkey ON packages_requires(pkgKey);" );
        Execute(nDB,"CREATE TRIGGER removals AFTER DELETE ON packages  BEGIN    DELETE FROM files WHERE pkgKey = old.pkgKey;    DELETE FROM requires WHERE pkgKey = old.pkgKey;    DELETE FROM provides WHERE pkgKey = old.pkgKey;    DELETE FROM conflicts WHERE pkgKey = old.pkgKey;    DELETE FROM obsoletes WHERE pkgKey = old.pkgKey;  END;");
        nDB.Commit;
        Free(nDB);
        return True;
    exception
        when others =>
            return False;
    end Create_DB;

            entry Get_DB(rDB : out Database_Description) when Initialized is
            begin
                rDB :=  DB;
            end Get_DB;

            function Prepare_Directories(Dir : String) return Boolean is 
                Suf : constant String := "/repodata/";
            begin
                if Ada.Directories.Exists(Dir & Suf) then 
                    Ada.Directories.Delete_Tree(Dir & Suf);
                end if;
                Ada.Directories.Create_Directory(Dir & Suf);
                return True;
            exception
                when others =>
                    return False;
            end Prepare_Directories;

            procedure Init_DB(FileName : in String) is 
                Suffix : constant String := "/repodata/";
                DB_Exception : exception;
            begin
               DB := GNATCOLL.SQL.Sqlite.Setup (FileName & Suffix & "dbname.db");
                if Prepare_Directories(FileName) and Create_DB then
                   Initialized := True;
                else 
                    pragma Debug(Put_line("DB failed"));
                    raise DB_Exception;
                end if;
            exception
                when others =>
                    Put_Line("DB is failed");
            end Init_DB;

            procedure Free is 
            begin
                Free(DB);
            end Free;
    end DB;
end ARPM_DB_Handlers;