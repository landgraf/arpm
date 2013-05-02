with Ada.Text_Io; use Ada.Text_IO;
with SQL.Queries; use SQL.Queries;
with Ada.Directories;
with Internal_Codecs; use Internal_Codecs;
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
            -- function Get_DB return ARPM_DB_Container_Access is 
            -- entry  Get_DB(rDB : out ARPM_DB_Container_Access) when DB /= Null and DB.Handler /= Null is 
            entry  Get_DB(rDB : out ARPM_DB_Container_Access) when Initialized is 
            begin
                rDB := DB;
            end Get_DB;
            function Prepare_filename(Dir : Universal_String) return Universal_String is
                RepoDir : constant String := (US_To_String(Dir) & "/repodata/");
            begin
                if not Ada.Directories.Exists(RepoDir) then
                    Ada.Directories.Create_Directory(RepoDir);
                end if;
                return Dir & To_Universal_String("/repodata/primary.db");
            end Prepare_FileName;

            function Create_DB return Integer is 
                function "+"
                    (Item : Wide_Wide_String) return League.Strings.Universal_String
                    renames League.Strings.To_Universal_String;
                Q : SQL.Queries.SQL_Query := DB.Handler.Query;
            begin
                -- Q.Prepare(+"PRAGMA foreign_keys = ON");
                -- Q.Execute;
                -- Q.Prepare(+"PRAGMA encoding = 'UTF-8'");
                -- Q.Execute;
                -- Q.Prepare(+"PRAGMA default_cache_size = 4000");
                -- Q.Execute;
                Q.Prepare(+"PRAGMA synchronous = OFF");
                Q.Execute;
                Q.Prepare(+"PRAGMA temp_store = MEMORY");
                Q.Execute;
                -- Q.Prepare(+"PRAGMA journal_mode = OFF");
                -- Q.Execute;
                Q.Prepare(+"CREATE TABLE conflicts (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65))");
                Q.Execute;
                Q.Prepare(+"CREATE TABLE db_info (dbversion INTEGER, checksum TEXT)");
                Q.Execute;
                Q.Prepare(+"CREATE TABLE files (  name TEXT,  type TEXT,  pkgKey CHAR(65))");
                Q.Execute;
                Q.Prepare(+"CREATE TABLE obsoletes (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65) )");
                Q.Execute;
                Q.Prepare(+"CREATE TABLE packages (  pkgKey CHAR(65) PRIMARY KEY,  pkgId TEXT,  name TEXT,  arch TEXT,  version TEXT,  epoch TEXT,  release TEXT,  summary TEXT,  description TEXT,  url TEXT,  time_file INTEGER,  time_build INTEGER,  rpm_license TEXT,  rpm_vendor TEXT,  rpm_group TEXT,  rpm_buildhost TEXT,  rpm_sourcerpm TEXT,  rpm_header_start INTEGER,  rpm_header_end INTEGER,  rpm_packager TEXT,  size_package INTEGER,  size_installed INTEGER,  size_archive INTEGER,  location_href TEXT,  location_base TEXT,  checksum_type TEXT)");
                Q.Execute;
                Q.Prepare(+"CREATE TABLE provides (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65) , provideKey CHAR(65))");
                Q.Execute;
                Q.Prepare(+"CREATE TABLE packages_provides ( pkgKey CHAR(65), provideKey CHAR(65), FOREIGN KEY(pkgKey) REFERENCES packages(pkgKey) ON DELETE CASCADE, FOREIGN KEY(provideKey) REFERENCES provides(providesKey) ON DELETE CASCADE)");
                Q.Execute;
                Q.Prepare(+"CREATE TABLE requires (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgkey CHAR(65) , pre BOOLEAN DEFAULT FALSE, requirekey CHAR(65))");
                Q.Execute;
                Q.Prepare(+"CREATE TABLE packages_requires ( pkgkey CHAR(65), requirekey CHAR(65), FOREIGN KEY(pkgKey) REFERENCES packages(pkgKey) ON DELETE CASCADE, FOREIGN KEY(requireKey) REFERENCES requires(requiresKey) ON DELETE CASCADE)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX filenames ON files (name)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX packageId ON packages (pkgId)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX packagename ON packages (name)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX pkgconflicts on conflicts (pkgKey)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX pkgfiles ON files (pkgKey)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX pkgobsoletes on obsoletes (pkgKey)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX pkgprovides on provides (pkgKey)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX pkgrequires on requires (pkgKey)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX providesname ON provides (name)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX requiresname ON requires (name)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX packages_provides_providekey ON packages_provides(provideKey)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX packages_provides_pkgkey ON packages_provides(pkgKey)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX packages_requires_requirekey ON packages_requires(requireKey)");
                Q.Execute;
                Q.Prepare(+"CREATE INDEX packages_requires_pkgkey ON packages_requires(pkgKey)");
                Q.Execute;
                Q.Prepare(+"CREATE TRIGGER removals AFTER DELETE ON packages  BEGIN    DELETE FROM files WHERE pkgKey = old.pkgKey;    DELETE FROM requires WHERE pkgKey = old.pkgKey;    DELETE FROM provides WHERE pkgKey = old.pkgKey;    DELETE FROM conflicts WHERE pkgKey = old.pkgKey;    DELETE FROM obsoletes WHERE pkgKey = old.pkgKey;  END");
                Q.Execute;
                return 0;
            exception
                when others =>
                    Put_Line("CreateDB is failed");
                    return 1;
            end Create_DB;

            procedure Init_DB(FileName : in Universal_String) is 
                DB_Driver : constant League.Strings.Universal_String
                    := League.Strings.To_Universal_String ("SQLITE3");
                DB_Options : constant League.Strings.Universal_String
                    := Prepare_Filename(Filename);
            begin
                Put_Line("InitDB");
                DB := new ARPM_DB_Container;
                DB.Handler := new SQL_Database'(SQL.Databases.Create (DB_Driver, DB_Options));
                DB.Handler.Open;
                DB.Error := 0;
                DB.Error := Create_DB;
                Initialized := True;
            exception
                when others =>
                    Put_Line("DB is failed");
            end Init_DB;
            procedure Free is 
            begin
                      DB.Handler.Close;
                      Free_Handler(DB.Handler);
                      Free(DB);
            end Free;
    end DB;
end ARPM_FIles_Handlers;
