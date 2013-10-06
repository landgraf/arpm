
with Ada.Text_IO; use Ada.Text_IO;
with GNATCOLL.SQL.Sqlite;
with Ada.Directories; use Ada.Directories;
package body ARPM_DB_Handlers is

   protected body DB_Keys is
     procedure Add_Provide_Key(Key : in String) is
     begin
       Provides.Insert(SHA256S(Key));
     end Add_Provide_Key;
     function Has_Provide_Key(Key : in String) return Boolean is
     begin
       if Provides.Is_Empty or not Provides.Contains(SHA256S(Key)) then
         return False;
       end if;
       return True;
     end Has_Provide_Key;
     procedure Add_Require_Key(Key : in String) is
     begin
       Requires.Insert(SHA256S(Key));
     end Add_Require_Key;
     function Has_Require_Key(Key : in String) return Boolean is
     begin
       if Requires.Is_Empty or not Requires.Contains(SHA256S(Key)) then
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
     Execute(nDB,"Create TABLE conflicts (  Name TEXT,  Flags TEXT,  Epoch TEXT,  Version TEXT,  release TEXT,  pkgKey CHAR(65));" );
     Execute(nDB,"Create TABLE db_info (dbVersion Integer, checksum TEXT);" );
     Execute(nDB,"Create TABLE Files (  Name TEXT,  type TEXT,  pkgKey CHAR(65)); " );
     Execute(nDB,"Create TABLE obsoletes (  Name TEXT,  Flags TEXT,  Epoch TEXT,  Version TEXT,  release TEXT,  pkgKey CHAR(65) );" );
     Execute(nDB,"Create TABLE packages (  pkgKey CHAR(65) PRIMARY KEY,  pkgId TEXT,  Name TEXT,  arch TEXT,  Version TEXT,  Epoch TEXT,  release TEXT,  summary TEXT,  description TEXT,  URL TEXT,  time_file Integer,  time_build Integer,  rpm_License TEXT,  rpm_vendor TEXT,  rpm_group TEXT,  rpm_buildhost TEXT,  rpm_sourcerpm TEXT,  Rpm_Header_start Integer,  Rpm_Header_end Integer,  rpm_packager TEXT,  size_package Integer,  size_installed Integer,  size_archive Integer,  location_href TEXT,  location_base TEXT,  checksum_type TEXT);" );
     Execute(nDB,"Create TABLE Provides (  Name TEXT,  Flags TEXT,  Epoch TEXT,  Version TEXT,  release TEXT,  pkgKey CHAR(65) , provideKey CHAR(65) PRIMARY KEY);" );
     Execute(nDB,"Create TABLE packages_Provides ( pkgKey CHAR(65), provideKey CHAR(65));" );
     Execute(nDB,"Create TABLE requires (  Name TEXT,  Flags TEXT,  Epoch TEXT,  Version TEXT,  release TEXT,  pkgkey CHAR(65) , pre BOOLEAN DEFAULT FALSE, requirekey CHAR(65) PRIMARY KEY);" );
     Execute(nDB,"Create TABLE packages_requires ( pkgkey CHAR(65), requirekey CHAR(65) );" );
     Execute(nDB,"Create Index fileNames ON Files (Name);" );
     Execute(nDB,"Create Index packageId ON packages (pkgId);" );
     Execute(nDB,"Create Index packageName ON packages (Name);" );
     Execute(nDB,"Create Index pkgconflicts on conflicts (pkgKey);" );
     Execute(nDB,"Create Index pkgFiles ON Files (pkgKey);" );
     Execute(nDB,"Create Index pkgobsoletes on obsoletes (pkgKey);" );
     Execute(nDB,"Create Index pkgProvides on Provides (pkgKey);" );
     Execute(nDB,"Create Index pkgrequires on requires (pkgKey);" );
     Execute(nDB,"Create Index ProvidesName ON Provides (Name);" );
     Execute(nDB,"Create Index requiresName ON requires (Name);" );
     Execute(nDB,"Create Index packages_Provides_providekey ON packages_Provides(provideKey);" );
     Execute(nDB,"Create Index packages_Provides_pkgkey ON packages_Provides(pkgKey);" );
     Execute(nDB,"Create Index packages_requires_requirekey ON packages_requires(requireKey);" );
     Execute(nDB,"Create Index packages_requires_pkgkey ON packages_requires(pkgKey);" );
     Execute(nDB,"Create TRIGGER removals AFTER DELETE ON packages  BEGIN   DELETE FROM Files WHERE pkgKey = old.pkgKey;   DELETE FROM requires WHERE pkgKey = old.pkgKey;   DELETE FROM Provides WHERE pkgKey = old.pkgKey;   DELETE FROM conflicts WHERE pkgKey = old.pkgKey;   DELETE FROM obsoletes WHERE pkgKey = old.pkgKey;  END;");
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
         DB := GNATCOLL.SQL.Sqlite.Setup (FileName & Suffix & "dbName.db");
         if Prepare_Directories(FileName) and Create_DB then
            Initialized := True;
         else
            pragma Debug(Put_Line("DB failed"));
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
