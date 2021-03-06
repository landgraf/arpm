with Ada.Directories; use Ada.Directories;
with POSIX.Files;
with ARpm_Files_Handlers; use ARpm_Files_Handlers;

package body ARPM_Walkers is
   procedure Walk_Directory
     (Directory : in String := ".";
     Pattern   : in String := "*.rpm") -- empty pattern = all file Names/subdirectory Names
   is
     Search  : Search_Type;
     Dir_Ent : Directory_Entry_Type;
   begin
     Start_Search (Search, Directory, Pattern);
     -- FIXME Name
     while More_Entries (Search) loop
       Get_Next_Entry (Search, Dir_Ent);
       ARpm_Files_Handlers.Files.put(To_Unbounded_String(Full_Name (Dir_Ent)));
     end loop;
     End_Search (Search);
   end Walk_Directory;

   procedure Start(Dir : in String) is
   begin
     if POSIX.Files.Is_Directory(POSIX.To_POSIX_String(Dir)) then
       Walk_Directory(Directory => Dir);
     end if;
       ARpm_Files_Handlers.Files.finish;
   end Start;
end ARPM_Walkers;
