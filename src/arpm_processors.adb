with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Exceptions;
with Ada.Text_IO ; use Ada.Text_IO;
with POSIX.Files; use POSIX.Files;
with POSIX; use POSIX;
with Arpm_Db_Containers;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with ARpm_Files_Handlers;
with ARPM_DB_Handlers;
with ARPM_RPM_Files; use ARPM_RPM_Files;

package body ARPM_Processors is


   task body ARPM_Processor is
     FileName : Unbounded_String;
     RPM : RPM_File_Access;
     DC : Database_Description;
     DB : Database_Connection;
     Transaction : Boolean;
     DB_Error : exception;
   begin
     ARPM_DB_Handlers.DB.Get_DB(DC);
       DB := GNATCOLL.SQL.Exec.Get_Task_Connection
            (Description  => DC);
       Execute(DB, "PRAGMA synchronous = OFF");
     ARpm_Files_Handlers.Workers.Increase;
     pragma Debug(Put_Line("Start worker...."));
     loop
       ARpm_Files_Handlers.Files.Get(FileName);
       if FileName = Null_Unbounded_String then
         exit;
       end if;
       if Is_File(To_POSIX_String(To_String(FileName))) then
            RPM := ARPM_RPM_FIles.Constructors.Create(To_String(FileName));
            RPM.Parse;
            try:
            begin
              Transaction := Start_Transaction(DB);
              if not Transaction then
                raise DB_Error;
              end if;
              Arpm_Db_Containers.Save(RPM, DB);
              Arpm_Db_Containers.Save_Requires(RPM, DB);
              Arpm_Db_Containers.Save_Provides(RPM, DB);
              Commit(DB);
            exception
              when SQL_Event: others =>
                pragma Debug(Put_Line("Failed to Save " & To_String(RPM.Name)));
                pragma Debug(Put_Line("Reason " & Ada.Exceptions.Exception_Information(SQL_Event)));
            end try;
            ARPM_RPM_Files.Close(RPM);
       end if;
     end loop;
     pragma Debug(Put_Line("Commit..."));
     Free(DB);
     ARpm_Files_Handlers.Workers.Decrease;
     if ARpm_Files_Handlers.Workers.Is_Empty then
       ARPM_DB_Handlers.DB.Free;
     end if;
   exception
     when The_Event: others =>
       pragma Debug(Put_Line("Processor:" & Ada.Exceptions.Exception_Message(The_Event)));
       pragma Debug(Put_Line (Ada.Exceptions.Exception_Information(The_Event)));
       ARpm_Files_Handlers.Workers.Decrease;
   end ARPM_Processor;
end ARPM_Processors;
