with Ada.Strings.Unbounded; use Ada.Strings.Unbounded; 
with Ada.Exceptions;
with arpm_c_bridge;
with ARPM_RPM_INternals; use ARPM_RPM_Internals;
with ARPM_Files_Handlers;
with Ada.Text_Io ; use Ada.Text_IO;
with POSIX.Files; use POSIX.Files;
with POSIX.IO; use POSIX.IO;
with POSIX; use POSIX;
with arpm_db_containers;
with ARPM_DB_Types;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;


with Internal_Codecs; use Internal_Codecs;

package body ARPM_Processors is 
    

    task body ARPM_Processor is 
        FileName : Unbounded_String;
        MyRPM : My_RPM_Struct_Access;
        RPM : ARPM_RPM_Access;
        DC : Database_Description;
        DB : Database_Connection;
        DB_ERROR : exception;
    begin
        ARPM_Files_Handlers.DB.Get_DB(DC);
           DB := GNATCOLL.SQL.Exec.Get_Task_Connection
                   (Description  => DC);
           Execute(DB, "PRAGMA synchronous = OFF");
        ARPM_Files_Handlers.Workers.Increase;
        pragma Debug(Put_Line("Start worker...."));
        loop
            ARPM_Files_Handlers.Files.Get(FileName);
            if FileName = Null_Unbounded_String then
                exit;
            end if;
            if Is_File(To_POSIX_String(To_String(FileName))) then
                    MyRPM := arpm_c_bridge.constructors.create(To_String(FileName));
                if INteger(MyRPM.Error) = 0 then
                    RPM := arpm_c_bridge.convert(MyRPM);
                    try:
                    begin
                        null;
                        arpm_db_containers.save(RPM, DB);
                        --arpm_db_containers.save_requires(RPM, DB);
                        --arpm_db_containers.save_provides(RPM, DB);
                    exception
                        when SQL_Event: others =>
                            pragma Debug(Put_Line("Failed to save " & To_String(RPM.Name)));
                            pragma Debug(Put_Line("Reason " & Ada.Exceptions.Exception_Information(SQL_Event)));
                    end try;
                    ARPM_RPM_Internals.Free(RPM);
                end if;
                ARPM_C_Bridge.Free(MyRPM);
            end if;
        end loop;
        pragma Debug(Put_Line("Commit..."));
        -- DB.Handler.Commit;
        -- DB.Handler.Close;
        ARPM_Files_Handlers.Workers.Decrease;
    exception
        when The_Event: others =>
            pragma Debug(Put_Line("Processor:" & Ada.Exceptions.Exception_Message(The_Event)));
            pragma Debug(Put_Line (Ada.Exceptions.Exception_Information(The_Event)));
            ARPM_Files_Handlers.Workers.Decrease;
    end ARPM_Processor;
end ARPM_Processors;
