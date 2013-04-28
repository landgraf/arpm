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
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;


with Internal_Codecs; use Internal_Codecs;

package body ARPM_Processors is 
    

    task body ARPM_Processor is 
        FileName : Unbounded_String;
        MyRPM : My_RPM_Struct_Access;
        RPM : ARPM_RPM_Access := new ARPM_RPM;
        DB : constant Session_Type := ARPM_Files_Handlers.Sessions.Get_Session;
    begin
        ARPM_Files_Handlers.Workers.Increase;
        loop
            ARPM_Files_Handlers.Files.Get(FileName);
            if FileName = Null_Unbounded_String then
                exit;
            end if;
            if Is_File(To_POSIX_String(To_String(FileName))) then
                MyRPM := arpm_c_bridge.constructors.create(To_String(FileName));
                if INteger(MyRPM.Error) = 0 then
                    -- RPM := arpm_c_bridge.convert(MyRPM);
                    arpm_c_bridge.convert(MyRPM => MyRPM, RPM => RPM);
                    arpm_db_containers.save(RPM, DB);
                    arpm_db_containers.save_depends(RPM, DB);
                    arpm_db_containers.save_provides(RPM, DB);
                end if;
                ARPM_C_Bridge.Free(MyRPM);
            end if;
        end loop;
        DB.Commit;
        ARPM_Files_Handlers.Workers.Decrease;
    exception
        when The_Event: others =>
            Put_Line("Processor:" & Ada.Exceptions.Exception_Message(The_Event));
            ARPM_Files_Handlers.Workers.Decrease;
    end ARPM_Processor;
end ARPM_Processors;
