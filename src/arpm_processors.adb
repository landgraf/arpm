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


package body ARPM_Processors is 
    

    function Process(FileName : in String) return My_RPM_Struct_Access is 
        MyRPM : My_RPM_Struct_Access;
        Status : Integer := Integer'Last;
    begin
            MyRPM := arpm_c_bridge.constructors.create(FileName);
            arpm_c_bridge.test(MyRPM,status);
        return MyRPM;
    end Process;

    task body ARPM_Processor is 
        FileName : Unbounded_String;
        MyRPM : My_RPM_Struct_Access;
        DB : constant Session_Type := ARPM_Files_Handlers.Sessions.Get_Session;
    begin
        ARPM_Files_Handlers.Workers.Increase;
        loop
            ARPM_Files_Handlers.Files.Get(FileName);
            if FileName = Null_Unbounded_String then
                exit;
            end if;
            if Is_File(To_POSIX_String(To_String(FileName))) then
                MyRPM := Process(To_String(FileName));
                if INteger(MyRPM.Error) = 0 then
                    arpm_db_containers.save(MyRPM, DB);
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
