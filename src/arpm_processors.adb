with Ada.Strings.Unbounded; use Ada.Strings.Unbounded; 
with arpm_c_bridge;
with ARPM_RPM_INternals; use ARPM_RPM_Internals;
with ARPM_Files_Handlers;
with Ada.Text_Io ; use Ada.Text_IO;
with POSIX.Files; use POSIX.Files;
with POSIX.IO; use POSIX.IO;
with POSIX; use POSIX;

package body ARPM_Processors is 
    

    procedure Process(FileName : String) is 
    begin
        if Is_File(To_POSIX_String(FileName)) then
            for I in 1..3 loop 
                Put_Line("=========================");
            end loop;
            if arpm_c_bridge.test(FileName) /= 0 then
                Put_Line("Error while reading " & Filename);
            end if;
        end if;
    end Process;

    task body ARPM_Processor is 
        FileName : Unbounded_String;
    begin
        loop
            ARPM_Files_Handlers.Files.Get(FileName);
            if FileName = Null_Unbounded_String then
                exit;
            end if;
            Process(To_String(FileName));
        end loop;
    end ARPM_Processor;
end ARPM_Processors;
