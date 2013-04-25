with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with ARPM_Walkers; use ARPM_Walkers;
with ARPM_Processors; use ARPM_Processors;
with ARPM_C_Bridge;

procedure Main is 
    threads : Positive := 8;
    pool : array (1..threads) of ARPM_Processor;
    ERROR_RPM_CONFIG : exception;
begin
    if Integer(ARPM_C_Bridge.Read_Config) /= 0 then
        raise ERROR_RPM_CONFIG;
    end if;
    if Ada.Command_Line.Argument_Count /= 1 then
        Put_Line("Directory name is not specified");
        OS_Exit(1);
    end if;
    Start(Argument(1));
end Main;
