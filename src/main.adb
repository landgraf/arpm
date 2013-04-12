with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with ARPM_Walkers; use ARPM_Walkers;
with ARPM_Processors; use ARPM_Processors;
procedure Main is 
    threads : Positive := 1;
    pool : array (1..threads) of ARPM_Processor;
begin
    if Ada.Command_Line.Argument_Count /= 1 then
        Put_Line("Directory name is not specified");
        OS_Exit(1);
    end if;
    Put_Line(Argument(1));
    Start(Argument(1));
end Main;
