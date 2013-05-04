with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with ARPM_Walkers; use ARPM_Walkers;
with ARPM_Processors; use ARPM_Processors;
with ARPM_Files_Handlers;
with ARPM_C_Bridge;
with Internal_Codecs; use Internal_Codecs;

procedure Createrepo is 
    function Get_Dir return String is 
    begin
        return Argument(1);
    exception
        when CONSTRAINT_ERROR => 
            Put_Line("Please specify directory name");
            OS_Exit(1);
    end Get_Dir;
    threads : Positive := 8;
    pool : array (1..threads) of ARPM_Processor;
    ERROR_RPM_CONFIG : exception;
    Dir : constant String := Get_Dir;
begin
    if Integer(ARPM_C_Bridge.Read_Config) /= 0 then
        raise ERROR_RPM_CONFIG;
    end if;
    ARPM_Files_Handlers.DB.Init_DB(Dir);
    Start(Dir);
    ARPM_Files_Handlers.Workers.Is_Empty;
    ARPM_Files_Handlers.DB.Free;
end Createrepo;
