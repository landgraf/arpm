with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with ARPM_Walkers; use ARPM_Walkers;
with ARPM_Processors; use ARPM_Processors;
with Arpm_C_Bridge;
with ARPM_DB_Handlers; -- .DB.Init_DB(Dir);

procedure Createrepo is 
    function Get_Dir return String is 
    begin
        return Argument(1);
    exception
        when Constraint_Error => 
            Put_Line("Please specify directory name");
            OS_Exit(1);
    end Get_Dir;
    threads : constant Positive := 8;
    pool : array (1..threads) of ARPM_Processor;
    Error_RPM_CONFIG : exception;
    Dir : constant String := Get_Dir;
begin
    if Integer(Arpm_C_Bridge.Read_Config) /= 0 then
        raise Error_RPM_CONFIG;
    end if;
    ARPM_DB_Handlers.DB.Init_DB(Dir);
    Start(Dir);
end Createrepo;
