package body arpm_rpm_depends is 

    function Name(This: in out rpm_depend) return Universal_String is (This.Name);
    function version(This: in out rpm_depend) return Universal_String is (This.Version); 
    procedure Set_Flags( This : in out rpm_depend; flags : in Long_Long_Integer ) is 
    begin
        This.Flags := flags; 
    end Set_Flags; 
    procedure Set_Name(This : in out rpm_depend; Name : in Universal_String) is 
    begin
        This.Name := Name; 
    end Set_Name;
    procedure Set_Version(This : in out rpm_depend; Version : in Universal_String) is 
    begin
        This.Version := Version; 
    end Set_Version;
end arpm_rpm_depends; 

