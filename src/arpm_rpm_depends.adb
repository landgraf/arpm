package body arpm_rpm_depends is 

    function Name(This: in out rpm_depend) return Unbounded_String is (This.Name);
    function version(This: in out rpm_depend) return Unbounded_String is (This.Version); 
    procedure Set_Flags( This : in out rpm_depend; flags : in Long_Long_Integer ) is 
    begin
        This.Flags := flags; 
    end Set_Flags; 
    procedure Set_Name(This : in out rpm_depend; Name : in Unbounded_String) is 
    begin
        This.Name := Name; 
    end Set_Name;
    procedure Set_Version(This : in out rpm_depend; Version : in Unbounded_String) is 
    begin
        This.Version := Version; 
    end Set_Version;
    procedure Free_depends(This : in out rpm_depends_access) is 
    begin
        Free(This); 
    end Free_Depends; 
end arpm_rpm_depends; 

