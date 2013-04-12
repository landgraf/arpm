package ARPM_RPMs is 
    type ARPM_RPM is tagged limited private;
    type ARPM_RPM_Access is access all ARPM_RPM; 
    private
    type ARPM_RPM is tagged limited record
        Package_Name : Unbounded_String := Null_Unbounded_String;
        Package_Version : Unbounded_String := Null_Unbounded_String;
        Package_Arch : Unbounded_String := Null_Unbounded_String; 
    end record;
end ARPM_RPMs;
