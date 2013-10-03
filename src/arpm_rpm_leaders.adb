package body arpm_rpm_leaders is 

    function Name(This : rpm_leader) return String is 
    begin
        return To_Ada(This.Name); 
    end Name;
    function RPMType ( This : rpm_leader) return rpmtypes is 
    begin
        return rpmtypes'Val(Integer(This.rpmtype));
    end RPMType; 
    function Magic ( This : rpm_leader ) return String is 
    begin
        return This.Magic'Img; 
    end Magic; 
end arpm_rpm_leaders; 

