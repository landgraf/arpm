with Ada.Text_IO; use Ada.Text_IO; 
package body arpm_rpm_headers is 

    function Version (This : in RPM_Header) return Integer is 
    begin
        return Integer(This.Version); 
    end Version; 
    function Indexes (This : in RPM_Header) return Integer is 
    begin
        Put_Line("FIXME : " & This.Indexes'Img);
        return Integer(This.Indexes); 
    end Indexes; 

    function Data_Bytes (This : in RPM_Header) return Integer is 
    begin
        return Integer(This.Indexes);
    end Data_Bytes; 
end arpm_rpm_headers; 

