with Ada.Text_IO; use Ada.Text_IO; 
package body arpm_rpm_headers is 

    function Version (This : in RPM_Header) return Integer is 
        function htons(Index : in dummy_byte) return dummy_byte; 
        pragma Import (C, htons, "htons");
    begin
        return Integer(htons(This.Version)); 
    end Version; 
    function Indexes (This : in RPM_Header) return Integer is 
        function htonl(Index : in four_byte_number) return four_byte_number; 
        pragma Import (C, htonl, "htonl");
    begin
        return Integer(htonl(This.Indexes)); 
    end Indexes; 

    function Data_Bytes (This : in RPM_Header) return Integer is 
        function htonl(Index : in four_byte_number) return four_byte_number; 
        pragma Import (C, htonl, "htonl");
    begin
        return Integer(htonl(This.data_bytes));
    end Data_Bytes; 
end arpm_rpm_headers; 

