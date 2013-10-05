with Ada.Text_IO; use Ada.Text_IO; 
         with  Ada.Unchecked_Conversion;
package body arpm_rpm_rpmhdrindexs is 

    function htonl(Index : in four_byte_number) return four_byte_number; 
        pragma Import (C, htonl, "htonl");
    function Tag(index : in rpmhdrindex) return String is 
         function Extract_Tag is 
              new Ada.Unchecked_Conversion (four_byte_number, tags_type);  
    begin
        return tags_type'Image(Extract_Tag(htonl(index.tag)));
    exception
        when CONSTRAINT_ERROR =>
            Put_Line("Exception tag: " & htonl(index.tag)'Img); 
            raise; 
    end Tag; 
    function Tag(Index : in rpmhdrindex) return Integer is 
    begin
        return Integer(htonl(index.tag)); 
    end Tag; 
    function Format(index : in rpmhdrindex) return String is 
         function Extract_Format is 
              new Ada.Unchecked_Conversion (four_byte_number, format_type);  
    begin
        return format_type'Image(Extract_format(htonl(index.format)));
    exception
        when CONSTRAINT_ERROR =>
            Put_Line("Exception format: " & htonl(index.format)'Img); 
            raise;
    end Format; 

    function Format(index : in rpmhdrindex) return Integer is 
    begin
        return Integer(htonl(index.format));
    end format;
end arpm_rpm_rpmhdrindexs; 

