with Ada.Text_IO; use Ada.Text_IO; 
         with  Ada.Unchecked_Conversion;
package body arpm_rpm_rpmhdrindexs is 

    function htonl(Index : in four_byte_number) return four_byte_number; 
        pragma Import (C, htonl, "htonl");

    function Tag(index : in rpmhdrindex) return tags_type is 
         function Extract_Tag is 
              new Ada.Unchecked_Conversion (four_byte_number, tags_type);  
    begin
        return Extract_Tag(htonl(index.tag));
    exception
        when CONSTRAINT_ERROR =>
            Put_Line("Exception tag: " & htonl(index.tag)'Img); 
            raise; 
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

    function Format(index : in rpmhdrindex) return format_type  is 
         function Extract_Format is 
              new Ada.Unchecked_Conversion (four_byte_number, format_type);  
    begin
        return Extract_format(htonl(index.format));
    exception
        when CONSTRAINT_ERROR =>
            Put_Line("Exception format: " & htonl(index.format)'Img); 
            raise;
    end Format; 

    function Format(index : in rpmhdrindex) return Integer is 
    begin
        return Integer(htonl(index.format));
    end format;
    function Data_Items( Index : in rpmhdrindex) return Integer is 
    begin
        return  Integer(htonl(index.number_of_data_items));
    end Data_Items;
    function Data_Offset ( Index : in rpmhdrindex) return Integer is 
    begin
        return  Integer(htonl(index.data_offset));
    end Data_offset;
end arpm_rpm_rpmhdrindexs; 

