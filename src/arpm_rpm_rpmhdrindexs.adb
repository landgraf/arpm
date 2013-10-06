with Ada.Text_IO; use Ada.Text_IO;
         with  Ada.Unchecked_Conversion;
package body arpm_rpm_rpmhdrIndexs is

    function htonl(Index : in four_byte_number) return four_byte_number;
        pragma Import (C, htonl, "htonl");

    function Tag(Index : in rpmhdrIndex) return tags_type is
         function Extract_Tag is
              new Ada.Unchecked_Conversion (four_byte_number, tags_type);
    begin
        return Extract_Tag(htonl(Index.tag));
    exception
        when Constraint_Error =>
            Put_Line("Exception tag: " & htonl(Index.tag)'Img);
            raise;
    end Tag;

    function Format(Index : in rpmhdrIndex) return String is
         function Extract_Format is
              new Ada.Unchecked_Conversion (four_byte_number, Format_type);
    begin
        return Format_type'Image(Extract_Format(htonl(Index.Format)));
    exception
        when Constraint_Error =>
            Put_Line("Exception Format: " & htonl(Index.Format)'Img);
            raise;
    end Format;

    function Format(Index : in rpmhdrIndex) return Format_type  is
         function Extract_Format is
              new Ada.Unchecked_Conversion (four_byte_number, Format_type);
    begin
        return Extract_Format(htonl(Index.Format));
    exception
        when Constraint_Error =>
            Put_Line("Exception Format: " & htonl(Index.Format)'Img);
            raise;
    end Format;

    function Format(Index : in rpmhdrIndex) return Integer is
    begin
        return Integer(htonl(Index.Format));
    end Format;
    function Data_Items( Index : in rpmhdrIndex) return Integer is
    begin
        return  Integer(htonl(Index.number_of_data_items));
    end Data_Items;
    function Data_Offset ( Index : in rpmhdrIndex) return Integer is
    begin
        return  Integer(htonl(Index.Data_Offset));
    end Data_Offset;
end arpm_rpm_rpmhdrIndexs;

