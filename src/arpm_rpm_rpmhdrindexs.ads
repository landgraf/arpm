package arpm_rpm_rpmhdrindexs is 
    type rpmhdrindex is private; 
    type rpmhdrindex_access is access all rpmhdrindex; 
    function Tag(index : in rpmhdrindex) return String; 
    function Format(index : in rpmhdrindex) return String; 

    private
    type four_byte_number is range 0..2**(4*8)-1;
    for four_byte_number'Size use 32; 
    type tags_type is (
        RPMTAG_HEADERSIGNATURES, 
        RPMTAG_HEADERIMMUTABLE, 
        RPMTAG_HEADERI18NTABLE); 
    for tags_type use (
        RPMTAG_HEADERSIGNATURES => 62, 
        RPMTAG_HEADERIMMUTABLE => 63, 
        RPMTAG_HEADERI18NTABLE => 100); 
    type format_type is 
        (RPM_NULL_TYPE, 
         RPM_CHAR_TYPE, 
         RPM_INT8_TYPE, 
         RPM_INT16_TYPE, 
         RPM_INT32_TYPE, 
         RPM_INT64_TYPE, 
         RPM_STRING_TYPE, 
         RPM_BIN_TYPE, 
         RPM_STRING_ARRAY_TYPE, 
         RPM_I18NSTRING_TYPE); 
    for format_type use 
        (RPM_NULL_TYPE => 0, 
         RPM_CHAR_TYPE => 1, 
         RPM_INT8_TYPE => 2, 
         RPM_INT16_TYPE => 3, 
         RPM_INT32_TYPE => 4, 
         RPM_INT64_TYPE => 5, 
         RPM_STRING_TYPE => 6, 
         RPM_BIN_TYPE => 7, 
         RPM_STRING_ARRAY_TYPE => 8 , 
         RPM_I18NSTRING_TYPE =>9
         ); 
    type rpmhdrindex is record
        tag : four_byte_number := 0; 
        format : four_byte_number := 0; 
        data_position  :  four_byte_number := 0; 
        number_of_data_items  : four_byte_number := 0;
    end record; 
    for rpmhdrindex'Size use 16*8; 


end arpm_rpm_rpmhdrindexs; 

