with Internal_Codecs; use Internal_Codecs; 
with POSIX.Files; 
with arpm_rpm_leaders; use arpm_rpm_leaders; 
with arpm_rpm_headers; use arpm_rpm_headers; 
with arpm_rpm_rpmhdrindexs; use arpm_rpm_rpmhdrindexs; 
with ada.text_io ; use ada.Text_IO; 
package body arpm_rpm_files is 
    function htonl32(number : in four_byte_number) return four_byte_number;
        pragma Import(C, htonl32, "htonl");
    function htonl16(number : in two_byte_number) return two_byte_number;
        pragma Import(C, htonl16, "htonl");

    function Read_String (This : in out RPM_File; items : in Integer := 1; Max_Length : in Integer := 1; Debug : Boolean := False ) return Universal_String is 
        Str : Universal_String; 
        buffer : dummy_byte; 
        Raw : String(1..Max_Length) := (others => Character'Val(0));
        Iter : Natural := 1; 
    begin
        for I in 1..items loop
            loop 
                dummy_byte'Read(This.Stream, buffer) ;
                This.Offset := This.Offset + buffer'Size;
                if buffer = 16#0# then 
                    Str := String_To_US(Raw(1..Iter));  
                    if Debug then
                        Put_Line("");
                        if Length(Str) /= Max_Length then
                            Put_Line("WARNING: SIZE IS NOT EQUAL OFFSET"); 
                        end if;
                    end if;
                    exit; 
                end if; 
                if Debug then
                    Put(Character'Val(buffer));
                end if;
                Raw(Iter) := Character'Val(buffer); 
                Iter := Iter + 1; 
            end loop;
        end loop;
        return Str; 
    end Read_String;

    function Read_i18_String (This : in out RPM_File; items : in Integer := 1; Max_Length : in Integer := 1; Debug : Boolean := False ) return Universal_String is 
        -- FIXME 
        -- Index records with type RPM_I18NSTRING_TYPE shall always have a count of 1. 
        -- The array entries in an index of type RPM_I18NSTRING_TYPE correspond to the locale names contained in the RPMTAG_HDRI18NTABLE index.
        -- Need to find what RPM_I18NSTRING_TYPE mean and how to avoid loosing of the byte
        Str : Universal_String; 
        buffer : dummy_byte; 
        Raw : String(1..Max_Length) := (others => Character'Val(0));
        Iter : Natural := 1; 
    begin
        for I in 1..items loop
            loop 
                dummy_byte'Read(This.Stream, buffer) ;
                This.Offset := This.Offset + buffer'Size;
                if buffer = 16#0# then 
                    Str := String_To_US(Raw(1..Iter));  
                end if; 
                -- exit only if all bytes has been read
                -- see FIXME above
                exit when Length(Str) = Max_Length;
                if Debug then
                    Put(Character'Val(buffer));
                end if;
                Raw(Iter) := Character'Val(buffer); 
                Iter := Iter + 1; 
            end loop;
        end loop;
        return Str; 
    end Read_i18_String;

    procedure Skip_String(This : in out RPM_File; items : in Integer := 1 ) is 
        buffer : dummy_byte; 
    begin
        for I in 1..items loop 
            loop 
                dummy_byte'Read(This.Stream, buffer) ;
                This.Offset := This.Offset + buffer'Size;
                if  buffer = 16#0# then 
                    exit;
                end if;
            end loop;
        end loop;
    end Skip_String; 

    procedure Skip_String_Array(This : in out RPM_File; items : in Integer) is 
    begin
        for I in 1..Items loop
            Skip_String(This); 
        end loop;
    end Skip_String_Array; 

    procedure Skip_Bin(This : in out RPM_File; items : in Integer := 1 ) is 
        buffer : dummy_byte; 
    begin
        for I in 1..items loop
            dummy_byte'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size;
        end loop;
    end Skip_Bin;

    function read_Int16(This : in out RPM_File;  items : in Integer := 1) return Integer is 
        buffer : two_byte_number := 0; 
    begin
        for I in 1..items loop
            two_byte_Number'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size;
        end loop;
        return INteger(htonl16(buffer));
    end read_Int16; 
    
    procedure Skip_Int16(This : in out RPM_File;  items : in Integer := 1) is 
        buffer : Integer; 
    begin
        buffer := read_Int16(This, items);
    end Skip_Int16; 

    function Read_Int32(This : in out RPM_File; items : in Integer := 1) return Long_Long_Integer is 
        buffer : four_byte_Number;
    begin
        for I in 1..items loop
            four_byte_Number'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size;
        end loop;
        return Long_Long_Integer(htonl32(buffer)); 
    end Read_Int32; 

    procedure Skip_Int32(This : in out RPM_File; items : in Integer := 1) is 
        buffer : Long_Long_Integer;
    begin
        buffer := read_Int32(This, items);
    end Skip_Int32; 

    procedure Parse(This : in out RPM_File) is 
        Number_Of_Indexes : Natural := 0; 
    begin
        this.read_leader; 
        this.read_header(Signature => True);
        this.read_header(Signature => False);
    end Parse;

    procedure Read_Leader(This : in out RPM_File) is 
       Leader : RPM_Leader; 
    begin
        pragma Debug(Put_Line("DEBUG: parsing  RPM_Leader")); 
        RPM_Leader'Read(This.Stream, Leader);
        This.Offset := This.Offset + leader'Size; 
        pragma Debug (Put_Line("DEBUG: Package name: " & Name(Leader)));
        pragma Debug (Put_Line("DEBUG: Package format: " & rpmtypes'Image(rpmtype(Leader))));
    end Read_Leader; 

    procedure Read_Header (This : in out RPM_File; Signature : Boolean := False) is 
       Header : RPM_Header; 
       buffer : dummy_byte := 0; 
       indexes : index_array_access; 
    begin
        pragma Debug(Put_Line("DEBUG: looking for RPM header")); 
        -- The header structure header always starts with a three-byte magic number: 8e ad e8
        header_loop:
        loop 
            for I in LABELONE'First..LABELONE'Last loop
                if buffer = LABELONE(I) then 
                    exit header_loop when I = 3; 
                    dummy_byte'Read(This.Stream, buffer) ;
                    This.Offset := This.Offset + buffer'Size;
                else
                    dummy_byte'Read(This.Stream, buffer) ;
                    This.Offset := This.Offset + buffer'Size;
                    exit; 
                end if; 
            end loop; 
        end loop header_loop; 
        rpm_header'Read(This.Stream, header);
        This.Offset := This.Offset + header'Size;
        indexes := This.Read_Indexes(arpm_rpm_headers.indexes(header));
        -- FIXME not implemented yet
        if not Signature then 
            This.Read_Payload(indexes); 
        end if;
    end Read_Header; 

    function Read_Indexes(This : in out RPM_File; count : in Integer) return index_array_access is 
        indexes : index_array_access := new index_array(1..count);
        index : rpmhdrindex; 
    begin
        pragma Debug(Put_Line("DEBUG: parsing rpmhdrindex")); 
        for I in 1..indexes'Length loop
            rpmhdrindex'Read(This.Stream, index);
            This.Offset := This.Offset + index'Size;
            -- binary first?
            if I = 1 then 
                indexes(indexes'Last) := index; 
            else
                indexes(I-1) := index;
            end if;
        end loop;
        return indexes; 
    end Read_Indexes;

    procedure Read_Payload(This: in out RPM_File; indexes : in index_array_access) is 
        -- payload store starts in the cursor position and we don't have to seek 
        cf : format_type; 
        cof : Integer; 
        ctag : tags_type; 
    begin
        pragma Debug (Put_Line ("Reading payload") ); 
        for I in 1..indexes'Length loop
            cf :=  format(indexes(I));
            cof := data_offset(indexes(I)); 
            ctag := tag(indexes(I));
            -- Put_Line("Read: " & tags_type'Image(ctag));
            case cf is 
                when RPM_BIN_TYPE => 
                    case ctag is 
                        when others =>
                            Skip_Bin(This, data_Items(indexes(I))); 
                            null;
                    end case;
                when RPM_STRING_ARRAY_TYPE =>
                    case ctag is 
                        when others =>
                            Skip_String_Array(This, data_items(indexes(I))); 
                    end case;
                when RPM_INT32_TYPE => 
                    case ctag is 
                        when RPMTAG_FILEMTIMES => 
                            Put_Line("RPMTAG_FILEMTIMES");
                            Skip_int32(This, data_items(indexes(I))); 
                        when RPMTAG_BUILDTIME =>
                            This.Build_Time := Read_Int32(This, Data_Items(indexes(I)));
                        when RPMTAG_SIZE => 
                            This.Size := Read_Int32(This, Data_Items(indexes(I)));
                        when others => 
                            -- Put_Line(tags_type'Image(ctag));
                            Skip_int32(This, data_items(indexes(I))); 
                    end case;
                when RPM_INT16_TYPE => 
                    case ctag is 
                        when others => 
                            Skip_int16(This, data_items(indexes(I))); 
                    end case;
                when RPM_STRING_TYPE => 
                    -- read name
                    case ctag is 
                        when RPMTAG_NAME =>
                            This.Name := Read_i18_String(
                                this, 
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_VERSION => 
                            This.Version := Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I))); 
                        when RPMTAG_RELEASE => 
                            This.Release := Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_DISTRIBUTION => 
                            This.Distribution := Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_BUILDHOST => 
                            This.Build_Host := Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_LICENSE => 
                            This.License :=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_postunprog => 
                            This.postunprog:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_cookie => 
                            This.cookie:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_packager => 
                            This.packager:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_PAYLOADFORMAT => 
                            This.PAYLOADFORMAT:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_PAYLOADCOMPRESSOR => 
                            This.PAYLOADCOMPRESSOR:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_PAYLOADFLAGS => 
                            This.PAYLOADFLAGS:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_VENDOR => 
                            This.Vendor:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_optflags => 
                            This.optflags:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_postinprog => 
                            This.postinprog:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_Arch => 
                            This.Arch:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_OS => 
                            This.OS:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_URL => 
                            This.URL:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_PLATFORM => 
                            This.Platform :=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_RPMVERSION => 
                            This.RPM_Version:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_SOURCERPM => 
                            This.SRPM:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when others =>
                            pragma Debug(Put_Line("READER OF " & tags_type'Image(ctag) & " IS NOT IMPLEMETED. OFFSET=" & This.Offset'Img));
                            Skip_String(this, data_items(indexes(I)));
                    end case;
                when RPM_I18NSTRING_TYPE =>
                    case ctag  is 
                        when RPMTAG_Group => 
                            This.Group:=  Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when RPMTAG_DESCRIPTION => 
                            This.Description := Read_i18_String(
                                this,  
                                data_items(indexes(I)), 
                                data_offset(indexes(I+1)) - data_offset(indexes(I))); 
                        when RPMTAG_SUMMARY =>
                            This.Summary := Read_String(this,  data_items(indexes(I)), data_offset(indexes(I+1)) - data_offset(indexes(I)));
                        when others =>
                            pragma Debug(Put_Line("READER OF " & tags_type'Image(ctag) & " IS NOT IMPLEMETED. OFFSET=" & This.Offset'Img));
                            Skip_String(this, data_items(indexes(I)));
                    end case;
                when others => 
                    Put_Line("TYPE:" & format_type'Image(cf));
            end case;
        end loop;
            pragma Debug(Put_Line("Name: " & US_To_String(This.Name)));
            pragma Debug(Put_Line("Version: " & US_To_String(This.Version)));
            pragma Debug(Put_Line("Release: " & US_To_String(This.Release)));
            pragma Debug(Put_Line("Summary: " & US_To_String(This.Summary)));
            pragma Debug(Put_Line("Description: " & US_To_String(This.Description)));
            pragma Debug(Put_Line("Build time: " & This.Build_Time'Img));
            pragma Debug(Put_Line("Size:" & This.Size'Img));
            pragma Debug(Put_Line("Build host: " & US_To_String(This.Build_host)));
            pragma Debug(Put_Line("Licanse:" & US_To_String(This.License)));
            pragma Debug(Put_Line("Vendor: " & US_To_String(This.Vendor)));
            pragma Debug(Put_Line("Group: " & US_To_String(This.Group)));
            pragma Debug(Put_Line("PAYLOADFLAGS: " & US_To_String(This.PAYLOADFLAGS)));
            pragma Debug(Put_Line("PAYLOADCOMPRESSOR: " & US_To_String(This.PAYLOADCOMPRESSOR)));
            pragma Debug(Put_Line("PAYLOADFORMAT: " & US_To_String(This.PAYLOADFORMAT)));
            pragma Debug(Put_Line("packager: " & US_To_String(This.packager)));
            pragma Debug(Put_Line("cookie: " & US_To_String(This.cookie)));
            pragma Debug(Put_Line("optflags: " & US_To_String(This.optflags)));
            pragma Debug(Put_Line("postunprog: " & US_To_String(This.postunprog)));
            pragma Debug(Put_Line("postinprog: " & US_To_String(This.postinprog)));
            pragma Debug(Put_Line("Arch: " & US_To_String(This.Arch)));
            pragma Debug(Put_Line("OS: " & US_To_String(This.OS)));
            pragma Debug(Put_Line("URL: " & US_To_String(This.URL)));
            pragma Debug(Put_Line("SRPM: " & US_To_String(This.SRPM)));
            pragma Debug(Put_Line("Platform: " & US_To_String(This.Platform)));
            pragma Debug(Put_Line("RPM_Version: " & US_To_String(This.RPM_Version)));
    end Read_Payload; 
    

    package body Constructors is 
        function  Create(Filename : Universal_String) return rpm_file_access is 
            use POSIX.Files; 
            RPM : rpm_file_access := new rpm_file; 
            IO_ERROR : exception; 
        begin
            if not Is_File(posix.TO_POSIX_String(US_To_String(Filename))) then
                raise IO_ERROR; 
            end if;
            Ada.Streams.Stream_IO.Open
                (File => RPM.File,
                Name => US_To_String(Filename),
                Mode => Ada.Streams.Stream_IO.In_File);
            RPM.Stream := Ada.Streams.Stream_IO.Stream (RPM.File);
            return RPM;
        end Create; 
    end Constructors; 


end arpm_rpm_files; 

