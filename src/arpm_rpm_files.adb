with Internal_Codecs; use Internal_Codecs; 
with POSIX.Files; 
with arpm_rpm_leaders; use arpm_rpm_leaders; 
with arpm_rpm_headers; use arpm_rpm_headers; 
with arpm_rpm_rpmhdrindexs; use arpm_rpm_rpmhdrindexs; 
with ada.text_io ; use ada.Text_IO; 
package body arpm_rpm_files is 
    function Read_String (This : in out RPM_File; offset : in Integer := 0 ) return Universal_String is 
        Str : Universal_String; 
        buffer : dummy_byte; 
    begin
        loop 
            dummy_byte'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size;
            exit when buffer = 16#0#; 
        end loop;
        return Str; 
    end Read_String;

    procedure Skip_String(This : in out RPM_File; offset : in Integer := 0 ) is 
        buffer : dummy_byte; 
    begin
        loop 
            dummy_byte'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size;
            -- Put(Character'Val(buffer));
            if  buffer = 16#0# then 
                -- Put_Line(";");
                exit;
            end if;
            -- exit when buffer = 16#0#; 
        end loop;
    end Skip_String; 

    procedure Skip_String_Array(This : in out RPM_File; Count : in Integer) is 
    begin
        for I in 1..Count loop
            Skip_String(This); 
        end loop;
    end Skip_String_Array; 

    procedure Skip_Bin(This : in out RPM_File; count : in Integer := 1 ) is 
        buffer : dummy_byte; 
    begin
        if count /= 1 then 
            Put_Line("Skipping " & count'Img & " bytes" );
        end if;
        for I in 1..count loop
            dummy_byte'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size;
        end loop;
    end Skip_Bin;

    procedure Skip_Int16(This : in out RPM_File) is 
    begin
        for I in 1..2 loop
            Skip_Bin(This); 
        end loop;
    end Skip_Int16; 

    procedure Skip_Int32(This : in out RPM_File) is 
    begin
        for I in 1..2 loop
            Skip_Int16(This); 
        end loop;
    end Skip_Int32; 

    procedure Parse(This : in out RPM_File) is 
        Number_Of_Indexes : Natural := 0; 
    begin
        this.read_leader; 
        -- skip signature for now
        this.read_header(Signature => True);
        this.read_header(Signature => False);
        --this.Read_Hdrindex;
        --this.Read_Store; 
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
            Put_line(cof'Img);
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
                        when others => 
                            Skip_int32(This); 
                    end case;
                when RPM_INT16_TYPE => 
                    case ctag is 
                        when others => 
                            Skip_int16(This); 
                    end case;
                when RPM_STRING_TYPE => 
                    -- read name
                    case ctag is 
                        when RPMTAG_NAME =>
                            This.Name := Read_String(this);
                        --when RPMTAG_VERSION => 
                        --    This.Version := Read_String(this); 
                        when others =>
                            -- pragma Debug(Put_Line("READER OF " & tags_type'Image(ctag) & " IS NOT IMPLEMETED. OFFSET=" & This.Offset'Img));
                            Skip_String(this);
                            -- pragma Debug(Put_Line("OFFSET=" & This.Offset'Img));
                    end case;
                when RPM_I18NSTRING_TYPE =>
                    case ctag  is 
                        when others =>
                            Skip_String(this);
                    end case;
                when others => 
                    Put_Line("TYPE:" & format_type'Image(cf));
            end case;
        end loop;
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

