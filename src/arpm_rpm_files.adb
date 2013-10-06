with Internal_Codecs; use Internal_Codecs;
with POSIX.Files;
with arpm_rpm_leaders; use arpm_rpm_leaders;
with arpm_Rpm_Headers; use arpm_Rpm_Headers;
with arpm_rpm_rpmhdrIndexs; use arpm_rpm_rpmhdrIndexs;
with ada.text_io ; use ada.Text_IO;
with Ada.Assertions;  use Ada.Assertions;

package body arpm_Rpm_Files is

    package body Constructors is
        function  Create(FileName : String) return RPM_File_Access is
            use POSIX.Files;
            RPM : RPM_File_Access := new Rpm_File;
            IO_ERROR : exception;
        begin
            if not Is_File(posix.TO_POSIX_String(FileName)) then
                raise IO_ERROR;
            end if;
            Ada.Streams.Stream_IO.Open
                (File => RPM.File,
                Name => FileName,
                Mode => Ada.Streams.Stream_IO.In_File);
            RPM.Stream := Ada.Streams.Stream_IO.Stream (RPM.File);
            return RPM;
        end Create;
    end Constructors;

    function htonl32(number : in four_byte_number) return four_byte_number;
        pragma Import(C, htonl32, "htonl");
    function htonl16(number : in two_byte_number) return two_byte_number;
        pragma Import(C, htonl16, "htonl");

    function Read_i18_String (This : in out Rpm_File; items : in Integer := 1; Max_Length : in Integer := 1; Debug : Boolean := False ) return Unbounded_String is
        Str : Unbounded_String;
        buffer : dummy_byte;
        Raw : String(1..Max_Length) := (others => Character'Val(0));
        Iter : Natural := 1;
    begin
            loop
                dummy_byte'Read(This.Stream, buffer) ;
                This.Offset := This.Offset + buffer'Size/8;
                if buffer = 16#0# then
                    return To_Unbounded_String(Raw(1..Iter));
                end if;
                if Debug then
                    Put_Line("" & Character'Val(buffer));
                end if;
                Raw(Iter) := Character'Val(buffer);
                Iter := Iter + 1;
            end loop;
    end Read_i18_String;

    procedure Skip_String(This : in out Rpm_File; items : in Integer := 1 ) is
        buffer : dummy_byte;
    begin
        for I in 1..items loop
            loop
                dummy_byte'Read(This.Stream, buffer) ;
                This.Offset := This.Offset + buffer'Size/8;
                if  buffer = 16#0# then
                    exit;
                end if;
            end loop;
        end loop;
    end Skip_String;

    procedure Skip_String_Array(This : in out Rpm_File; items : in Integer) is
    begin
        for I in 1..Items loop
            Skip_String(This);
        end loop;
    end Skip_String_Array;

    procedure Skip_Bin(This : in out Rpm_File; items : in Integer := 1 ) is
        buffer : dummy_byte;
    begin
        for I in 1..items loop
            dummy_byte'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size/8;
        end loop;
    end Skip_Bin;

    function read_int16(This : in out Rpm_File;  items : in Integer := 1) return Integer is
        buffer : two_byte_number := 0;
    begin
        for I in 1..items loop
            two_byte_Number'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size/8;
        end loop;
        return Integer(htonl16(buffer));
    end read_int16;

    procedure Skip_int16(This : in out Rpm_File;  items : in Integer := 1) is
        buffer : Integer;
    begin
        buffer := read_int16(This, items);
    end Skip_int16;

    function Read_int32(This : in out Rpm_File; items : in Integer := 1) return Long_Long_Integer is
        buffer : four_byte_Number;
    begin
        for I in 1..items loop
            four_byte_Number'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size/8;
        end loop;
        return Long_Long_Integer(htonl32(buffer));
    end Read_int32;

    procedure Skip_int32(This : in out Rpm_File; items : in Integer := 1) is
        buffer : Long_Long_Integer;
    begin
        buffer := read_int32(This, items);
    end Skip_int32;

    procedure Parse(This : in out Rpm_File) is
        Number_Of_Indexes : Natural := 0;
    begin
        this.read_leader;
        this.read_header(Signature => True);
        this.read_header(Signature => False);
    end Parse;

    procedure Read_Leader(This : in out Rpm_File) is
       Leader : RPM_Leader;
    begin
        pragma Debug(Put_Line("DEBUG: parsing  RPM_Leader"));
        RPM_Leader'Read(This.Stream, Leader);
        This.Offset := This.Offset + leader'Size/8;
        pragma Debug (Put_Line("DEBUG: Package Name: " & Name(Leader)));
        pragma Debug (Put_Line("DEBUG: Package Format: " & rpmtypes'Image(rpmtype(Leader))));
    end Read_Leader;

    procedure Read_Header (This : in out Rpm_File; Signature : Boolean := False) is
       Header : Rpm_Header;
       buffer : dummy_byte := 0;
       Indexes : Index_array_access;
    begin
        pragma Debug(Put_Line("DEBUG: looking for RPM header"));
        -- The header structure header always starts with a three-byte Magic number: 8e ad e8
        header_loop:
        loop
            for I in LABELONE'First..LABELONE'Last loop
                if buffer = LABELONE(I) then
                    exit header_loop when I = 3;
                    dummy_byte'Read(This.Stream, buffer) ;
                    This.Offset := This.Offset + buffer'Size/8;
                else
                    dummy_byte'Read(This.Stream, buffer) ;
                    This.Offset := This.Offset + buffer'Size/8;
                    exit;
                end if;
            end loop;
        end loop header_loop;
        Rpm_Header'Read(This.Stream, header);
        This.Offset := This.Offset + header'Size/8;
        Indexes := This.Read_Indexes(arpm_Rpm_Headers.Indexes(header));
        -- FIXME not implemented yet
        if not Signature then
            This.Read_Payload(Indexes);
        end if;
        Free_Indexes(Indexes);
    end Read_Header;

    function Read_Indexes(This : in out Rpm_File; count : in Integer) return Index_array_access is
        Indexes : Index_array_access := new Index_array(1..count);
        Index : rpmhdrIndex;
    begin
        pragma Debug(Put_Line("DEBUG: parsing rpmhdrIndex"));
        for I in 1..Indexes'Length loop
            rpmhdrIndex'Read(This.Stream, Index);
            This.Offset := This.Offset + Index'Size/8;
            -- binary first?
            if I = 1 then
                Indexes(Indexes'Last) := Index;
            else
                Indexes(I-1) := Index;
            end if;
        end loop;
        return Indexes;
    end Read_Indexes;

    procedure Check_String_Types(This: in out Rpm_File; Index : in rpmhdrIndex; length : in Integer) is
    begin
        case  tag(Index) is
            when RPMTAG_Name =>
                This.Name := Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_Version =>
                This.Version := Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_RELEASE =>
                This.Release := Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_DISTRIBUTION =>
                This.Distribution := Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_BUILDHOST =>
                This.Build_Host := Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_LICENSE =>
                This.License :=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_postunprog =>
                This.postunprog:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_cookie =>
                This.cookie:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_packager =>
                This.packager:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_PAYLOADFormat =>
                This.PAYLOADFormat:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_POSTIN =>
                This.postin :=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_POSTUN =>
                This.postun :=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_PAYLOADCOMPRESSOR =>
                This.PAYLOADCOMPRESSOR:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_PAYLOADFlags =>
                This.PAYLOADFlags:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_VENDOR =>
                This.Vendor:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_optFlags =>
                This.optFlags:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_postinprog =>
                This.postinprog:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_Arch =>
                This.Arch:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_OS =>
                This.OS:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_URL =>
                This.URL:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_PLATFORM =>
                This.Platform :=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_RPMVersion =>
                This.RPM_Version:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_SOURCERPM =>
                This.SRPM:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_Group =>
                This.Group:=  Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_DESCRIPTION =>
                This.Description := Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when RPMTAG_SUMMARY =>
                This.Summary := Read_i18_String(
                    this,
                    data_items(Index),
                    length);
            when others =>
                pragma Debug(Put_Line("READER OF " & tags_type'Image(tag(Index)) & " IS NOT IMPLEMETED. OFFSET=" & This.Offset'Img));
                Skip_String(this, data_items(Index));
        end case;
    end Check_String_Types;

    procedure Check_int32_Type(This : in out Rpm_File; Index : in rpmhdrIndex) is
        use arpm_rpm_depends;
        OVERWRITE_EXCEPTION : exception;
    begin
        case tag(Index) is
            when RPMTAG_BUILDTIME =>
                This.Build_Time := Read_int32(This, Data_Items(Index));
            when RPMTAG_REQUIREFlags =>
                if This.Requires = Null then
                    This.Requires := new rpm_depend_array(1..data_Items(Index));
                end if;
                for I in 1..This.Requires'Length loop
                    Set_Flags(This.Requires(1),   Read_int32(This, 1));
                end loop;
            when RPMTAG_PROVIDEFlags =>
                if This.Provides = Null then
                    This.Provides := new rpm_depend_array(1..data_Items(Index));
                end if;
                for I in 1..This.Provides'Length loop
                    Set_Flags(This.Provides(1),   Read_int32(This, 1));
                end loop;
            when RPMTAG_SIZE =>
                This.Size := Read_int32(This, Data_Items(Index));
            when others =>
                pragma Debug(Put_Line("READER OF " & tags_type'Image(tag(Index)) & " IS NOT IMPLEMETED. OFFSET=" & This.Offset'Img));
                Skip_int32(This, data_items(Index));
        end case;
    end Check_int32_Type;

    procedure Read_Payload(This: in out Rpm_File; Indexes : in Index_array_access) is
        procedure Allign_Offset(This : in out Rpm_File; Offset : in Integer)
            with
                Pre => Offset - This.Offset > 0,
                Post => This.Offset = offset;

        procedure Allign_Offset(This : in out Rpm_File; Offset : in Integer)
        is
            bytes_to_skip : constant Integer := Offset - This.Offset;
            byte : dummy_byte;
        begin
            for I in 1..bytes_to_skip loop
                dummy_byte'Read(This.Stream, byte);
                This.Offset := This.Offset + byte'Size/8;
            end loop;
        end Allign_Offset;
        cf : Format_type;
        cof : Integer;
        ctag : tags_type;
    begin
        pragma Debug (Put_Line ("DEBUG: Reading payload") );
        for I in 1..Indexes'Length loop
            cf :=  Format(Indexes(I));
            cof := Data_Offset(Indexes(I));
            ctag := tag(Indexes(I));
            if cof = 0 then
                pragma Debug(Put_Line("DEBUG: Reseting offset"));
                This.Offset := 0;
            end if;
            -- Put_Line("Read: " & tags_type'Image(ctag));
            -- Sometimes We have to skip bytes to allign
            -- cursor (Rpm_File.Offset) and tag offset (Data_Offset(Indexes(I)))
            -- String types are not alligned
            if This.Offset /= cof then
                Allign_Offset(This, cof);
            end if;
            case cf is
                when RPM_BIN_TYPE =>
                    case ctag is
                        when others =>
                            Skip_Bin(This, data_Items(Indexes(I)));
                            null;
                    end case;
                when RPM_STRING_ARRAY_TYPE =>

                    case ctag is
                        when RPMTAG_PROVIDEName =>
                            if This.Provides  = Null then
                                This.Provides :=  new rpm_depend_array(1..data_Items(Indexes(I)));
                            end if;
                            for Prov in 1..This.Provides'Length loop
                                Set_Name(This.Provides(Prov), Read_I18_String(
                                    This,
                                    1,
                                    Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I))
                                    ));
                            end loop;
                        when RPMTAG_PROVIDEVersion =>
                            if This.Provides  = Null then
                                This.Provides :=  new rpm_depend_array(1..data_Items(Indexes(I)));
                            end if;
                            for Req in 1..This.Provides'Length loop
                                Set_Version(This.Provides(Req), Read_I18_String(
                                    This,
                                    1,
                                    Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I))
                                    ));
                            end loop;

                        when RPMTAG_REQUIREName =>
                            if This.Requires  = Null then
                                This.Requires :=  new rpm_depend_array(1..data_Items(Indexes(I)));
                            end if;
                            for Req in 1..This.Requires'Length loop
                                Set_Name(This.Requires(Req), Read_I18_String(
                                    This,
                                    1,
                                    Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I))
                                    ));
                            end loop;
                        when RPMTAG_REQUIREVersion =>
                            if This.Requires  = Null then
                                This.Requires :=  new rpm_depend_array(1..data_Items(Indexes(I)));
                            end if;
                            for Req in 1..This.Requires'Length loop
                                Set_Version(This.Requires(Req), Read_I18_String(
                                    This,
                                    1,
                                    Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I))
                                    ));
                            end loop;
                        when others =>
                            Skip_String_Array(This, data_items(Indexes(I)));
                    end case;
                when RPM_int32_TYPE =>
                    Check_int32_Type(This, Indexes(I));
                when RPM_int16_TYPE =>
                    case ctag is
                        when others =>
                            Skip_int16(This, data_items(Indexes(I)));
                    end case;
                when RPM_STRING_TYPE | RPM_I18NSTRING_TYPE =>
                    Check_String_Types(This => This,
                    Index => Indexes(I),
                    Length => Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I)));
                when others =>
                    pragma Debug(Put_Line("READER OF " & tags_type'Image(ctag) & " IS NOT IMPLEMETED. OFFSET=" & This.Offset'Img));
            end case;
        end loop;



            -- DEBUG ONLY
            pragma Debug(Put_Line("RESULT ## Name: " & To_String(This.Name)));
            pragma Debug(Put_Line("RESULT ## Version: " & To_String(This.Version)));
            pragma Debug(Put_Line("RESULT ## Release: " & To_String(This.Release)));
            pragma Debug(Put_Line("RESULT ## Summary: " & To_String(This.Summary)));
            pragma Debug(Put_Line("RESULT ## Description: " & To_String(This.Description)));
            pragma Debug(Put_Line("RESULT ## Build time: " & This.Build_Time'Img));
            pragma Debug(Put_Line("RESULT ## Size:" & This.Size'Img));
            pragma Debug(Put_Line("RESULT ## Build host: " & To_String(This.Build_host)));
            pragma Debug(Put_Line("RESULT ## Licanse:" & To_String(This.License)));
            pragma Debug(Put_Line("RESULT ## Vendor: " & To_String(This.Vendor)));
            pragma Debug(Put_Line("RESULT ## Group: " & To_String(This.Group)));
            pragma Debug(Put_Line("RESULT ## PAYLOADFlags: " & To_String(This.PAYLOADFlags)));
            pragma Debug(Put_Line("RESULT ## PAYLOADCOMPRESSOR: " & To_String(This.PAYLOADCOMPRESSOR)));
            pragma Debug(Put_Line("RESULT ## PAYLOADFormat: " & To_String(This.PAYLOADFormat)));
            pragma Debug(Put_Line("RESULT ## packager: " & To_String(This.packager)));
            pragma Debug(Put_Line("RESULT ## cookie: " & To_String(This.cookie)));
            pragma Debug(Put_Line("RESULT ## optFlags: " & To_String(This.optFlags)));
            pragma Debug(Put_Line("RESULT ## postunprog: " & To_String(This.postunprog)));
            pragma Debug(Put_Line("RESULT ## postinprog: " & To_String(This.postinprog)));
            pragma Debug(Put_Line("RESULT ## postin: " & To_String(This.postin)));
            pragma Debug(Put_Line("RESULT ## postun: " & To_String(This.postun)));
            pragma Debug(Put_Line("RESULT ## Arch: " & To_String(This.Arch)));
            pragma Debug(Put_Line("RESULT ## OS: " & To_String(This.OS)));
            pragma Debug(Put_Line("RESULT ## URL: " & To_String(This.URL)));
            pragma Debug(Put_Line("RESULT ## SRPM: " & To_String(This.SRPM)));
            pragma Debug(Put_Line("RESULT ## Platform: " & To_String(This.Platform)));
            pragma Debug(Put_Line("RESULT ## RPM_Version: " & To_String(This.RPM_Version)));
            for I in 1..This.Provides'Length loop
                pragma Debug(Put_Line("RESULT ## Provides Name: " & To_String(This.Provides(I).Name) &
                    "; Version: " & To_String(This.Provides(I).Version)));
            end loop;
            for I in 1..This.Requires'Length loop
                pragma Debug(Put_Line("RESULT ## Requires Name: " & To_String(This.Requires(I).Name) &
                    "; Version: " & To_String(This.Requires(I).Version)));
            end loop;

    end Read_Payload;

    procedure Free(This : in out RPM_File_Access) is
    begin
        Free_Depends(This.Requires);
        Free_Depends(This.Provides);
        Free_RPM(This);
    end Free;
end arpm_Rpm_Files;

