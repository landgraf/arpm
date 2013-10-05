with Internal_Codecs; use Internal_Codecs; 
with POSIX.Files; 
with arpm_rpm_leaders; use arpm_rpm_leaders; 
with arpm_rpm_headers; use arpm_rpm_headers; 
with arpm_rpm_rpmhdrindexs; use arpm_rpm_rpmhdrindexs; 
with ada.text_io ; use ada.Text_IO; 
package body arpm_rpm_files is 
    procedure Parse(This : in out RPM_File) is 
    begin
        this.read_leader; 
        -- skip signature for now
        for I in 1..2 loop
            this.read_header;
        end loop;
        this.Read_Hdrindex;
    end Parse;
    procedure Read_Leader(This : in out RPM_File) is 
       Leader : RPM_Leader; 
    begin
        pragma Debug(Put_Line("DEBUG: parsing  RPM_Leader")); 
        RPM_Leader'Read(This.Stream, Leader);
        pragma Debug (Put_Line("DEBUG: Package name: " & Name(Leader)));
        pragma Debug (Put_Line("DEBUG: Package format: " & rpmtypes'Image(rpmtype(Leader))));
    end Read_Leader; 

    procedure Read_Header ( This : in out RPM_File) is 
       Header : RPM_Header; 
       buffer : dummy_byte := 0; 
    begin
        pragma Debug(Put_Line("DEBUG: looking for RPM header")); 
        -- The header structure header always starts with a three-byte magic number: 8e ad e8
        header_loop:
        loop 
            for I in LABELONE'First..LABELONE'Last loop
                if buffer = LABELONE(I) then 
                    exit header_loop when I = 3; 
                    dummy_byte'Read(This.Stream, buffer) ;
                else
                    dummy_byte'Read(This.Stream, buffer) ;
                    exit; 
                end if; 
            end loop; 
        end loop header_loop; 
        rpm_header'Read(This.Stream, header);
        this.indexes := new index_array(1..indexes(header)); 
        pragma Debug ( Put_Line("DEBUG: Header version: " & version(header)'Img)) ; 
        pragma Debug ( Put_Line("DEBUG: Header data bytes: " & Data_Bytes(header)'Img));
        pragma Debug ( Put_Line("DEBUG: Header indexes: " & indexes(header)'Img)); 

    end Read_Header; 

    procedure Read_Hdrindex(This : in out RPM_File) is 
        index : rpmhdrindex; 
    begin
        pragma Debug(Put_Line("DEBUG: parsing rpmhdrindex")); 
        for I in 1..This.indexes'Length loop
            rpmhdrindex'Read(This.Stream, index);
            -- FIXME read store here 
            This.indexes(I) := index; 
            pragma DEBUG(Put_Line("DEBUG: index: " & tag(index)));
            pragma DEBUG(Put_Line("DEBUG: format: " & format(index)));
            pragma Debug (Put_Line("DEBUG: Number of data items: " & data_items(index)'Img ));
            pragma Debug (Put_Line("DEBUG: Data offset: " & data_offset(index)'Img ));
        end loop;
    end Read_hdrindex; 

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

