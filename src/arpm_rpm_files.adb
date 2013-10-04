with Internal_Codecs; use Internal_Codecs; 
with POSIX.Files; 
with arpm_rpm_leaders; use arpm_rpm_leaders; 
with arpm_rpm_headers; use arpm_rpm_headers; 
with ada.text_io ; use ada.Text_IO; 
package body arpm_rpm_files is 
    procedure Read_Leader(This : in out RPM_File) is 
       Leader : RPM_Leader; 
    begin
        pragma Debug(Put_Line("DEBUG: parsing  RPM_Leader")); 
        RPM_Leader'Read(This.Stream, Leader);
        Put_Line("magic: " & Magic(Leader));
        Put_Line("Package name: " & Name(Leader));
        Put_Line("Package format: " & rpmtypes'Image(rpmtype(Leader)));
    end Read_Leader; 

    procedure Read_Header ( This : in out RPM_File) is 
       Header : RPM_Header; 
       buffer : dummy_byte := 0; 
       version : dummy_byte := 0; 
       header_indexes : header_indexes_type := 0; 

    begin
        pragma Debug(Put_Line("DEBUG: looking for RPM header")); 
        -- The header structure header always starts with a three-byte magic number: 8e ad e8
        header_loop:
        loop 
            for I in LABELONE'First..LABELONE'Last loop
                if buffer = LABELONE(I) then 
                    pragma Debug ( Put_Line("DEBUG: Magic element " & I'Img & " found " & buffer'Img)); 
                    exit header_loop when I = 3; 
                    dummy_byte'Read(This.Stream, buffer) ;
                else
                    -- reset the mask and continue
                    dummy_byte'Read(This.Stream, buffer) ;
                    exit; 
                end if; 
            end loop; 
        end loop header_loop; 
        --  Following this is a one-byte version number
        dummy_byte'Read(This.Stream, version) ;
        pragma Debug(Put_Line("DEBUG: Version is " & version'Img));
        for i in 1..4 loop
            -- Next are four bytes that are reserved for future expansion
            dummy_byte'Read(This.Stream, buffer) ;
        end loop; 
        --  there is a four-byte number that indicates how many index entries exist in this header structure
        header_indexes_type'Read(This.Stream, header_indexes) ;
        pragma Debug(Put_Line("DEBUG: Number of indexes " & version'Img));


    end Read_Header; 

    package body Constructors is 
        function  Create(Filename : Universal_String) return rpm_file_access is 
            use POSIX.Files; 
            RPM : rpm_file_access := new rpm_file; 
            IO_ERROR : exception; 
        begin
            RPM.File_Name := Filename;
            if not Is_File(posix.TO_POSIX_String(US_To_String(Filename))) then
                raise IO_ERROR; 
            end if;
            Ada.Streams.Stream_IO.Open
                (File => RPM.File,
                Name => US_To_String(RPM.File_name),
                Mode => Ada.Streams.Stream_IO.In_File);
            RPM.Stream := Ada.Streams.Stream_IO.Stream (RPM.File);
            return RPM;
        end Create; 
    end Constructors; 

end arpm_rpm_files; 

