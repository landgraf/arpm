with Internal_Codecs; use Internal_Codecs;
with POSIX.Files;
with arpm_Rpm_Leaders; use arpm_Rpm_Leaders;
with arpm_Rpm_Headers; use arpm_Rpm_Headers;
with arpm_rpm_rpmhdrIndexs; use arpm_rpm_rpmhdrIndexs;
with Ada.Text_IO ; use Ada.Text_IO;
with Ada.Assertions;  use Ada.Assertions;

package body Arpm_Rpm_Files is

   package body Constructors is
     function  Create(FileName : String) return Rpm_File_Access is
       use POSIX.Files;
       RPM : Rpm_File_Access := new Rpm_File;
       IO_Error : exception;
     begin
       if not Is_File(POSIX.To_POSIX_String(FileName)) then
         raise IO_Error;
       end if;
       Ada.Streams.Stream_IO.Open
         (File => RPM.File,
         Name => FileName,
         Mode => Ada.Streams.Stream_IO.In_File);
       RPM.Stream := Ada.Streams.Stream_IO.Stream (RPM.File);
       return RPM;
     end Create;
   end Constructors;

   function Name (This : in out RPM_File) return Unbounded_String is (This.Name); 
   function Provides (This : in out RPM_File) return arpm_rpm_depends.rpm_depends_access is (This.Provides); 
   function Requires (This : in out RPM_File) return arpm_rpm_depends.rpm_depends_access is (This.Requires); 
   function Vendor (This : in out RPM_File) return Unbounded_String is (This.Vendor); 
   function License (This : in out RPM_File) return Unbounded_String is (This.License); 
   function Url (This : in out RPM_File) return Unbounded_String is (This.Url); 
   function Description (This : in out RPM_File) return Unbounded_String is (This.Description); 
   function Arch (This : in out RPM_File) return Unbounded_String is (This.Arch); 
   function Summary (This : in out RPM_File) return Unbounded_String is (This.Summary); 
   function Version (This : in out RPM_File) return Unbounded_String is (This.Version); 
   function Release (This : in out RPM_File) return Unbounded_String is (This.Release); 
   function Epoch (This : in out RPM_File) return Unbounded_String is (This.Epoch); 

   function htonl32(number : in Four_Byte_Number) return Four_Byte_Number;
     pragma Import(C, htonl32, "htonl");
   function htonl16(number : in Two_Byte_Number) return Two_Byte_Number;
     pragma Import(C, htonl16, "htonl");

   function Read_I18_String (This : in out Rpm_File; Items : in Integer := 1; Max_Length : in Integer := 1; Debug : Boolean := False ) return Unbounded_String is
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
   end Read_I18_String;

   procedure Skip_String(This : in out Rpm_File; Items : in Integer := 1 ) is
     buffer : dummy_byte;
   begin
     for I in 1..Items loop
       loop
         dummy_byte'Read(This.Stream, buffer) ;
         This.Offset := This.Offset + buffer'Size/8;
         if  buffer = 16#0# then
            exit;
         end if;
       end loop;
     end loop;
   end Skip_String;

   procedure Skip_String_Array(This : in out Rpm_File; Items : in Integer) is
   begin
     for I in 1..Items loop
       Skip_String(This);
     end loop;
   end Skip_String_Array;

   procedure Skip_Bin(This : in out Rpm_File; Items : in Integer := 1 ) is
     buffer : dummy_byte;
   begin
     for I in 1..Items loop
       dummy_byte'Read(This.Stream, buffer) ;
       This.Offset := This.Offset + buffer'Size/8;
     end loop;
   end Skip_Bin;

   function read_int16(This : in out Rpm_File;  Items : in Integer := 1) return Integer is
     buffer : Two_Byte_Number := 0;
   begin
     for I in 1..Items loop
       Two_Byte_Number'Read(This.Stream, buffer) ;
       This.Offset := This.Offset + buffer'Size/8;
     end loop;
     return Integer(htonl16(buffer));
   end read_int16;

   procedure Skip_int16(This : in out Rpm_File;  Items : in Integer := 1) is
     buffer : Integer;
   begin
     buffer := read_int16(This, Items);
   end Skip_int16;

   function Read_Int32(This : in out Rpm_File; Items : in Integer := 1) return Long_Long_Integer is
     buffer : Four_Byte_Number;
   begin
     for I in 1..Items loop
       Four_Byte_Number'Read(This.Stream, buffer) ;
       This.Offset := This.Offset + buffer'Size/8;
     end loop;
     return Long_Long_Integer(htonl32(buffer));
   end Read_Int32;

   procedure Skip_int32(This : in out Rpm_File; Items : in Integer := 1) is
     buffer : Long_Long_Integer;
   begin
     buffer := Read_Int32(This, Items);
   end Skip_int32;

   procedure Parse(This : in out Rpm_File) is
     Number_Of_Indexes : Natural := 0;
   begin
     This.Read_Leader;
     This.Read_Header(Signature => True);
     This.Read_Header(Signature => False);
   end Parse;

   procedure Read_Leader(This : in out Rpm_File) is
     Leader : Rpm_Leader;
   begin
     pragma Debug(Put_Line("DEBUG: parsing  Rpm_Leader"));
     Rpm_Leader'Read(This.Stream, Leader);
     This.Offset := This.Offset + Leader'Size/8;
     pragma Debug (Put_Line("DEBUG: Package Name: " & Name(Leader)));
     pragma Debug (Put_Line("DEBUG: Package Format: " & Rpm_Types'Image(Rpm_Type(Leader))));
   end Read_Leader;

   procedure Read_Header (This : in out Rpm_File; Signature : Boolean := False) is
     Header : Rpm_Header;
     buffer : dummy_byte := 0;
     Indexes : Index_array_access;
   begin
     pragma Debug(Put_Line("DEBUG: looking for RPM Header"));
     -- The Header structure Header always starts with a three-byte Magic number: 8e ad e8
     Header_loop:
     loop
       for I in LABELONE'First..LABELONE'Last loop
         if buffer = LABELONE(I) then
            exit Header_loop when I = 3;
            dummy_byte'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size/8;
         else
            dummy_byte'Read(This.Stream, buffer) ;
            This.Offset := This.Offset + buffer'Size/8;
            exit;
         end if;
       end loop;
     end loop Header_loop;
     Rpm_Header'Read(This.Stream, Header);
     This.Offset := This.Offset + Header'Size/8;
     Indexes := This.Read_Indexes(arpm_Rpm_Headers.Indexes(Header));
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

   procedure Check_String_Types(This: in out Rpm_File; Index : in rpmhdrIndex; Length : in Integer) is
   begin
     case  Tag(Index) is
       when RPMTag_Name =>
         This.Name := Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_Version =>
         This.Version := Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_RELEASE =>
         This.Release := Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_DISTRIBUTION =>
         This.Distribution := Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_BUILDHOST =>
         This.Build_Host := Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_License =>
         This.License :=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_POSTUNPROG =>
         This.postunprog:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_COOKIE =>
         This.cookie:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_PACKAGER =>
         This.packager:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_PAYLOADFormat =>
         This.PAYLOADFormat:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_POSTIN =>
         This.postin :=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_POSTUN =>
         This.postun :=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_PAYLOADCOMPRESSOR =>
         This.PAYLOADCOMPRESSOR:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_PAYLOADFlags =>
         This.PAYLOADFlags:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_VENDOR =>
         This.Vendor:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_OPTFLAGS =>
         This.optFlags:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_POSTINPROG =>
         This.postinprog:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_ARCH =>
         This.Arch:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_OS =>
         This.OS:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_URL =>
         This.URL:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_PLATFORM =>
         This.Platform :=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_RPMVersion =>
         This.RPM_Version:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_SOURCERPM =>
         This.SRPM:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_GROUP =>
         This.Group:=  Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_DESCRIPTION =>
         This.Description := Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when RPMTag_SUMMARY =>
         This.Summary := Read_I18_String(
            This,
            Data_Items(Index),
            Length);
       when others =>
         pragma Debug(Put_Line("READER OF " & Tags_Type'Image(Tag(Index)) & " IS NOT IMPLEMETED. Offset=" & This.Offset'Img));
         Skip_String(This, Data_Items(Index));
     end case;
   end Check_String_Types;

   procedure Check_int32_Type(This : in out Rpm_File; Index : in rpmhdrIndex) is
     use arpm_rpm_depends;
     OVERWRITE_EXCEPTION : exception;
   begin
     case Tag(Index) is
       when RPMTag_BUILDTIME =>
         This.Build_Time := Read_Int32(This, Data_Items(Index));
       when RPMTag_REQUIREFlags =>
         if This.Requires = Null then
            This.Requires := new rpm_depend_array(1..Data_Items(Index));
         end if;
         for I in 1..This.Requires'Length loop
            Set_Flags(This.Requires(1),   Read_Int32(This, 1));
         end loop;
       when RPMTag_PROVIDEFlags =>
         if This.Provides = Null then
            This.Provides := new rpm_depend_array(1..Data_Items(Index));
         end if;
         for I in 1..This.Provides'Length loop
            Set_Flags(This.Provides(1),   Read_Int32(This, 1));
         end loop;
       when RPMTag_SIZE =>
         This.Size := Read_Int32(This, Data_Items(Index));
       when others =>
         pragma Debug(Put_Line("READER OF " & Tags_Type'Image(Tag(Index)) & " IS NOT IMPLEMETED. Offset=" & This.Offset'Img));
         Skip_int32(This, Data_Items(Index));
     end case;
   end Check_int32_Type;

   procedure Read_Payload(This: in out Rpm_File; Indexes : in Index_array_access) is
     procedure Allign_Offset(This : in out Rpm_File; Offset : in Integer)
       with
         Pre => Offset - This.Offset > 0,
         Post => This.Offset = Offset;

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
     cf : Format_Type;
     cof : Integer;
     cTag : Tags_Type;
   begin
     pragma Debug (Put_Line ("DEBUG: Reading payload") );
     for I in 1..Indexes'Length loop
       cf :=  Format(Indexes(I));
       cof := Data_Offset(Indexes(I));
       cTag := Tag(Indexes(I));
       if cof = 0 then
         pragma Debug(Put_Line("DEBUG: Reseting Offset"));
         This.Offset := 0;
       end if;
       -- Put_Line("Read: " & Tags_Type'Image(cTag));
       -- Sometimes We have to skip bytes to allign
       -- cursor (Rpm_File.Offset) and Tag Offset (Data_Offset(Indexes(I)))
       -- String types are not alligned
       if This.Offset /= cof then
         Allign_Offset(This, cof);
       end if;
       case cf is
         when RPM_BIN_TYPE =>
            case cTag is
              when others =>
                Skip_Bin(This, Data_Items(Indexes(I)));
                null;
            end case;
         when RPM_STRING_ARRAY_TYPE =>

            case cTag is
              when RPMTag_PROVIDEName =>
                if This.Provides  = Null then
                  This.Provides :=  new rpm_depend_array(1..Data_Items(Indexes(I)));
                end if;
                for Prov in 1..This.Provides'Length loop
                  Set_Name(This.Provides(Prov), Read_I18_String(
                     This,
                     1,
                     Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I))
                     ));
                end loop;
              when RPMTag_PROVIDEVersion =>
                if This.Provides  = Null then
                  This.Provides :=  new rpm_depend_array(1..Data_Items(Indexes(I)));
                end if;
                for Req in 1..This.Provides'Length loop
                  Set_Version(This.Provides(Req), Read_I18_String(
                     This,
                     1,
                     Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I))
                     ));
                end loop;

              when RPMTag_REQUIREName =>
                if This.Requires  = Null then
                  This.Requires :=  new rpm_depend_array(1..Data_Items(Indexes(I)));
                end if;
                for Req in 1..This.Requires'Length loop
                  Set_Name(This.Requires(Req), Read_I18_String(
                     This,
                     1,
                     Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I))
                     ));
                end loop;
              when RPMTag_REQUIREVersion =>
                if This.Requires  = Null then
                  This.Requires :=  new rpm_depend_array(1..Data_Items(Indexes(I)));
                end if;
                for Req in 1..This.Requires'Length loop
                  Set_Version(This.Requires(Req), Read_I18_String(
                     This,
                     1,
                     Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I))
                     ));
                end loop;
              when others =>
                Skip_String_Array(This, Data_Items(Indexes(I)));
            end case;
         when RPM_int32_TYPE =>
            Check_int32_Type(This, Indexes(I));
         when RPM_int16_TYPE =>
            case cTag is
              when others =>
                Skip_int16(This, Data_Items(Indexes(I)));
            end case;
         when RPM_STRING_TYPE | RPM_I18NSTRING_TYPE =>
            Check_String_Types(This => This,
            Index => Indexes(I),
            Length => Data_Offset(Indexes(I+1)) - Data_Offset(Indexes(I)));
         when others =>
            pragma Debug(Put_Line("READER OF " & Tags_Type'Image(cTag) & " IS NOT IMPLEMETED. Offset=" & This.Offset'Img));
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
       pragma Debug(Put_Line("RESULT ## Build host: " & To_String(This.Build_Host)));
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

   procedure Close(This : in out Rpm_File_Access) is
   begin
     Free_Depends(This.Requires);
     Ada.Streams.Stream_IO.Close (This.File);
     Free_Depends(This.Provides);
     Free_RPM(This);
   end Close;
end Arpm_Rpm_Files;

