with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

package body ARPM_C_Bridge is 
    function Print(FileName : String) return String is 
        -- pragma Linker_Options("-lrpm");
        -- pragma Linker_Options("-lrpmio");SS
        myRPM : ARPM_C;
    begin
        Parse_RPM(New_String(filename), myrpm);
        if myRPM.errno = 0 then
            pragma Debug(Put_Line("Package name: " & Value(MyRPM.pkg_Name) & "  , version: " & Value(MyRPM.pkg_version) & "   , relase: " & Value(myRPM.pkg_release))) ;
            return "SUCCESS";  
        else 
            return "FAILED:  ERROR CODE:  " & myRPM.errno'Img ;
        end if;
    exception
        when Error: others =>
            Ada.Text_IO.Put("Exception: ");
            Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Name(Error));
            Ada.Text_IO.Put_Line(Ada.Exceptions.Exception_Message(Error));
    end Print;
end ARPM_C_Bridge;
