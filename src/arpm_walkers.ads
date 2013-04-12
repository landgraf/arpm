with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ARPM_Files_Handlers; use ARPM_Files_Handlers;
package ARPM_Walkers is 
    type ARPM_Walker is tagged limited private;
    type ARPM_Walker_Access is access all ARPM_Walker;
    procedure Start(Dir : String);
    private
    type ARPM_Walker is tagged limited record
        Dir_Name : Unbounded_String := Null_Unbounded_String;
    end record;
end ARPM_Walkers;
