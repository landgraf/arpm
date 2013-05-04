with Ada.Unchecked_Deallocation;
with GNATCOLL.SQL.Sessions;
package ARPM_DB_Types is 
    type ARPM_DB_Container is record 
        Handler : access GNATCOLL.SQL.Sessions.Session_Type; 
        Error  : Integer := Integer'Last;
    end record;
    type ARPM_DB_Container_Access is access all ARPM_DB_Container;
    -- Empty_ARPM_Vector_Container : constant ARPM_DB_Container := new ARPM_DB_Container;
    type DB_Access is access all GNATCOLL.SQL.Sessions.Session_Type;
    procedure Free is new Ada.Unchecked_Deallocation(
        Object => ARPM_DB_Container, 
        Name => ARPM_DB_Container_Access);
    procedure Free_Handler is new Ada.Unchecked_Deallocation(
        Object => GNATCOLL.SQL.Sessions.Session_Type, 
        Name => DB_Access);
end ARPM_DB_Types;
