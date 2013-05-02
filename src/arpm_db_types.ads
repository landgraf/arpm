with SQL.Databases;
with Ada.Unchecked_Deallocation;
package ARPM_DB_Types is 
    type ARPM_DB_Container is record 
        Handler : access SQL.Databases.SQL_Database;
        Error  : Integer := Integer'Last;
    end record;
    type ARPM_DB_Container_Access is access all ARPM_DB_Container;
    type DB_Access is access all SQL.Databases.SQL_Database;
    procedure Free is new Ada.Unchecked_Deallocation(
        Object => ARPM_DB_Container, 
        Name => ARPM_DB_Container_Access);
    procedure Free_Handler is new Ada.Unchecked_Deallocation(
        Object => SQL.Databases.SQL_Database, 
        Name => DB_Access);
end ARPM_DB_Types;
