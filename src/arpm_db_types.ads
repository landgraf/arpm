with SQL.Databases;
package ARPM_DB_Types is 
    type ARPM_DB_Container is record 
        Handler : access SQL.Databases.SQL_Database;
        Error  : Integer := Integer'Last;
    end record;
    type ARPM_DB_Container_Access is access all ARPM_DB_Container;
end ARPM_DB_Types;
