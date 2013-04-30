with SQL.Databases; use SQL.Databases;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with League.Strings; use League.Strings;
with Matreshka.Internals.SQL_Drivers.SQLite3.Factory;
package ARPM_DB_Containers is 
    type ARPM_DB_Container is record 
        Handler : access SQL_Database;
        Error  : Integer := Integer'Last;
    end record;
    type ARPM_DB_Container_Access is access all ARPM_DB_Container;
    package Constructors is 
        function Create(Name : in Universal_String) return ARPM_DB_Container_Access;
    end Constructors;
    procedure Save_Main(RPM : in ARPM_RPM_Access; DB : in ARPM_DB_Container_Access);
end ARPM_DB_Containers;
