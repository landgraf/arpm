with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with SQL.Databases; use SQL.Databases;
with League.Strings; use League.Strings;
with League.String_Vectors; use League.String_Vectors;
with ARPM_DB_Types; use ARPM_DB_Types;
package ARPM_DB_Containers is 
    procedure Save_Main(RPM : in ARPM_RPM_Access; DB : in ARPM_DB_Container_Access);
end ARPM_DB_Containers;
