with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
with ARPM_DB_Types; use ARPM_DB_Types;
package ARPM_DB_Containers is 
    procedure Save_Main(RPM : in ARPM_RPM_Access; DB : in Database_Connection);
--    procedure Save_Provides(RPM : in ARPM_RPM_Access; DB : in Database_Connection);
    procedure Save_requires(RPM : in ARPM_RPM_Access; DB : in Database_Connection);
end ARPM_DB_Containers;
