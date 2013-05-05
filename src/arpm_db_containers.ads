with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
package ARPM_DB_Containers is 
    procedure Save(RPM : in ARPM_RPM_Access; DB : in out Database_Connection);
    procedure Save_Requires(RPM : in ARPM_RPM_Access; DB : in out Database_Connection);
    procedure Save_Provides(RPM : in ARPM_RPM_Access; DB : in out Database_Connection);
end ARPM_DB_Containers;
