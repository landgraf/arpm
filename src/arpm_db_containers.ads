with ARPM_RPM_internals; use ARPM_RPM_internals;
with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
package Arpm_Db_Containers is
    procedure Save(RPM : in ARPM_RPM_Access; DB : in Database_Connection);
    procedure Save_Requires(RPM : in ARPM_RPM_Access; DB : in Database_Connection);
    procedure Save_Provides(RPM : in ARPM_RPM_Access; DB : in Database_Connection);
end Arpm_Db_Containers;
