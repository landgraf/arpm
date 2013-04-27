with ORM; use ORM; 
with GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
package ARPM_DB_Containers is 
    procedure Save(MyRPM : in My_RPM_Struct_Access; DB : in Session_Type)
        with Pre => DB /= No_Session;
end ARPM_DB_Containers; 
