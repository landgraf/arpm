with ARPM_RPM_Files; use ARPM_RPM_Files; 
with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
package Arpm_Db_Containers is
   procedure Save(RPM : in RPM_File_Access; DB : in Database_Connection);
   procedure Save_Requires(RPM : in RPM_File_Access; DB : in Database_Connection);
   procedure Save_Provides(RPM : in RPM_File_Access; DB : in Database_Connection);
end Arpm_Db_Containers;
