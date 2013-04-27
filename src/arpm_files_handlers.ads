with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.Sqlite;

package ARPM_Files_Handlers is 
    package Files_Container_Package is new Ada.Containers.Vectors(
        Element_Type => Unbounded_String, Index_Type => Positive
        );
    subtype Files_Container is Files_Container_Package.Vector;

    protected Sessions is 
        entry Wait_Session(DB : out Session_Type);
        procedure Create_Session;
        function Get_Session return Session_Type;
        private
        -- DB : Session_Type := No_Session; 
        Setup : Boolean := False;
    end Sessions;
    protected Files is 
        procedure Put(FileName : Unbounded_String);
        entry Get(FileName : out Unbounded_String);
        entry Finish; 
        private
        F : Files_Container;
        E : Boolean := False;
    end Files;
    protected Workers is 
        procedure Increase; 
        procedure Decrease;
        entry Is_Empty;
        private
        Count : Integer := 0; 
        Started : Boolean := False;
    end Workers;
end ARPM_FIles_Handlers;
