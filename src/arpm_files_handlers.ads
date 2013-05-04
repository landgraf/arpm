with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.SQL.Exec; use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sessions; use GNATCOLL.SQL.Sessions;
with GNATCOLL.SQL.SQLite;
with Ada.Containers.Vectors; use Ada.Containers;
with ARPM_DB_Types; use ARPM_DB_Types;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;


package ARPM_Files_Handlers is 

    protected Files is 
        procedure Put(FileName : Unbounded_String);
        entry Get(FileName : out Unbounded_String);
        entry Finish; 
        private
        F : ARPM_Vector_Container;
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

    protected KeyGenerator is 
        procedure Next(Key : out Integer);
        private 
        K : Integer := 0;
    end KeyGenerator;

    protected DB_Keys is 
            procedure Add_Provide_Key(Key : in Unbounded_String);
            function Has_Provide_Key(Key : in Unbounded_String) return Boolean;
            procedure Add_Require_Key(Key : in Unbounded_String);
            function Has_Require_Key(Key : in Unbounded_String) return Boolean;
        private
            Provides : ARPM_Vector_Container ; --ARPM_VecotUnbounded_String_Vector := Empty_Unbounded_String_Vector;
            Requires : ARPM_Vector_Container; --  Unbounded_String_Vector := Empty_Unbounded_String_Vector;
    end DB_Keys;

    protected DB is 
            -- function Get_DB return ARPM_DB_Container_Access;
            procedure Init_DB(FileName : in String);
            entry Get_DB(rDB : out Database_Description);
            procedure Free;
        private 
            DB : GNATCOLL.SQL.Exec.Database_Description;
            Initialized : Boolean := False;
    end DB;
end ARPM_FIles_Handlers;
