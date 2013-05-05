with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
with League.Strings; use League.Strings;
with League.String_Vectors; use League.String_Vectors;
with ARPM_DB_Types; use ARPM_DB_Types;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;


package ARPM_Files_Handlers is 
    package Files_Container_Package is new Ada.Containers.Vectors(
        Element_Type => Unbounded_String, Index_Type => Positive
        );
    subtype Files_Container is Files_Container_Package.Vector;

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

    protected KeyGenerator is 
        procedure Next(Key : out Integer);
        private 
        K : Integer := 0;
    end KeyGenerator;

    protected DB_Keys is 
            procedure Add_Provide_Key(Key : in Universal_String);
            function Has_Provide_Key(Key : in Universal_String) return Boolean;
            procedure Add_Require_Key(Key : in Universal_String);
            function Has_Require_Key(Key : in Universal_String) return Boolean;
        private
            Provides : ARPM_OSets_Container; --Universal_String_Vector := Empty_Universal_String_Vector;
            Requires :  ARPM_OSets_Container; -- Universal_String_Vector := Empty_Universal_String_Vector;
    end DB_Keys;

    protected DB is 
            -- function Get_DB return ARPM_DB_Container_Access;
            entry  Get_DB(rDB : out ARPM_DB_Container_Access);
            procedure Init_DB(FileName : in Universal_String);
            procedure Free;
        private 
            DB : ARPM_DB_Types.ARPM_DB_Container_Access;
            Initialized : Boolean := False;
    end DB;
end ARPM_FIles_Handlers;
