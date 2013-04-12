with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors; use Ada.Containers;
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
end ARPM_FIles_Handlers;
