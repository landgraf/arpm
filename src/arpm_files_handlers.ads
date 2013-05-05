with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ARPM_RPM_Internals; use ARPM_RPM_Internals;


package ARPM_Files_Handlers is 

    protected Files is 
    --  Onject to syncronize files 
        
        
        procedure Put(FileName : Unbounded_String);
        -- Put file to process
        
        
        entry Get(FileName : out Unbounded_String);
        -- Get file to process
        
        
        entry Finish; 
        -- Call after last file

        private
        F : ARPM_Vector_Container;
        E : Boolean := False;
    end Files;

    protected Workers is 
    -- Syncronoze workers 
    -- to free memoty after last worker is finished
        
        procedure Increase; 
        -- Increase count of workers
        
        procedure Decrease;
        -- Decrease count of workes
        
        entry Is_Empty;
        -- Barier to determinate if all workers are finished

        function Is_empty return Boolean;
        private
        Count : Integer := 0; 
        Started : Boolean := False;
    end Workers;

end ARPM_FIles_Handlers;
