with Ada.Text_Io; use Ada.Text_IO;
package body ARPM_Files_Handlers is 
    protected body Files is 
        procedure Put(FileName : Unbounded_String) is 
        begin
            F.Append(FileName);
        end Put;
        entry Get(FileName : out Unbounded_String) when not F.Is_Empty or E is 
        begin
            if E then
                FileName := Null_Unbounded_String;
            else
                FileName := F.First_Element;
                F.Delete_First;
            end if;
        end Get;
        entry Finish when F.Is_Empty is 
        begin
            E := True;
        end Finish;
    end Files;
    protected body Workers is 
        procedure Increase is 
        begin
            Count := Count + 1;
            Started := True;
        end Increase;

        procedure Decrease is
        begin
            Count := Count - 1;
        end Decrease;
        
        entry Is_Empty when Count = 0 and Started is
        begin
            null;
        end Is_Empty;
    end Workers;
    
    protected body  KeyGenerator is 
        procedure Next(Key : out Integer) is 
        begin
            Key := K; 
            K := K + 1;
        end Next;
    end KeyGenerator;

end ARPM_FIles_Handlers;
