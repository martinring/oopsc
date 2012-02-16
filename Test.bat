@ECHO OFF
FOR %%i IN (examples\*.oops) DO (  
  ECHO Compiling %%i:  
  java -jar target\oopsc-assembly-1.0.jar %%i %%i.asm
  IF EXIST %%i.asm (        
    java -jar oopsvm.jar %%i.asm    
    ECHO:
  )
  PAUSE 
)