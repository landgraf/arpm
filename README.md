The project is *just for fun* RPM parser. It has been written for educational purpose only. 
Related topics:
- ~Ada-C interaction (rpm_wrappers.c and arpm_c_bridge.ad?)~
- RPM headers reader. 
- RPM payload reader (RPM_BIN_TYPE is not supported yet)
- multitasking and protected types for syncronization 

##### Benchmark
Fedora 19 DVD disk repo (3951 files):
 ```bash
$ time ./bin/createrepo ~/Downloads/packages/ 
 real    2m5.759s
 user    0m34.438s
 sys     0m19.290s
 ```

