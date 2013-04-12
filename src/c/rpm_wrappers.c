#include <stdio.h>
#include <stdlib.h>
 
#include <rpm/rpmlib.h>
#include <rpm/header.h>
#include <rpm/rpmts.h>
#include <rpm/rpmdb.h>
#include <rpm/rpmlog.h>


typedef struct {
    int   errcode;
    char* pkg_name;
    char* pkg_version; 
    char* pkg_release;
} my_rpm_struct;


void  parse_rpm(char* filename, my_rpm_struct *mrs){
    // my struct to be returned
    // my_rpm_struct mrs; 
    mrs->errcode = 0;

    int i;
    rpmts ts;

    FD_t fd;
    rpmRC rc;
    Header hdr;
    char *pkg_name, *pkg_version, *pkg_release;
    rpmVSFlags vsflags = 0;

    rc = rpmReadConfigFiles(NULL, NULL);
    if (rc != RPMRC_OK) {
        mrs->errcode = 100;
        return;
    }

    fd = Fopen(filename, "r.ufdio");
    if ((!fd) || Ferror(fd)) {
        mrs->errcode = 1;
        if (fd) {
            Fclose(fd);
        }
        return;
    }

    ts = rpmtsCreate();

    vsflags |= _RPMVSF_NODIGESTS;
    vsflags |= _RPMVSF_NOSIGNATURES;
    vsflags |= RPMVSF_NOHDRCHK;
    (void) rpmtsSetVSFlags(ts, vsflags);

    rc = rpmReadPackageFile(ts, fd, filename, &hdr);
    if (rc != RPMRC_OK) {
        mrs->errcode = 2;
        Fclose(fd);
        return;
    }
    Fclose(fd);

    if (headerNVR(hdr, (const char **) &pkg_name,
                (const char **) &pkg_version,
                (const char **) &pkg_release))
    {
        mrs->errcode = 3;
    } else {
        mrs->pkg_name = pkg_name;
        mrs->pkg_version = pkg_version;
        mrs->pkg_release = pkg_release;
        headerFreeData(pkg_name, RPM_STRING_TYPE);
        headerFreeData(pkg_version, RPM_STRING_TYPE);
        headerFreeData(pkg_release, RPM_STRING_TYPE);
    }

    headerFree(hdr);
    rpmtsFree(ts);
};
