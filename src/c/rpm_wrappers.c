#include <stdio.h>
#include <stdlib.h>

#include <rpm/rpmlib.h>
#include <rpm/header.h>
#include <rpm/rpmts.h>
#include <rpm/rpmdb.h>

typedef struct{
    int error;
    char* name;
    char* version;
    char* epoch;
    char* release;
    char* arch;
    char* summary;
    char* description;
    char* url;
    char* license;
    char* vendor;
    int depends_count;
    char** depends_on;
    char** depends_version;
    int provides_count;
    char** provides;
    char** provides_version;
} my_rpm_struct;

int read_config(){
    rpmRC rc;
    rc = rpmReadConfigFiles(NULL, NULL);
    if (rc != RPMRC_OK) {
        return rc;
    }
    return 0;
}

void free_config(){
    rpmFreeRpmrc();
};

void get_requires_version(my_rpm_struct* myrpm, Header hdr, rpmtd td)
{
    int rc;
    rpmtdFreeData(td);
    rpmtdReset(td);
    rc = headerGet(hdr, RPMTAG_REQUIREVERSION, td, HEADERGET_EXT);
    myrpm->depends_version = (char**)malloc(myrpm->depends_count*sizeof(char*));
    int j;
    for (j = 0; j < myrpm->depends_count; j++){
        myrpm->depends_version[j] = strdup((char*) rpmtdGetString(td));
        rpmtdNext(td);
    }

}
void  get_req(my_rpm_struct* myrpm, Header hdr, rpmtd td)
{
    int rc;
    rpmtdFreeData(td);
    rpmtdReset(td);
    rc = headerGet(hdr, RPMTAG_REQUIRENAME, td, HEADERGET_EXT);
    myrpm->depends_count = rpmtdCount(td);
    myrpm->depends_on = (char**)malloc(myrpm->depends_count*sizeof(char*));
    int j;
    for (j = 0; j < myrpm->depends_count; j++){
        myrpm->depends_on[j] = strdup((char*) rpmtdGetString(td));
        rpmtdNext(td);
    }
}
void get_provides_version(my_rpm_struct* myrpm, Header hdr, rpmtd td)
{
    int rc;
    rpmtdFreeData(td);
    rpmtdReset(td);
    rc = headerGet(hdr, RPMTAG_PROVIDEVERSION, td, HEADERGET_EXT);
    myrpm->provides_version = (char**)malloc(myrpm->provides_count*sizeof(char*));
    int j;
    for (j = 0; j < myrpm->provides_count; j++){
        myrpm->provides_version[j] = strdup((char*) rpmtdGetString(td));
        rpmtdNext(td);
    }

}
void  get_provides(my_rpm_struct* myrpm, Header hdr, rpmtd td)
{
    int rc;
    rpmtdFreeData(td);
    rpmtdReset(td);
    rc = headerGet(hdr, RPMTAG_PROVIDENAME, td, HEADERGET_EXT);
    myrpm->provides_count = rpmtdCount(td);
    myrpm->provides = (char**)malloc(myrpm->provides_count*sizeof(char*));
    int j;
    for (j = 0; j < myrpm->provides_count; j++){
        myrpm->provides[j] = strdup((char*) rpmtdGetString(td));
        rpmtdNext(td);
    }
}

char* get_txt(Header hdr, rpmtd td, rpmTagVal tag){
    int rc;
    char* name;
    rpmtdFreeData(td);
    rpmtdReset(td);
    rc = headerGet(hdr, tag, td, HEADERGET_EXT);
    if (rpmtdGetString(td)  != NULL)
    {
        name =  strdup((char*) rpmtdGetString(td));
    } else{
        name =  strdup((char*) "");
    };
    return name;
}

void parse_rpm (char* filename,my_rpm_struct* myrpm)
{
    int i;
    rpmts ts;

    FD_t fd;
    rpmRC rc;
    Header hdr;
    rpmtd td;
    rpmVSFlags vsflags = 0;

    fd = Fopen(filename, "r.ufdio");
    if ((!fd) || Ferror(fd)) {
        myrpm->error = 1;
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
        Fclose(fd);
        myrpm->error = 2;
        return;
    }
    //hdr = headerLink(hdr);
    Fclose(fd);

    td = rpmtdNew();
    myrpm->name = get_txt(hdr,td, RPMTAG_NAME);
    myrpm->version = get_txt(hdr,td, RPMTAG_VERSION);
    myrpm->epoch = get_txt(hdr,td, RPMTAG_EPOCH);
    myrpm->release = get_txt(hdr,td, RPMTAG_RELEASE);
    myrpm->arch   = get_txt(hdr,td, RPMTAG_ARCH);
    myrpm->summary   = get_txt(hdr,td, RPMTAG_SUMMARY);
    myrpm->description   = get_txt(hdr,td, RPMTAG_DESCRIPTION);
    myrpm->url   = get_txt(hdr,td, RPMTAG_URL);
    myrpm->vendor = get_txt(hdr,td, RPMTAG_VENDOR);
    myrpm->license   = get_txt(hdr,td, RPMTAG_LICENSE);
    get_req(myrpm,hdr,td);
    get_requires_version(myrpm, hdr, td);
    get_provides(myrpm,hdr,td);
    get_provides_version(myrpm, hdr, td);

    // rpmFreeMacros (NULL);
    rpmtdFreeData(td);
    rpmtdFree(td);
    headerFree(hdr);
    rpmtsFree(ts);
    rpmFreeCrypto();
    myrpm->error = 0; 
}
