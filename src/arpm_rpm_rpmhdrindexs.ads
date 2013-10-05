package arpm_rpm_rpmhdrindexs is 
    type rpmhdrindex is private; 
    type rpmhdrindex_access is access all rpmhdrindex; 
    function Tag(index : in rpmhdrindex) return String; 
    function Format(index : in rpmhdrindex) return String; 
    function Format(index : in rpmhdrindex) return Integer; 
    function Tag(Index : in rpmhdrindex) return Integer; 

    private
    type four_byte_number is range 0..2**(4*8)-1;
    for four_byte_number'Size use 32; 
    type tags_type is (
        RPMTAG_HEADERSIGNATURES,
        RPMTAG_HEADERIMMUTABLE,
        RPMTAG_HEADERI18NTABLE,
        RPMTAG_NAME,
        RPMTAG_VERSION,
        RPMTAG_RELEASE,
        RPMTAG_EPOCH,
        RPMTAG_SUMMARY,
        RPMTAG_DESCRIPTION,
        RPMTAG_BUILDTIME,
        RPMTAG_BUILDHOST,
        RPMTAG_INSTALLTIME,
        RPMTAG_SIZE,
        RPMTAG_DISTRIBUTION,
        RPMTAG_VENDOR,
        RPMTAG_GIF,
        RPMTAG_XPM,
        RPMTAG_LICENSE,
        RPMTAG_PACKAGER,
        RPMTAG_GROUP,
        RPMTAG_CHANGELOG,
        RPMTAG_SOURCE,
        RPMTAG_PATCH,
        RPMTAG_URL,
        RPMTAG_OS,
        RPMTAG_ARCH,
        RPMTAG_PREIN,
        RPMTAG_POSTIN,
        RPMTAG_PREUN,
        RPMTAG_POSTUN,
        RPMTAG_OLDFILENAMES,
        RPMTAG_FILESIZES,
        RPMTAG_FILESTATES,
        RPMTAG_FILEMODES,
        RPMTAG_FILEUIDS,
        RPMTAG_FILEGIDS,
        RPMTAG_FILERDEVS,
        RPMTAG_FILEMTIMES,
        RPMTAG_FILEDIGESTS,
        RPMTAG_FILELINKTOS,
        RPMTAG_FILEFLAGS,
        RPMTAG_ROOT,
        RPMTAG_FILEUSERNAME,
        RPMTAG_FILEGROUPNAME,
        RPMTAG_EXCLUDE,
        RPMTAG_EXCLUSIVE,
        RPMTAG_ICON,
        RPMTAG_SOURCERPM,
        RPMTAG_FILEVERIFYFLAGS,
        RPMTAG_ARCHIVESIZE,
        RPMTAG_PROVIDENAME,
        RPMTAG_REQUIREFLAGS,
        RPMTAG_REQUIRENAME,
        RPMTAG_REQUIREVERSION,
        RPMTAG_NOSOURCE,
        RPMTAG_NOPATCH,
        RPMTAG_CONFLICTFLAGS,
        RPMTAG_CONFLICTNAME,
        RPMTAG_CONFLICTVERSION,
        RPMTAG_DEFAULTPREFIX,
        RPMTAG_BUILDROOT,
        RPMTAG_INSTALLPREFIX,
        RPMTAG_EXCLUDEARCH,
        RPMTAG_EXCLUDEOS,
        RPMTAG_EXCLUSIVEARCH,
        RPMTAG_EXCLUSIVEOS,
        RPMTAG_AUTOREQPROV,
        RPMTAG_RPMVERSION,
        RPMTAG_TRIGGERSCRIPTS,
        RPMTAG_TRIGGERNAME,
        RPMTAG_TRIGGERVERSION,
        RPMTAG_TRIGGERFLAGS,
        RPMTAG_TRIGGERINDEX,
        RPMTAG_VERIFYSCRIPT,
        RPMTAG_CHANGELOGTIME,
        RPMTAG_CHANGELOGNAME,
        RPMTAG_CHANGELOGTEXT,
        RPMTAG_BROKENMD5,
        RPMTAG_PREREQ,
        RPMTAG_PREINPROG,
        RPMTAG_POSTINPROG,
        RPMTAG_PREUNPROG,
        RPMTAG_POSTUNPROG,
        RPMTAG_BUILDARCHS,
        RPMTAG_OBSOLETENAME,
        RPMTAG_VERIFYSCRIPTPROG,
        RPMTAG_TRIGGERSCRIPTPROG,
        RPMTAG_DOCDIR,
        RPMTAG_COOKIE,
        RPMTAG_FILEDEVICES,
        RPMTAG_FILEINODES,
        RPMTAG_FILELANGS,
        RPMTAG_PREFIXES,
        RPMTAG_INSTPREFIXES,
        RPMTAG_TRIGGERIN,
        RPMTAG_TRIGGERUN,
        RPMTAG_TRIGGERPOSTUN,
        RPMTAG_AUTOREQ,
        RPMTAG_AUTOPROV,
        RPMTAG_CAPABILITY,
        RPMTAG_SOURCEPACKAGE,
        RPMTAG_OLDORIGFILENAMES,
        RPMTAG_BUILDPREREQ,
        RPMTAG_BUILDREQUIRES,
        RPMTAG_BUILDCONFLICTS,
        RPMTAG_BUILDMACROS,
        RPMTAG_PROVIDEFLAGS,
        RPMTAG_PROVIDEVERSION,
        RPMTAG_OBSOLETEFLAGS,
        RPMTAG_OBSOLETEVERSION,
        RPMTAG_DIRINDEXES,
        RPMTAG_BASENAMES,
        RPMTAG_DIRNAMES,
        RPMTAG_ORIGDIRINDEXES,
        RPMTAG_ORIGBASENAMES,
        RPMTAG_ORIGDIRNAMES,
        RPMTAG_OPTFLAGS,
        RPMTAG_DISTURL,
        RPMTAG_PAYLOADFORMAT,
        RPMTAG_PAYLOADCOMPRESSOR,
        RPMTAG_PAYLOADFLAGS,
        RPMTAG_INSTALLCOLOR,
        RPMTAG_INSTALLTID,
        RPMTAG_REMOVETID,
        RPMTAG_SHA1RHN,
        RPMTAG_RHNPLATFORM,
        RPMTAG_PLATFORM,
        RPMTAG_PATCHESNAME,
        RPMTAG_PATCHESFLAGS,
        RPMTAG_PATCHESVERSION,
        RPMTAG_CACHECTIME,
        RPMTAG_CACHEPKGPATH,
        RPMTAG_CACHEPKGSIZE,
        RPMTAG_CACHEPKGMTIME,
        RPMTAG_FILECOLORS,
        RPMTAG_FILECLASS,
        RPMTAG_CLASSDICT,
        RPMTAG_FILEDEPENDSX,
        RPMTAG_FILEDEPENDSN,
        RPMTAG_DEPENDSDICT,
        RPMTAG_SOURCEPKGID,
        RPMTAG_FILECONTEXTS,
        RPMTAG_FSCONTEXTS,
        RPMTAG_RECONTEXTS,
        RPMTAG_POLICIES,
        RPMTAG_PRETRANS,
        RPMTAG_POSTTRANS,
        RPMTAG_PRETRANSPROG,
        RPMTAG_POSTTRANSPROG,
        RPMTAG_DISTTAG,
        RPMTAG_SUGGESTSNAME,
        RPMTAG_SUGGESTSVERSION,
        RPMTAG_SUGGESTSFLAGS,
        RPMTAG_ENHANCESNAME,
        RPMTAG_ENHANCESVERSION,
        RPMTAG_ENHANCESFLAGS,
        RPMTAG_PRIORITY,
        RPMTAG_CVSID,
        RPMTAG_BLINKPKGID,
        RPMTAG_BLINKHDRID,
        RPMTAG_BLINKNEVRA,
        RPMTAG_FLINKPKGID,
        RPMTAG_FLINKHDRID,
        RPMTAG_FLINKNEVRA,
        RPMTAG_PACKAGEORIGIN,
        RPMTAG_TRIGGERPREIN,
        RPMTAG_BUILDSUGGESTS,
        RPMTAG_BUILDENHANCES,
        RPMTAG_SCRIPTSTATES,
        RPMTAG_SCRIPTMETRICS,
        RPMTAG_BUILDCPUCLOCK,
        RPMTAG_FILEDIGESTALGOS,
        RPMTAG_VARIANTS,
        RPMTAG_XMAJOR,
        RPMTAG_XMINOR,
        RPMTAG_REPOTAG,
        RPMTAG_KEYWORDS,
        RPMTAG_BUILDPLATFORMS,
        RPMTAG_PACKAGECOLOR,
        RPMTAG_PACKAGEPREFCOLOR,
        RPMTAG_XATTRSDICT,
        RPMTAG_FILEXATTRSX,
        RPMTAG_DEPATTRSDICT,
        RPMTAG_CONFLICTATTRSX,
        RPMTAG_OBSOLETEATTRSX,
        RPMTAG_PROVIDEATTRSX,
        RPMTAG_REQUIREATTRSX,
        RPMTAG_BUILDPROVIDES,
        RPMTAG_BUILDOBSOLETES,
        RPMTAG_DBINSTANCE,
        RPMTAG_NVRA,
        RPMTAG_FILENAMES,
        RPMTAG_FILEPROVIDE,
        RPMTAG_FILEREQUIRE,
        RPMTAG_FSNAMES,
        RPMTAG_FSSIZES,
        RPMTAG_TRIGGERCONDS,
        RPMTAG_TRIGGERTYPE,
        RPMTAG_ORIGFILENAMES,
        RPMTAG_LONGFILESIZES,
        RPMTAG_LONGSIZE,
        RPMTAG_FILECAPS,
        RPMTAG_FILEDIGESTALGO,
        RPMTAG_BUGURL,
        RPMTAG_EVR,
        RPMTAG_NVR,
        RPMTAG_NEVR,
        RPMTAG_NEVRA,
        RPMTAG_HEADERCOLOR,
        RPMTAG_VERBOSE,
        RPMTAG_EPOCHNUM,
        RPMTAG_PREINFLAGS,
        RPMTAG_POSTINFLAGS,
        RPMTAG_PREUNFLAGS,
        RPMTAG_POSTUNFLAGS,
        RPMTAG_PRETRANSFLAGS,
        RPMTAG_POSTTRANSFLAGS,
        RPMTAG_VERIFYSCRIPTFLAGS,
        RPMTAG_TRIGGERSCRIPTFLAGS,
        RPMTAG_COLLECTIONS,
        RPMTAG_POLICYNAMES,
        RPMTAG_POLICYTYPES,
        RPMTAG_POLICYTYPESINDEXES,
        RPMTAG_POLICYFLAGS,
        RPMTAG_VCS,
        RPMTAG_ORDERNAME,
        RPMTAG_ORDERVERSION,
        RPMTAG_ORDERFLAGS,
        RPMTAG_MSSFMANIFEST,
        RPMTAG_MSSFDOMAIN,
        RPMTAG_INSTFILENAMES,
        RPMTAG_REQUIRENEVRS,
        RPMTAG_PROVIDENEVRS,
        RPMTAG_OBSOLETENEVRS,
        RPMTAG_CONFLICTNEVRS,
        RPMTAG_FILENLINKS          
        ); 
    for tags_type use (
        RPMTAG_HEADERSIGNATURES => 62, 
        RPMTAG_HEADERIMMUTABLE => 63, 
        RPMTAG_HEADERI18NTABLE => 100, 
        RPMTAG_NAME                 => 1000,        
        RPMTAG_VERSION              => 1001,        
        RPMTAG_RELEASE              => 1002,        
        RPMTAG_EPOCH                => 1003,        
        RPMTAG_SUMMARY              => 1004,        
        RPMTAG_DESCRIPTION          => 1005,        
        RPMTAG_BUILDTIME            => 1006,        
        RPMTAG_BUILDHOST            => 1007,        
        RPMTAG_INSTALLTIME          => 1008,        
        RPMTAG_SIZE                 => 1009,        
        RPMTAG_DISTRIBUTION         => 1010,        
        RPMTAG_VENDOR               => 1011,        
        RPMTAG_GIF                  => 1012,        
        RPMTAG_XPM                  => 1013,        
        RPMTAG_LICENSE              => 1014,        
        RPMTAG_PACKAGER             => 1015,        
        RPMTAG_GROUP                => 1016,        
        RPMTAG_CHANGELOG            => 1017, 
        RPMTAG_SOURCE               => 1018,        
        RPMTAG_PATCH                => 1019,        
        RPMTAG_URL                  => 1020,        
        RPMTAG_OS                   => 1021,        
        RPMTAG_ARCH                 => 1022,        
        RPMTAG_PREIN                => 1023,        
        RPMTAG_POSTIN               => 1024,        
        RPMTAG_PREUN                => 1025,        
        RPMTAG_POSTUN               => 1026,        
        RPMTAG_OLDFILENAMES         => 1027, 
        RPMTAG_FILESIZES            => 1028,        
        RPMTAG_FILESTATES           => 1029, 
        RPMTAG_FILEMODES            => 1030,        
        RPMTAG_FILEUIDS             => 1031, 
        RPMTAG_FILEGIDS             => 1032, 
        RPMTAG_FILERDEVS            => 1033,        
        RPMTAG_FILEMTIMES           => 1034, 
        RPMTAG_FILEDIGESTS          => 1035,        
        RPMTAG_FILELINKTOS          => 1036,        
        RPMTAG_FILEFLAGS            => 1037,        
        RPMTAG_ROOT                 => 1038, 
        RPMTAG_FILEUSERNAME         => 1039,        
        RPMTAG_FILEGROUPNAME        => 1040,        
        RPMTAG_EXCLUDE              => 1041, 
        RPMTAG_EXCLUSIVE            => 1042, 
        RPMTAG_ICON                 => 1043, 
        RPMTAG_SOURCERPM            => 1044,        
        RPMTAG_FILEVERIFYFLAGS      => 1045,        
        RPMTAG_ARCHIVESIZE          => 1046,        
        RPMTAG_PROVIDENAME          => 1047,        
        RPMTAG_REQUIREFLAGS         => 1048,        
        RPMTAG_REQUIRENAME          => 1049,        
        RPMTAG_REQUIREVERSION       => 1050,        
        RPMTAG_NOSOURCE             => 1051, 
        RPMTAG_NOPATCH              => 1052, 
        RPMTAG_CONFLICTFLAGS        => 1053, 
        RPMTAG_CONFLICTNAME         => 1054,        
        RPMTAG_CONFLICTVERSION      => 1055,        
        RPMTAG_DEFAULTPREFIX        => 1056, 
        RPMTAG_BUILDROOT            => 1057, 
        RPMTAG_INSTALLPREFIX        => 1058, 
        RPMTAG_EXCLUDEARCH          => 1059, 
        RPMTAG_EXCLUDEOS            => 1060, 
        RPMTAG_EXCLUSIVEARCH        => 1061, 
        RPMTAG_EXCLUSIVEOS          => 1062, 
        RPMTAG_AUTOREQPROV          => 1063, 
        RPMTAG_RPMVERSION           => 1064,        
        RPMTAG_TRIGGERSCRIPTS       => 1065,        
        RPMTAG_TRIGGERNAME          => 1066,        
        RPMTAG_TRIGGERVERSION       => 1067,        
        RPMTAG_TRIGGERFLAGS         => 1068,        
        RPMTAG_TRIGGERINDEX         => 1069,        
        RPMTAG_VERIFYSCRIPT         => 1079,        
        RPMTAG_CHANGELOGTIME        => 1080,        
        RPMTAG_CHANGELOGNAME        => 1081,        
        RPMTAG_CHANGELOGTEXT        => 1082,        
        RPMTAG_BROKENMD5            => 1083, 
        RPMTAG_PREREQ               => 1084, 
        RPMTAG_PREINPROG            => 1085,        
        RPMTAG_POSTINPROG           => 1086,        
        RPMTAG_PREUNPROG            => 1087,        
        RPMTAG_POSTUNPROG           => 1088,        
        RPMTAG_BUILDARCHS           => 1089, 
        RPMTAG_OBSOLETENAME         => 1090,        
        RPMTAG_VERIFYSCRIPTPROG     => 1091,        
        RPMTAG_TRIGGERSCRIPTPROG    => 1092,        
        RPMTAG_DOCDIR               => 1093, 
        RPMTAG_COOKIE               => 1094,        
        RPMTAG_FILEDEVICES          => 1095,        
        RPMTAG_FILEINODES           => 1096,        
        RPMTAG_FILELANGS            => 1097,        
        RPMTAG_PREFIXES             => 1098,        
        RPMTAG_INSTPREFIXES         => 1099,        
        RPMTAG_TRIGGERIN            => 1100, 
        RPMTAG_TRIGGERUN            => 1101, 
        RPMTAG_TRIGGERPOSTUN        => 1102, 
        RPMTAG_AUTOREQ              => 1103, 
        RPMTAG_AUTOPROV             => 1104, 
        RPMTAG_CAPABILITY           => 1105, 
        RPMTAG_SOURCEPACKAGE        => 1106, 
        RPMTAG_OLDORIGFILENAMES     => 1107, 
        RPMTAG_BUILDPREREQ          => 1108, 
        RPMTAG_BUILDREQUIRES        => 1109, 
        RPMTAG_BUILDCONFLICTS       => 1110, 
        RPMTAG_BUILDMACROS          => 1111, 
        RPMTAG_PROVIDEFLAGS         => 1112,        
        RPMTAG_PROVIDEVERSION       => 1113,        
        RPMTAG_OBSOLETEFLAGS        => 1114,        
        RPMTAG_OBSOLETEVERSION      => 1115,        
        RPMTAG_DIRINDEXES           => 1116,        
        RPMTAG_BASENAMES            => 1117,        
        RPMTAG_DIRNAMES             => 1118,        
        RPMTAG_ORIGDIRINDEXES       => 1119, 
        RPMTAG_ORIGBASENAMES        => 1120, 
        RPMTAG_ORIGDIRNAMES         => 1121, 
        RPMTAG_OPTFLAGS             => 1122,        
        RPMTAG_DISTURL              => 1123,        
        RPMTAG_PAYLOADFORMAT        => 1124,        
        RPMTAG_PAYLOADCOMPRESSOR    => 1125,        
        RPMTAG_PAYLOADFLAGS         => 1126,        
        RPMTAG_INSTALLCOLOR         => 1127, 
        RPMTAG_INSTALLTID           => 1128,        
        RPMTAG_REMOVETID            => 1129,        
        RPMTAG_SHA1RHN              => 1130, 
        RPMTAG_RHNPLATFORM          => 1131,        
        RPMTAG_PLATFORM             => 1132,        
        RPMTAG_PATCHESNAME          => 1133, 
        RPMTAG_PATCHESFLAGS         => 1134, 
        RPMTAG_PATCHESVERSION       => 1135, 
        RPMTAG_CACHECTIME           => 1136,        
        RPMTAG_CACHEPKGPATH         => 1137,        
        RPMTAG_CACHEPKGSIZE         => 1138,        
        RPMTAG_CACHEPKGMTIME        => 1139,        
        RPMTAG_FILECOLORS           => 1140,        
        RPMTAG_FILECLASS            => 1141,        
        RPMTAG_CLASSDICT            => 1142,        
        RPMTAG_FILEDEPENDSX         => 1143,        
        RPMTAG_FILEDEPENDSN         => 1144,        
        RPMTAG_DEPENDSDICT          => 1145,        
        RPMTAG_SOURCEPKGID          => 1146,        
        RPMTAG_FILECONTEXTS         => 1147,        
        RPMTAG_FSCONTEXTS           => 1148,        
        RPMTAG_RECONTEXTS           => 1149,        
        RPMTAG_POLICIES             => 1150,        
        RPMTAG_PRETRANS             => 1151,        
        RPMTAG_POSTTRANS            => 1152,        
        RPMTAG_PRETRANSPROG         => 1153,        
        RPMTAG_POSTTRANSPROG        => 1154,        
        RPMTAG_DISTTAG              => 1155,        
        RPMTAG_SUGGESTSNAME         => 1156,        
        RPMTAG_SUGGESTSVERSION      => 1157,        
        RPMTAG_SUGGESTSFLAGS        => 1158,        
        RPMTAG_ENHANCESNAME         => 1159,        
        RPMTAG_ENHANCESVERSION      => 1160,        
        RPMTAG_ENHANCESFLAGS        => 1161,        
        RPMTAG_PRIORITY             => 1162, 
        RPMTAG_CVSID                => 1163, 
        RPMTAG_BLINKPKGID           => 1164, 
        RPMTAG_BLINKHDRID           => 1165, 
        RPMTAG_BLINKNEVRA           => 1166, 
        RPMTAG_FLINKPKGID           => 1167, 
        RPMTAG_FLINKHDRID           => 1168, 
        RPMTAG_FLINKNEVRA           => 1169, 
        RPMTAG_PACKAGEORIGIN        => 1170, 
        RPMTAG_TRIGGERPREIN         => 1171, 
        RPMTAG_BUILDSUGGESTS        => 1172, 
        RPMTAG_BUILDENHANCES        => 1173, 
        RPMTAG_SCRIPTSTATES         => 1174, 
        RPMTAG_SCRIPTMETRICS        => 1175, 
        RPMTAG_BUILDCPUCLOCK        => 1176, 
        RPMTAG_FILEDIGESTALGOS      => 1177, 
        RPMTAG_VARIANTS             => 1178, 
        RPMTAG_XMAJOR               => 1179, 
        RPMTAG_XMINOR               => 1180, 
        RPMTAG_REPOTAG              => 1181,        
        RPMTAG_KEYWORDS             => 1182,        
        RPMTAG_BUILDPLATFORMS       => 1183,        
        RPMTAG_PACKAGECOLOR         => 1184, 
        RPMTAG_PACKAGEPREFCOLOR     => 1185, 
        RPMTAG_XATTRSDICT           => 1186, 
        RPMTAG_FILEXATTRSX          => 1187, 
        RPMTAG_DEPATTRSDICT         => 1188, 
        RPMTAG_CONFLICTATTRSX       => 1189, 
        RPMTAG_OBSOLETEATTRSX       => 1190, 
        RPMTAG_PROVIDEATTRSX        => 1191, 
        RPMTAG_REQUIREATTRSX        => 1192, 
        RPMTAG_BUILDPROVIDES        => 1193, 
        RPMTAG_BUILDOBSOLETES       => 1194, 
        RPMTAG_DBINSTANCE           => 1195, 
        RPMTAG_NVRA                 => 1196, 
        RPMTAG_FILENAMES            => 5000, 
        RPMTAG_FILEPROVIDE          => 5001, 
        RPMTAG_FILEREQUIRE          => 5002, 
        RPMTAG_FSNAMES              => 5003, 
        RPMTAG_FSSIZES              => 5004, 
        RPMTAG_TRIGGERCONDS         => 5005, 
        RPMTAG_TRIGGERTYPE          => 5006, 
        RPMTAG_ORIGFILENAMES        => 5007, 
        RPMTAG_LONGFILESIZES        => 5008,        
        RPMTAG_LONGSIZE             => 5009, 
        RPMTAG_FILECAPS             => 5010, 
        RPMTAG_FILEDIGESTALGO       => 5011, 
        RPMTAG_BUGURL               => 5012, 
        RPMTAG_EVR                  => 5013, 
        RPMTAG_NVR                  => 5014, 
        RPMTAG_NEVR                 => 5015, 
        RPMTAG_NEVRA                => 5016, 
        RPMTAG_HEADERCOLOR          => 5017, 
        RPMTAG_VERBOSE              => 5018, 
        RPMTAG_EPOCHNUM             => 5019, 
        RPMTAG_PREINFLAGS           => 5020, 
        RPMTAG_POSTINFLAGS          => 5021, 
        RPMTAG_PREUNFLAGS           => 5022, 
        RPMTAG_POSTUNFLAGS          => 5023, 
        RPMTAG_PRETRANSFLAGS        => 5024, 
        RPMTAG_POSTTRANSFLAGS       => 5025, 
        RPMTAG_VERIFYSCRIPTFLAGS    => 5026, 
        RPMTAG_TRIGGERSCRIPTFLAGS   => 5027, 
        RPMTAG_COLLECTIONS          => 5029, 
        RPMTAG_POLICYNAMES          => 5030,        
        RPMTAG_POLICYTYPES          => 5031,        
        RPMTAG_POLICYTYPESINDEXES   => 5032,        
        RPMTAG_POLICYFLAGS          => 5033,        
        RPMTAG_VCS                  => 5034, 
        RPMTAG_ORDERNAME            => 5035,        
        RPMTAG_ORDERVERSION         => 5036,        
        RPMTAG_ORDERFLAGS           => 5037,        
        RPMTAG_MSSFMANIFEST         => 5038, 
        RPMTAG_MSSFDOMAIN           => 5039, 
        RPMTAG_INSTFILENAMES        => 5040, 
        RPMTAG_REQUIRENEVRS         => 5041, 
        RPMTAG_PROVIDENEVRS         => 5042, 
        RPMTAG_OBSOLETENEVRS        => 5043, 
        RPMTAG_CONFLICTNEVRS        => 5044, 
        RPMTAG_FILENLINKS           => 5045 
        ); 

    type format_type is 
        (RPM_NULL_TYPE, 
        RPM_CHAR_TYPE, 
        RPM_INT8_TYPE, 
        RPM_INT16_TYPE, 
        RPM_INT32_TYPE, 
        RPM_INT64_TYPE, 
        RPM_STRING_TYPE, 
        RPM_BIN_TYPE, 
        RPM_STRING_ARRAY_TYPE, 
        RPM_I18NSTRING_TYPE); 
    for format_type use 
        (RPM_NULL_TYPE => 0, 
        RPM_CHAR_TYPE => 1, 
        RPM_INT8_TYPE => 2, 
        RPM_INT16_TYPE => 3, 
        RPM_INT32_TYPE => 4, 
        RPM_INT64_TYPE => 5, 
        RPM_STRING_TYPE => 6, 
        RPM_BIN_TYPE => 7, 
        RPM_STRING_ARRAY_TYPE => 8 , 
        RPM_I18NSTRING_TYPE =>9
        ); 
    type rpmhdrindex is record
        tag : four_byte_number := 0; 
        format : four_byte_number := 0; 
        data_position  :  four_byte_number := 0; 
        number_of_data_items  : four_byte_number := 0;
    end record; 
    for rpmhdrindex'Size use 16*8; 


end arpm_rpm_rpmhdrindexs; 

