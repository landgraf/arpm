PRAGMA foreign_keys = ON;
PRAGMA encoding = "UTF-8";
PRAGMA default_cache_size = 4000;
PRAGMA synchronous = OFF;
PRAGMA temp_store = MEMORY;
PRAGMA journal_mode = OFF;
CREATE TABLE conflicts (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65));
CREATE TABLE db_info (dbversion INTEGER, checksum TEXT);
CREATE TABLE files (  name TEXT,  type TEXT,  pkgKey CHAR(65));
CREATE TABLE obsoletes (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65) );
CREATE TABLE packages (  pkgKey CHAR(65) PRIMARY KEY,  pkgId TEXT,  name TEXT,  arch TEXT,  version TEXT,  epoch TEXT,  release TEXT,  summary TEXT,  description TEXT,  url TEXT,  time_file INTEGER,  time_build INTEGER,  rpm_license TEXT,  rpm_vendor TEXT,  rpm_group TEXT,  rpm_buildhost TEXT,  rpm_sourcerpm TEXT,  rpm_header_start INTEGER,  rpm_header_end INTEGER,  rpm_packager TEXT,  size_package INTEGER,  size_installed INTEGER,  size_archive INTEGER,  location_href TEXT,  location_base TEXT,  checksum_type TEXT);
CREATE TABLE provides (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgKey CHAR(65) , provideKey CHAR(65));
CREATE TABLE packages_provides ( pkgKey CHAR(65), provideKey CHAR(65), FOREIGN KEY(pkgKey) REFERENCES packages(pkgKey) ON DELETE CASCADE, 
    FOREIGN KEY(provideKey) REFERENCES provides(providesKey) ON DELETE CASCADE);
CREATE TABLE requires (  name TEXT,  flags TEXT,  epoch TEXT,  version TEXT,  release TEXT,  pkgkey CHAR(65) , pre BOOLEAN DEFAULT FALSE, requirekey CHAR(65));
CREATE TABLE packages_requires ( pkgkey CHAR(65), requirekey CHAR(65), FOREIGN KEY(pkgKey) REFERENCES packages(pkgKey) ON DELETE CASCADE, 
    FOREIGN KEY(requireKey) REFERENCES requires(requiresKey) ON DELETE CASCADE);
CREATE INDEX filenames ON files (name);
CREATE INDEX packageId ON packages (pkgId);
CREATE INDEX packagename ON packages (name);
CREATE INDEX pkgconflicts on conflicts (pkgKey);
CREATE INDEX pkgfiles ON files (pkgKey);
CREATE INDEX pkgobsoletes on obsoletes (pkgKey);
CREATE INDEX pkgprovides on provides (pkgKey);
CREATE INDEX pkgrequires on requires (pkgKey);
CREATE INDEX providesname ON provides (name);
CREATE INDEX requiresname ON requires (name);
CREATE INDEX packages_provides_providekey ON packages_provides(provideKey);
CREATE INDEX packages_provides_pkgkey ON packages_provides(pkgKey);
CREATE INDEX packages_requires_requirekey ON packages_requires(requireKey);
CREATE INDEX packages_requires_pkgkey ON packages_requires(pkgKey);
CREATE TRIGGER removals AFTER DELETE ON packages  BEGIN    DELETE FROM files WHERE pkgKey = old.pkgKey;    DELETE FROM requires WHERE pkgKey = old.pkgKey;    DELETE FROM provides WHERE pkgKey = old.pkgKey;    DELETE FROM conflicts WHERE pkgKey = old.pkgKey;    DELETE FROM obsoletes WHERE pkgKey = old.pkgKey;  END;

