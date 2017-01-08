@0xa2fcad306f2b650b;

struct FileCounts {
  entries @0 :List(Entry);
  truncated @1 :Bool;

  struct Entry {
    fileType @0 :Text;
    count @1 :UInt32;
  }
}

interface VersionControlPlugin {
  status @0 (directory :Text)
         -> (branch :Text, fileCounts :FileCounts);
}

struct Plugin {
  union {
    versionControl :group {
      vcName @0 :Text;
      plugin @1 :VersionControlPlugin;
    }
    dummy @2 :Void;
  }
}

interface PluginProcess {
  initialize @0 () -> (plugins :List(Plugin));
}
