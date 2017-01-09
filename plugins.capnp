@0xa2fcad306f2b650b;

struct Option(T) {
  union {
    none @0 :Void;
    some @1 :T;
  }
}

struct FileCounts {
  entries @0 :List(Entry);
  truncated @1 :Bool;

  struct Entry {
    fileType @0 :Text;
    count @1 :UInt32;
  }
}

interface VersionControlPlugin {
  struct Status {
    branch @0 :Text;
    displayBranch @1 :Text;
    counts @2 :FileCounts;
  }

  status @0 (directory :Text, branchOnly :Bool) -> (status :Option(Status));
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
