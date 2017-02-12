## About
This is an implementation for the database interface, using [SQLite](https://sqlite.org/) as the backend, and connecting to it via [cl-sqlite](https://common-lisp.net/project/cl-sqlite/). The sqlite-pcre extension must be installed on the system, or regex queries will fail.

## How To
The configuration is as follows:

```
:default          ; => The name of the connection to use on startup.
:connections name ; => A pathname to the sqlite storage file
```

The pathname of the storage file is relative to the configuration directory. On first launch, two connections named `"radiance"` and `"test"` are set up, with storage pathnames being `"radiance.db"` and `"test.db"` respectively. The default is set to `"radiance"`.
