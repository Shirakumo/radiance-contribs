## About
This is a database interface implementation using [LambdaLite](https://github.com/Wukix/LambdaLite) as the backend. As such it does not enforce data type correctness and completely ignores the collection schema. However, it has no external dependencies and is thus easy to set up.

## How To
The configuration looks as follows:

```
:default ; => Database connection to connect to on startup.
name     ; => Pathname of the connection storage location.
```

The pathname is relative to the configuration directory. On first init, two default connections named `"radiance"` and `"test"` are added with the paths `"radiance.db"` and `"test.db"` respectively. The radiance db is set as the default.
