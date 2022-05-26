## About
This is an implementation for the database interface, using [PostgreSQL](https://postgresql.org) through the [Postmodern](http://marijnhaverbeke.nl/postmodern/) connection library.

## How To
This interface will not configure any defaults for you, as it has no way of guessing them. You will have to configure it manually before it can be used. The configuration format is as follows:

```
:default                    ; => The name of the connection to use on startup.
:connections name :host     ; => The host of the database server. Defaults to "localhost".
:connections name :port     ; => The port of the database server. Defaults to 5432.
:connections name :user     ; => The username to connect with.
:connections name :pass     ; => The password for the connecting user.
:connections name :ssl      ; => Whether to use SSL or not.
:connections name :database ; => The name of the database to connect to.
```
