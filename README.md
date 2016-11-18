## About Radiance-Contribs
This is a collection of useful helper modules and standard implementations for Radiance interfaces.

## Standard Implementations
### Server
#### i-hunchentoot
Bridge to the [Hunchentoot](http://weitz.de/hunchentoot/) HTTP server. This is the default backend, though also rather slow.

#### i-woo
Bridge to the (currnetly) fastest CL HTTP server [Woo](https://github.com/fukamachi/woo).

#### i-wookie
Bridge to the asynchronous HTTP server [Wookie](https://github.com/orthecreedence/wookie).

### Database
#### i-lambdalite
Bridge to the [LambdaLite](https://github.com/Wukix/LambdaLite) CL object-store. This is the default backend, but certainly not the fastest.

#### i-postmodern
Bridge to [PostgreSQL](https://postgresql.org) servers using the [Postmodern](http://marijnhaverbeke.nl/postmodern/) native protocol implementation. This is the backend you will probably want to use on production setups.

#### i-sqlite
Bridge to the single-library [SQLite](https://sqlite.org/) database using [cl-sqlite](https://common-lisp.net/project/cl-sqlite/).

### Logger
#### i-log4cl
Bridge to the popular [log4cl](https://github.com/sharplispers/log4cl) logging library.

#### i-verbose
Bridge to the extensible [Verbose](http://shinmera.github.io/verbose/) logging framework. This is the default backend.

### Auth
#### r-remote-auth
Allows authentication against a remote oAuth service.

#### r-simple-auth
Allows authentication through a simple password.

### Admin
#### r-simple-admin
A simple, yet stylish, default implementation of an administration panel site.

### Ban
#### r-simple-ban
Allows performing bans based on IP address of the user.

### Cache
#### r-simple-cache
A very crude file-based cache. Stores everything on file, loads everything from file. No memory caching at all.

### Data-Model
#### r-simple-model
Very basic and simple data-model implementation. Though I'm not sure if it can be improved very much.

### Profile
#### r-simple-profile
A simple, yet stylish, default implementation of a user profile site.

### Rate
#### r-simple-rate
Very simple resource rate limiting that uses the database to store timeout information.

### Session
#### r-simple-sessions
Simple yet secure sessions implemented using HTTP cookies. Uses a prune-thread in the back to clear dead sessions regularly.

### User
#### r-simple-users
Basic user objects that are backed using some database collections. Full support for the branch-based permissions system.

## Helper Modules
### r-web-debug
Shows an informative 'debugger' site that includes a full stack trace, arguments list, and even source information, when an error occurs. Useful for development setups.

### r-simple-errors
Shows basic error pages that don't give too much information about the failure, as to not accidentally leak sensitive information. Useful for production setups, though you might also want to provide your own system like this to show the templates you want.

### r-welcome
Basic welcome module to introduce users to Radiance on a first launch.

### r-clip
Extensions to the [Clip](https://shinmera.github.io/clip) template system to make it more convenient for use in Radiance.

### r-forms
An unfinished form building system.

### r-orm
An unfinished Object Relational System using the database interface as backend.

### r-ratify
Extensions to the [Ratify](https://shinmera.github.io/ratify) type and format checking systems to make it more convenient for use in Radiance.

### i-json
Provides a JSON API output. Loading this will change the default api output to JSON.
