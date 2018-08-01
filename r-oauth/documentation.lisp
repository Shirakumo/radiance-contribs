#|
 This file is a part of Radiance
 (c) 2018 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:oauth)

(docs:define-docs
  (type server
    "Class representing the server object used for Radiance's oAuth.

See NORTH:SERVER")
  
  (type application
    "Class representing an oAuth application in Radiance.

See DESCRIPTION
See AUTHOR
See NORTH:APPLICATION")
  
  (function description
    "Returns the description of the oAuth application provided by the user.

See APPLICATION")
  
  (function author
    "Returns the user ID of the user that created this application.

See APPLICATION
See USER:GET")
  
  (type session
    "Class representing an oAuth session in Radiance.

See USER
See EXPIRY
See NORTH:SESSION")
  
  (function user
    "Returns the user ID of the user that is associated with this session.

See SESSION
See USER:GET")
  
  (function expiry
    "Returns the universal-time on which this session is going to expire.

See SESSION")
  
  (variable *server*
    "Keeps Radiance's server instance.

See SERVER")

  (function make-application
    "Creates a new application object and returns it.

The name and description are purely for display to the user and should
inform them about the application's purpose.

See NORTH:MAKE-APPLICATION")

  (function application
    "Retrieves an application object given by its application key.

See NORTH:APPLICATION")

  (function revoke-application
    "Revokes (deletes) the given application and invalidates all sessions connected through it.

See NORTH:REVOKE-APPLICATION")
  
  (function prune-sessions
    "Delete expired sessions.

Any session with an expiry date lower than the current universal-time
will be removed and invalidated.

See SESSION
See EXPIRY")
  
  (function start-prune-thread
    "Starts a background thread to periodically prune expired sessions.

This signals an error if the thread is already running.
The thread will call PRUNE-SESSIONS every hour.

See PRUNE-SESSIONS
See STOP-PRUNE-THREAD")
  
  (function stop-prune-thread
    "Stops the background prune thread if it is still active.

See START-PRUNE-THREAD"))
