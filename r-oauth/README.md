## About r-oauth
This provides an oAuth 1.0a provider service for Radiance, allowing other applications to use a standardised authorisation protocol to access protected resources such as API endpoints.

## How To
When `r-oauth` is loaded, it will automatically provide the following standardised oAuth endpoints:

* `oauth/request-token`
* `oauth/authorize`
* `oauth/access-token`
* `oauth/verify`

oAuth clients that would like to use your services should be given the proper external URLs pointing to those API endpoints.

oAuth applications can be created through the `oauth/applications` page with the `admin` interface, or manually by using `make-application`. Applications can also be removed on the same admin page, or by using `revoke-application`.

Requests to protected resources that contain an `Authorization` header will be checked for oAuth validity. If the validity check fails, an error is signalled which will likely cause the request to fail. If the check succeeds, the associated oAuth session's user is associated and used for the given request.

## Configurables
The following config settings are available:

    :permissions :default  --- The permissions that are granted by default to new users.
                               Defaults to '("oauth.authorize" "oauth.application)
    :lifetime :unauth      --- The lifetime of unauthorised sessions in seconds.
                               Defaults to one minute.
    :lifetime :auth        --- The lifetime of authorised sessions in seconds.
                               Defaults to one year.

The following permissions are checked:

    oauth.authorize        --- Whether the user is allowed to authorize oAuth sessions.
    oauth.application      --- Whether the user is allowed to create oAuth applications.
